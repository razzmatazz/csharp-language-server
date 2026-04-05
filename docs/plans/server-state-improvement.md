# Server State Improvement Plan

This document analyses the workspace reload logic in the server state loop,
catalogues known race conditions, and proposes improvements.

## Background

The server uses a single-threaded `MailboxProcessor` state loop
(`ServerStateLoop.fs`) to serialise all state mutations.  Handlers run
concurrently but **buffer their effects** in `RequestEffects`; these are only
replayed into the state loop when a request is **retired** (in ordinal order).
This design is intended to prevent races, but several gaps remain.

---

## 1  Workspace Reload Flow (current)

### Trigger

`didChangeWatchedFiles` (in `Handlers/Workspace.fs`) watches
`**/*.{cs,csproj,sln,slnx}`.  When a `.csproj`, `.sln`, or `.slnx` file
changes, the handler emits:

```fsharp
context.UpdateEffects(_.WithWorkspaceReloadRequested(TimeSpan.FromSeconds(5)))
```

### Effect propagation

Effects are buffered on the `RequestContext` and replayed as `ServerEvent`s
when the request retires.  `requestEffectsToServerEvents` (in `Lsp/Server.fs`)
converts each `TimeSpan` entry in `WorkspaceReloadRequested` into a
`WorkspaceReloadRequested` event.

### Debouncing

In `ServerStateLoop.fs`, `WorkspaceReloadRequested` sets a deadline:

```fsharp
let suggestedDeadline = DateTime.Now + reloadNoLaterThanIn   // +5 s
match state.WorkspaceReloadPending with
| Some currentDeadline ->
    if suggestedDeadline < currentDeadline then suggestedDeadline
    else currentDeadline                          // keep the earlier deadline
| None -> suggestedDeadline
```

Multiple rapid events coalesce — the **earliest** deadline wins.

### Tick-based check

`PeriodicTimerTick` fires every 250 ms.  When the deadline has passed it calls
`enterDrainingMode`, which freezes the request queue.

### Draining → teardown → re-init

Once all non-background requests up to the drain ordinal have retired,
`processRequestQueue` returns `Drained`.  The state loop processes
`RequestQueueDrained`:

```fsharp
| RequestQueueDrained ->
    let tornDownWorkspace = workspaceTeardown state.Workspace
    // TODO: reset awaiters
    postServerEvent ProcessRequestQueue
    return { state with
               Workspace = tornDownWorkspace
               WorkspaceReloadPending = None
               RequestQueue = state.RequestQueue |> enterDispatchingMode }
```

`workspaceTeardown` cancels any `Loading` CTS, disposes any `Ready` workspace,
and resets every folder's `Solution` to `Uninitialized`.  The next request that
calls `GetWorkspaceFolderReadySolution` triggers `ProcessSolutionAwaiters`,
which kicks off `workspaceFolderWithSolutionInitialized` for each
`Uninitialized` folder.

---

## 2  Debounce Semantics

The current debounce uses **earliest-deadline-wins**:

- `.csproj` change at T=0 → deadline T+5 s
- `.sln` change at T=3 s → deadline stays at T+5 s (earlier wins)
- `.sln` change at T=6 s → triggers a *second* reload cycle

This is a **fixed window from the first change**, not a sliding/resetting
debounce.  A sliding window (each new event resets the deadline to
`now + delay`) would better absorb bursts that span more than 5 s (e.g.
`dotnet add package` touching `.csproj`, `obj/`, and `.sln` in sequence).

---

## 3  Catalogued Race Conditions

### Race 1 — Stale `WorkspaceFolderSolutionChange` from a cancelled load

**Severity: high — can corrupt workspace state.**

1. Solution load #1 starts.  `wf.Solution = Loading(async1, cts1)`.
2. Reload requested → queue drains → `RequestQueueDrained` → `workspaceTeardown`
   cancels `cts1`, sets `wf.Solution = Uninitialized`.
3. Solution load #2 starts via `ProcessSolutionAwaiters`.
4. `task.ContinueWith` from load #1 fires (on threadpool).  It observes
   `t.IsCanceled` and posts
   `WorkspaceFolderSolutionChange(uri, Defunct("…cancelled…"))`.
5. The state loop handler **blindly replaces** `wf.Solution`:

   ```fsharp
   | WorkspaceFolderSolutionChange(uri, newSolution) ->
       let updatedWf = { wf with Solution = newSolution }
   ```

   Load #2's `Loading` (or even `Ready`) is overwritten with a stale `Defunct`.

There is also a variant where load #1's task was already past the cancellation
point, so `ContinueWith` delivers a stale `Ready(…)` that overwrites load #2.

### Race 2 — TOCTOU for concurrent `ReadOnly` handlers emitting `WorkspaceFolderChange`

**Severity: medium — causes silent data loss (lost solution mutations).**

Pattern: a handler calls `GetWorkspaceFolderReadySolution` (reads a snapshot of
`wf`), does async work, then emits `WithWorkspaceFolderChange(updatedWf)`.
Multiple concurrent `ReadOnly` requests read the **same** snapshot.  When
retired in ordinal order, each `WorkspaceFolderChange` does a full replacement
via `workspaceWithFolderUpdated` — the second one overwrites the first one's
mutations (last-writer-wins).

Concrete scenario: two concurrent `textDocument/definition` requests each
decompile a different metadata symbol, adding to `DecompiledSymbolMetadata`.
Both start from the same snapshot; only the second one's map survives.

Affected handlers (all emit `WithWorkspaceFolderChange` from `ReadOnly` mode):

| Handler | File |
|---------|------|
| `definition` | `Definition.fs` |
| `implementation` | `Implementation.fs` |
| `typeDefinition` | `TypeDefinition.fs` |
| `prepareTypeHierarchy`, `supertypes`, `subtypes` | `TypeHierarchy.fs` |
| `prepareCallHierarchy` | `CallHierarchy.fs` |
| `csharp/metadata` | `CSharpMetadata.fs` |

### Race 3 — TOCTOU within `didChangeWatchedFiles` loop

**Severity: medium — second file change overwrites the first.**

The handler loops over `p.Changes`.  For each `.cs` change it calls
`GetWorkspaceFolderReadySolution` (same snapshot every time, because effects
are buffered), mutates it, and emits `WithWorkspaceFolderChange`.  Each
iteration's `updatedWf` is derived from the **original** snapshot, so only the
last iteration's changes survive.

### Race 4 — `ReadWrite` handlers vs. async `WorkspaceFolderSolutionChange`

**Severity: low–medium.**

Even `ReadWrite` handlers (`didOpen`, `didChange`, `didClose`) are vulnerable:
between the `GetWorkspaceFolderReadySolution` call and effect replay, the state
loop can process non-request events (e.g. `WorkspaceFolderSolutionChange` from
an async completion callback).  The handler's buffered `WorkspaceFolderChange`
then replays over a state that has moved on.

### Race 5 — Dangling `SolutionReadyAwaiters` after teardown

**Severity: medium — can hang requests indefinitely.**

`RequestQueueDrained` and `WorkspaceConfigurationChanged` both tear down the
workspace but **do not** clear or reply to `SolutionReadyAwaiters`.  Both have
a TODO comment acknowledging this.  Handlers blocked on
`GetWorkspaceFolderReadySolution` (which uses `PostAndAsyncReply`) will hang
until the reloaded solution happens to satisfy them — or forever if the
workspace folder URI changed.

### Race 6 — `ProcessSolutionAwaiters` iterates stale state

**Severity: low.**

The `for wf in wfsWithUninitializedSolution` loop looks up folders from
`state.Workspace` (original), while updates go into a mutable `newState`.  This
is fragile but not currently broken because the loop updates `newState` before
the second phase reads it.  However, if a `WorkspaceFolderSolutionChange`
event arrives between the two phases (it can't, because the mailbox is
single-threaded), the state would be inconsistent.  The real risk is that
future refactors could break the subtle ordering dependency.

### Race 7 — Stale diagnostics after reload

**Severity: low — cosmetic only.**

`PushDiagnostics` kicks off `Task.Run(resolveDocumentDiagnostics)` with a
`Document` from the current solution snapshot.  If the solution is reloaded
while the task runs, the published diagnostics are stale.  Not crash-inducing
but can confuse users briefly.

---

## 4  Proposed Fix: Generation Counter

Add a generation counter to `LspWorkspaceFolder`:

```fsharp
type LspWorkspaceFolder =
    { ...
      Generation: int64
      ... }
```

### How it fixes Race 1

- `workspaceTeardown` bumps `Generation` when resetting to `Uninitialized`.
- `workspaceFolderWithSolutionInitialized` captures the current `Generation`
  and passes it into the completion callback.
- `WorkspaceFolderSolutionChange` becomes
  `WorkspaceFolderSolutionChange(uri, generation, newSolution)`.
- The state loop handler checks: if `wf.Generation <> generation`, the event
  is **stale** → discard silently.

### How it helps with Race 2 / 3 / 4

The `WorkspaceFolderChange` event could similarly carry the generation that the
handler read when it obtained its snapshot.  The state loop would reject
changes whose generation doesn't match.  However, this means the handler's
mutations are silently lost — which may be acceptable for metadata decompilation
(Race 2) but not for document text updates (Race 4).

For document text updates, a better approach may be to move the solution
mutation **into the state loop** rather than having the handler construct an
`updatedWf`.  The handler would emit a lightweight "intent" event (e.g.
`DocumentTextChanged(uri, newSourceText)`) and the state loop would apply it
to the **current** `wf`, eliminating the stale-snapshot problem entirely.

---

## 5  Proposed Fix: Clear Awaiters on Teardown (Race 5)

When `RequestQueueDrained` or `WorkspaceConfigurationChanged` tears down the
workspace, reply to all `SolutionReadyAwaiters` with `None`:

```fsharp
for (_, rc) in state.SolutionReadyAwaiters do
    rc.Reply(None)

return { state with SolutionReadyAwaiters = [] }
```

Handlers receiving `None` already handle the "no workspace folder" case
gracefully (they return an empty result).

---

## 6  Proposed Fix: Sliding Debounce

Change the debounce from "earliest deadline wins" to "sliding window":

```fsharp
| WorkspaceReloadRequested reloadNoLaterThanIn ->
    let newDeadline = DateTime.Now + reloadNoLaterThanIn
    return { state with WorkspaceReloadPending = Some newDeadline }
```

Each new change pushes the deadline forward, so a burst of changes
(e.g. `dotnet add package` touching multiple files over several seconds)
settles before reload begins.

---

## 7  Proposed Simplification: `WorkspaceConfigurationChanged` and Double Teardown

`didChangeWorkspaceFolders` currently emits **both**
`WorkspaceConfigurationChanged` (which tears down immediately and rebuilds
the folder list) and `WorkspaceReloadRequested` (which starts a second
drain-then-teardown cycle).  This is a double teardown.

Options:

- **Option A:** `WorkspaceConfigurationChanged` only reconfigures the folder
  list (no teardown).  The subsequent `WorkspaceReloadRequested` handles
  teardown through the normal drain path.
- **Option B:** `WorkspaceConfigurationChanged` does teardown as now, but
  `didChangeWorkspaceFolders` stops emitting `WorkspaceReloadRequested`.

Option A is cleaner because it keeps teardown in one place (the drain path).

---

## 8  Proposed Simplification: `WorkspaceFolderSolutionChange` vs `WorkspaceFolderChange`

`WorkspaceFolderSolutionChange(uri, solution)` is a special case of
`WorkspaceFolderChange(wf)` — it updates only `wf.Solution`.  They could be
merged, but keeping them separate makes the generation-check guard (§4) cleaner
since the async solution callback is the primary source of stale events.

No change recommended here unless the generation counter makes the distinction
unnecessary.

---

## 9  Summary

| Issue | Severity | Fix |
|-------|----------|-----|
| Stale `WorkspaceFolderSolutionChange` from cancelled load | High | Generation counter on `LspWorkspaceFolder` |
| TOCTOU for concurrent `ReadOnly` `WorkspaceFolderChange` | Medium | Generation check, or move mutation into state loop |
| TOCTOU within `didChangeWatchedFiles` loop | Medium | Accumulate changes in loop, emit single effect |
| `ReadWrite` handlers vs async completion events | Low–Med | Generation check, or intent-based events |
| Dangling `SolutionReadyAwaiters` | Medium | Reply `None` on teardown |
| Earliest-deadline debounce | Low | Switch to sliding window |
| Double teardown from `didChangeWorkspaceFolders` | Low | Unify teardown path |
