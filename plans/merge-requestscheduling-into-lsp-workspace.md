# RFC: Workspace Pending-Operation Queue and Request Queue Coupling

This document analyses a concrete bug in `WorkspaceFolderConfigurationChanged`,
the proposed `solutionPathOverride`-at-runtime feature, and how both motivate a
queue of pending workspace operations on `ServerState`.  It also answers the
original question of whether `Runtime/RequestScheduling.fs` should be merged
into `Lsp/Workspace.fs`.

---

## 1  The Concrete Bug: Premature `workspaceTeardown`

`ServerStateLoop.fs` handles `WorkspaceFolderConfigurationChanged` today as
follows:

```fsharp
| WorkspaceFolderConfigurationChanged workspaceFolders ->
    for (_, rc) in state.SolutionReadyAwaiters do
        rc.Reply(None)

    let _ = workspaceTeardown state.Workspace   // ← BUG: tears down immediately

    let newWorkspace =
        workspaceFrom workspaceFolders |> workspaceWithSolutionPathOverride state.Config

    return
        { state with
            Workspace = newWorkspace
            SolutionReadyAwaiters = [] }
```

This event is posted by `ApplyWorkspaceUpdate` when it processes a retired
request's `LspWorkspaceUpdate.FolderReconfiguration`.  The problem:

1. **The request queue is not drained first.**  Other in-flight `ReadOnly`
   handlers may still be running — they hold a snapshot of the old workspace
   and will attempt to retire with `WorkspaceFolderChange` events or call back
   via `WorkspaceFolderSolutionChange`.  Tearing down the workspace out from
   under them corrupts state.

2. **The correct path already exists.**  The `WorkspaceReloadRequested` →
   `PeriodicTimerTick` → `enterDrainingMode` → `RequestQueueDrained` →
   `workspaceTeardown` flow handles teardown safely, after all non-background
   requests have retired.  Folder reconfiguration should go through the same
   path.

The fix is to treat a folder reconfiguration as a *pending operation* that is
enqueued and applied only after the queue drains.

---

## 2  The Proposed Feature: Runtime `solutionPathOverride`

`csharp.solutionPathOverride` is currently a CLI-only flag (`--solution`),
stamped into `CSharpConfiguration.solutionPathOverride` at startup and applied
via `workspaceWithSolutionPathOverride` when the workspace is first built.
`mergeCSharpConfiguration` preserves it from the old config — the client cannot
push a new value today.

The proposed feature would allow the client to change `solutionPathOverride` at
runtime via `workspace/didChangeConfiguration`.  When the value changes:

1. The existing workspace must be **torn down** (the old solution file is no
   longer valid).
2. The workspace must be **rebuilt** with the **same folder list** but the
   **new config overlay** applied (`workspaceWithSolutionPathOverride newConfig`).
3. Solution loading restarts from `Uninitialized`.

This is the same drain → teardown → rebuild pattern, but with a different
post-drain action: keep the folders, swap the config overlay.

---

## 3  Three Pending-Operation Cases

All three currently-known workspace-disrupting operations share the same
lifecycle:

| Operation | Trigger | Post-drain workspace shape |
|---|---|---|
| **Reload** | `didChangeWatchedFiles` (`.csproj`/`.sln`/`.slnx` change) | Keep folders; all folders reset to `Uninitialized` |
| **Folder replacement** | `didChangeWorkspaceFolders` | Replace folder list; all folders reset to `Uninitialized` |
| **Solution path change** | `didChangeConfiguration` (proposed) | Keep folders; re-apply config overlay; all folders reset to `Uninitialized` |

Today:
- Reload is handled correctly (via `PeriodicTimerTick` → `enterDrainingMode`),
  but its deadline is stored on `LspWorkspace.ReloadPending` — the wrong layer.
- Folder replacement fires `workspaceTeardown` immediately — a race bug.
- Solution path change has no runtime support yet.

---

## 4  Why Multiple Drain Cycles Are Not Needed

A naive queue model processes one operation per drain cycle — pop the head,
apply it, if more remain immediately re-enter draining before dispatching
anything.  This is correct but unnecessarily complex.

The key observation: **no handlers run while the queue is draining**.  The
intermediate workspace states between queued operations are never observable.
There is no reason to teardown → rebuild → drain → teardown → rebuild N times
when we can teardown once, fold all accumulated operations into a single final
workspace shape, and enter dispatching mode — all in the `RequestQueueDrained`
handler.

Operations accumulate freely in `PendingOperations` while the queue is
draining (or before it starts draining).  When `RequestQueueDrained` fires, the
entire list is folded left-to-right into one post-drain workspace.  Because the
fold applies operations in arrival order, later operations naturally override
earlier ones where they conflict — no explicit precedence table is needed.

---

## 5  Data Model

```fsharp
/// A workspace-disrupting operation to be applied after the request queue drains.
type WorkspacePendingOperation =
    | PendingReload            of reloadNoLaterThan: DateTime
    | PendingFolderReplacement of newFolders: WorkspaceFolder list
    | PendingSolutionPathChange of newConfig: CSharpConfiguration
```

`ServerState` gains one field:

```fsharp
PendingOperations: WorkspacePendingOperation list
```

`LspWorkspace.ReloadPending` (`DateTime option`) is **removed** — it moves here.

Operations are appended unconditionally with one exception: consecutive
`PendingReload` entries at the tail of the list are merged by taking the
**later** deadline (extend the debounce window):

```fsharp
let enqueueOperation (op: WorkspacePendingOperation) (ops: WorkspacePendingOperation list) =
    match List.tryLast ops, op with
    | Some (PendingReload t1), PendingReload t2 ->
        ops[.. ops.Length - 2] @ [ PendingReload(max t1 t2) ]
    | _ ->
        ops @ [ op ]
```

No other normalisation is needed.  If a `PendingFolderReplacement` is appended
after a `PendingReload`, both are kept: the fold will produce the folder
replacement as the final result, with the intermediate reload simply being a
no-op on workspace shape — correct by construction.

---

## 6  The Fold: Accumulate Into a Single Work Unit

The post-drain action is a single left-fold over `state.PendingOperations`:

```fsharp
let applyPendingOperation (state: ServerState) (ws: LspWorkspace) (op: WorkspacePendingOperation) =
    match op with
    | PendingReload _ ->
        ws   // workspace shape unchanged; folders already Uninitialized from teardown

    | PendingFolderReplacement newFolders ->
        // replace folder list; always re-apply current config overlay
        workspaceFrom newFolders |> workspaceWithSolutionPathOverride state.Config

    | PendingSolutionPathChange _ ->
        // keep existing folders; re-apply updated config (already in state.Config)
        workspaceWithSolutionPathOverride state.Config ws
```

Properties of the fold:
- `[PendingReload; PendingReload]` → same as one reload (merge handled by
  `enqueueOperation`; fold is a no-op on workspace shape either way).
- `[PendingReload; PendingFolderReplacement f]` → folder replacement wins.
- `[PendingFolderReplacement f; PendingReload]` → keeps `f`, reload is no-op.
- `[PendingFolderReplacement f; PendingSolutionPathChange cfg]` → folder
  replacement, then config overlay applied on top.
- `[PendingSolutionPathChange cfg1; PendingSolutionPathChange cfg2]` → second
  config wins (applied last); `state.Config` already holds `cfg2`.
- `[PendingSolutionPathChange; PendingFolderReplacement f]` → folder
  replacement applied last; `workspaceFrom f |> withSolutionPathOverride
  state.Config` uses the already-updated `state.Config`.

---

## 7  How the Fixed Flow Works

### 7.1  Folder reconfiguration (`didChangeWorkspaceFolders`)

**Before (buggy):**
```
didChangeWorkspaceFolders
  → WithFolderReconfiguration(newFolders) in LspWorkspaceUpdate
  → ApplyWorkspaceUpdate posts WorkspaceFolderConfigurationChanged
  → WorkspaceFolderConfigurationChanged tears down workspace immediately ← RACE
```

**After:**
```
didChangeWorkspaceFolders
  → WithFolderReconfiguration(newFolders) in LspWorkspaceUpdate
  → ApplyWorkspaceUpdate: enqueue PendingFolderReplacement(newFolders)
  → PeriodicTimerTick: enterDrainingMode (if not already draining)
  → RequestQueueDrained: fold [… PendingFolderReplacement(newFolders)]
      → workspaceTeardown; workspaceFrom newFolders; enterDispatchingMode
```

If further operations arrived during the drain, they are all in
`PendingOperations` by the time `RequestQueueDrained` fires and are folded into
the same single rebuild.

### 7.2  Workspace reload (`didChangeWatchedFiles`)

```fsharp
| WorkspaceReloadRequested quietPeriod ->
    let newDeadline = DateTime.UtcNow + quietPeriod
    return
        { state with
            PendingOperations =
                state.PendingOperations
                |> enqueueOperation (PendingReload newDeadline) }
```

### 7.3  Solution path change (`didChangeConfiguration`, proposed)

```fsharp
| ConfigurationChange newConfig ->
    // ... existing locale / analyzersEnabled logic ...
    let solutionPathChanged =
        newConfig.solutionPathOverride <> state.Config.solutionPathOverride

    let newOps =
        if not solutionPathChanged then state.PendingOperations
        else state.PendingOperations |> enqueueOperation (PendingSolutionPathChange newConfig)

    return { state with Config = newConfig; PendingOperations = newOps }
```

### 7.4  `PeriodicTimerTick` — drain trigger

```fsharp
| PeriodicTimerTick ->
    // ... push diagnostics, stats dump ...
    let shouldDrain =
        state.PendingOperations
        |> List.exists (fun op ->
            match op with
            | PendingFolderReplacement _
            | PendingSolutionPathChange _ -> true
            | PendingReload deadline      -> deadline < DateTime.UtcNow)

    match shouldDrain with
    | false -> return state
    | true ->
        match enterDrainingMode state.RequestQueue with
        | Some rq ->
            postServerEvent ProcessRequestQueue
            return { state with RequestQueue = rq }
        | None -> return state   // already draining
```

### 7.5  `RequestQueueDrained` — fold all operations, single teardown

```fsharp
| RequestQueueDrained ->
    for (_, rc) in state.SolutionReadyAwaiters do
        rc.Reply(None)

    let tornDownWorkspace = workspaceTeardown state.Workspace

    let newWorkspace =
        state.PendingOperations
        |> List.fold (applyPendingOperation state) tornDownWorkspace

    postServerEvent ProcessRequestQueue

    return
        { state with
            Workspace          = newWorkspace
            SolutionReadyAwaiters = []
            PendingOperations  = []
            RequestQueue       = state.RequestQueue |> enterDispatchingMode }
```

If `PendingOperations` was empty (drain triggered by something external),
`tornDownWorkspace` passes through the fold unchanged — the workspace is simply
reset to `Uninitialized` and re-initialised via `ProcessSolutionAwaiters` as
today.

---

## 8  Where Should the New Types Live?

### Option A — `ServerStateLoop.fs` (recommended)

Define `WorkspacePendingOperation`, `enqueueOperation`, and
`applyPendingOperation` as local types/functions alongside `ServerState`.

- ✅ No structural changes — no new files, no project edits.
- ✅ All three pieces are only meaningful to the state loop; locality is honest.
- ✅ `Lsp/Workspace.fs` loses `ReloadPending` and stays a pure data-model layer.

### Option B — New `Runtime/WorkspacePendingOperation.fs`

Extract if the operation types or fold logic grow (e.g. progress-reporting
labels per operation, per-operation debounce durations).

### Option C — Merge `RequestScheduling.fs` into `Lsp/Workspace.fs`

**Verdict: do not merge.**  The coupling between `RequestQueue.Mode` and
`PendingOperations` is resolved inside `ServerStateLoop`, where both already
live.  Neither `Lsp/Workspace.fs` nor `Runtime/RequestScheduling.fs` need to
know about pending operations.  Merging would:
- Pull `ILspClient`, `ILogger`, `AsyncReplyChannel`, and Roslyn into the pure
  workspace data layer.
- Invert the clean `Runtime → Lsp` dependency direction.
- Produce a ~400-line file mixing an immutable data model with a
  concurrency-policy algorithm.

---

## 9  Migration Steps

1. **Add `WorkspacePendingOperation` DU**, `enqueueOperation`, and
   `applyPendingOperation` to `Runtime/ServerStateLoop.fs`.

2. **Add `PendingOperations: WorkspacePendingOperation list` to `ServerState`**,
   defaulting to `[]`.

3. **Remove `ReloadPending` from `LspWorkspace`** (`Lsp/Workspace.fs`).
   Update `PeriodicTimerTick` to read from `state.PendingOperations`.

4. **Delete `WorkspaceFolderConfigurationChanged`** from `ServerEvent` and its
   handler — replaced by `enqueueOperation` inside `ApplyWorkspaceUpdate`.

5. **Update `ApplyWorkspaceUpdate`**: when `wsUpdate.FolderReconfiguration` is
   `Some newFolders`, call `enqueueOperation (PendingFolderReplacement newFolders)`.

6. **Update `WorkspaceReloadRequested`** handler per §7.2.

7. **Update `ConfigurationChange`** handler per §7.3.

8. **Update `PeriodicTimerTick`** per §7.4.

9. **Update `RequestQueueDrained`** per §7.5.

10. **Allow `solutionPathOverride` to be pushed by `didChangeConfiguration`**:
    update `mergeCSharpConfiguration` to overwrite (not `orElse`) the
    `solutionPathOverride` field.  Consider a distinct `csharp.solutionPath`
    setting name to avoid ambiguity with the CLI flag.

11. **Add regression tests** covering:
    - Multiple operations enqueued during one drain — all folded into a single
      rebuild; workspace torn down exactly once.
    - `[PendingReload, PendingFolderReplacement]` — folder replacement wins.
    - `[PendingFolderReplacement, PendingReload]` — reload is a no-op on shape.
    - `[PendingSolutionPathChange cfg1, PendingSolutionPathChange cfg2]` — only
      cfg2 survives.
    - Drain with in-flight `ReadOnly` handlers — workspace not accessed after
      teardown; stale `WorkspaceFolderSolutionChange` discarded via generation.
    - `solutionPathOverride` pushed via `didChangeConfiguration` triggers
      teardown and re-init with new solution file.
    - Empty `PendingOperations` at drain time — workspace reset and re-inited
      without error.

---

## 10  Summary

| Issue | Status | Fix |
|-------|--------|-----|
| `workspaceTeardown` in `WorkspaceFolderConfigurationChanged` fires before drain | 🐛 Bug | Enqueue `PendingFolderReplacement`; fold and apply in `RequestQueueDrained` |
| Multiple disrupting ops during a drain are dropped or cause extra cycles | 🐛 Bug | Accumulate in `PendingOperations`; fold all into a single rebuild at drain time |
| `LspWorkspace.ReloadPending` mixes scheduling into data model | 🔧 Design smell | Move to `ServerState.PendingOperations` |
| Reload, folder replacement, solution path change handled inconsistently | 🔧 Design smell | Unify under `WorkspacePendingOperation` list; single fold |
| Runtime `solutionPathOverride` change has no safe implementation path | 🚧 Proposed feature | `PendingSolutionPathChange` case covers it naturally |
| Merge `RequestScheduling.fs` into `Lsp/Workspace.fs` | ❌ Not recommended | Coupling resolved in `ServerStateLoop`; no changes needed in either file |
