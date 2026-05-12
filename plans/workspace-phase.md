# Workspace Phase

This plan covers three tightly related changes:

1. **Queue workspace reconfiguration updates** so they are applied only after
   the request queue is fully drained, rather than immediately when
   `ApplyWorkspaceUpdate` fires.
2. **Assign `LspWorkspacePhase.Reconfiguring`** during the drain window so the
   phase is observable and meaningful.
3. **Issue server-side cancellation** to all in-flight requests — including
   `ReadOnlyBackground` — when a reconfiguration drain begins.
4. **Write an integration test** that exercises the full
   `Ready → Reconfiguring → Configured` transition against a new two-solution
   fixture.

---

## Background

`LspWorkspacePhase.Reconfiguring` is declared in the DU and documented in
`Workspace.fs` but is **never assigned** by any code path.  When
`ApplyWorkspaceUpdate` fans out a `FolderReconfiguration`, it immediately posts
`WorkspaceFolderConfigurationChanged` into the mailbox, which applies
`workspaceTeardown → workspaceFoldersReplaced` atomically before any in-flight
requests have a chance to finish.  The `Reconfiguring` phase and its drain
semantics exist only in comments.

The `WorkspaceReloadRequested` path (triggered by `.csproj`/`.sln` file
changes) does drain the queue first via a `PeriodicTimerTick` debounce, but
also never sets `Phase = Reconfiguring`.

`ReadOnlyBackground` requests (e.g. `workspace/diagnostic`) are excluded from
the drain predicate — they don't block drain completion — but are never
cancelled when a reconfiguration begins.  They can continue reading a workspace
that is about to be torn down.

---

## Changes

### 1  Add `PendingReconfiguration` DU and queue to `ServerState`

**File:** `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs`

Introduce a new discriminated union covering every kind of update that must be
deferred until the request queue is drained:

```fsharp
[<RequireQualifiedAccess>]
type PendingReconfiguration =
    | FolderReconfiguration of WorkspaceFolder list   // workspace/didChangeWorkspaceFolders
    | ConfigurationChange of CSharpConfiguration      // workspace/didChangeConfiguration
    | WorkspaceReload of deadline: DateTime           // .csproj / .sln file change; teardown not before deadline
```

(`WorkspaceFolder` is the LSP wire type from
`Ionide.LanguageServerProtocol.Types`, matching the payload of
`WorkspaceFolderConfigurationChanged` and `wsUpdate.FolderReconfiguration`.)

Add a list field to `ServerState`:

```fsharp
type ServerState =
    { ...
      PendingReconfigurations: PendingReconfiguration list   // new; applied in order at drain
      ... }
```

Default: `[]`.  Items are appended when their trigger fires and the queue has
not yet drained.  The list is consumed in `RequestQueueDrained` — each item is
applied in order to produce the final post-drain workspace state — then reset
to `[]`.

---

### 2  Add `workspaceSetPhaseReconfiguring` to `Workspace.fs`

**File:** `src/CSharpLanguageServer/Lsp/Workspace.fs`

A small pipeline function, consistent with `workspaceTeardown`,
`workspaceFoldersReplaced`, etc.:

```fsharp
let workspaceSetPhaseReconfiguring (workspace: LspWorkspace) =
    { workspace with Phase = LspWorkspacePhase.Reconfiguring }
```

---

### 3  Change event handlers to append to `PendingReconfigurations`

**File:** `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs`

Each trigger now appends to `state.PendingReconfigurations` and enters drain
mode, rather than applying the change immediately:

| Trigger | Appended case |
|---------|---------------|
| `ApplyWorkspaceUpdate` with `FolderReconfiguration = Some folders` | `PendingReconfiguration.FolderReconfiguration folders` |
| `ApplyWorkspaceUpdate` with `ConfigurationChange = Some cfg` | `PendingReconfiguration.ConfigurationChange cfg` |
| `WorkspaceReloadRequested` (`.csproj`/`.sln` change) | `PendingReconfiguration.WorkspaceReload deadline` |

For all three:
- Append the appropriate case to `state.PendingReconfigurations`.
- Call `enterDrainingMode` on the request queue (idempotent if already draining).
- Set `workspace.Phase = Reconfiguring`.
- Post `ProcessRequestQueue`.
- Do **not** post `WorkspaceFolderConfigurationChanged` from
  `ApplyWorkspaceUpdate` — that event is no longer needed and its handler is
  removed (see "Decisions" below).
- Continue to post `ConfigurationChange` for its side effects (culture switch,
  `WorkspaceDiagnosticRefresh`, updating `state.Config`).  Updating
  `state.Config` immediately is safe: in-flight requests already captured
  their `RequestContext.Config` at activation time, and the fold at drain
  picks up the updated `state.Config` for the workspace path-override stamp.

#### `WorkspaceReload` & debounce — minimal-disruption variant

The original plan suggested replacing `Workspace.ReloadPending` entirely with
the deadline carried inside `PendingReconfiguration.WorkspaceReload`.  That is
the eventual target shape, but it changes debounce semantics: a burst of
`.csproj` changes would all enter drain immediately on the first event, and
the fold would have to detect "deadline not yet reached" and re-queue itself.

**For this iteration we keep the existing debounce path unchanged:**

- `WorkspaceReloadRequested` still updates `Workspace.ReloadPending` as today
  (sliding-window deadline).
- `PeriodicTimerTick` still calls `enterDrainingMode` once the deadline has
  passed, AND now also appends a `PendingReconfiguration.WorkspaceReload`
  marker to `state.PendingReconfigurations` AND sets
  `Phase = Reconfiguring` at the same moment.
- `RequestQueueDrained` folds the list as below; the `WorkspaceReload` case
  unconditionally performs `workspaceTeardown` (deadline check already
  happened in the tick).

This keeps debounce identical to today, makes `Phase = Reconfiguring`
observable for the reload case too, and unifies the post-drain code path with
the folder-reconfiguration case.  Removing `Workspace.ReloadPending` in favour
of carrying the deadline inside the DU is deferred to a follow-up.

`ClientInitialize`, `ClientShutdown`, `TraceLevelChange`, and other
non-structural events in `ApplyWorkspaceUpdate` continue to be posted
immediately as before.

---

### 4  Apply deferred updates in `RequestQueueDrained`

**File:** `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs`

`RequestQueueDrained` folds over `state.PendingReconfigurations` in order,
applying each case to the workspace, then resets the list to `[]`:

```fsharp
| RequestQueueDrained ->
    let applyOne (ws: LspWorkspace) reconfig =
        match reconfig with
        | PendingReconfiguration.FolderReconfiguration folders ->
            ws
            |> workspaceTeardown
            |> workspaceFoldersReplaced folders
            |> workspaceSolutionPathOverride state.Config
        | PendingReconfiguration.ConfigurationChange _ ->
            // state.Config was already updated when the ConfigurationChange
            // event ran (the event is still posted immediately for side
            // effects).  Re-stamp the path override on the existing first
            // folder so a later, in-place config change without a folder
            // reconfiguration still takes effect on the next reload.
            workspaceSolutionPathOverride state.Config ws
        | PendingReconfiguration.WorkspaceReload _ ->
            workspaceTeardown ws   // deadline already vetted by PeriodicTimerTick

    let finalWorkspace =
        state.PendingReconfigurations
        |> List.fold applyOne state.Workspace

    postServerEvent ProcessRequestQueue
    return
        { state with
            Workspace = finalWorkspace
            PendingReconfigurations = []
            RequestQueue = state.RequestQueue |> enterDispatchingMode }
```

Final `Phase` after the fold:
- `FolderReconfiguration` → `Configured` (set by `workspaceFoldersReplaced`).
- `ConfigurationChange` alone (no folder reconfig in the list) → unchanged from
  whatever `workspaceSolutionPathOverride` leaves; in practice the caller will
  combine it with a folder reconfig in the same drain window, so this only
  matters as a fallback.
- `WorkspaceReload` → `ShuttingDown` (preserved from existing
  `workspaceTeardown` behaviour; subsequent reload puts it back to
  `Configured` via the next `ProcessSolutionAwaiters` / `LoadWorkspaceFolder`
  path).

If `PendingReconfigurations` is empty (e.g. drain raced with an external
caller), the fold is a no-op and the workspace is left unchanged.  This is a
behaviour change from today: the current `RequestQueueDrained` always tears
the workspace down.  All current callers of `enterDrainingMode` are the
reload path, which now also appends a `WorkspaceReload` marker, so the
teardown is preserved end-to-end.

---

### 5  Server-side cancellation of `BackgroundReadOnly` requests on drain

**File:** `src/CSharpLanguageServer/Runtime/RequestScheduling.fs`,
`src/CSharpLanguageServer/Lsp/Server.fs`

The JSON-RPC transport (`Runtime/JsonRpc.fs`) already creates a
`CancellationTokenSource` per inbound request and attaches it to the
`JsonRpcRequestContext`; that CTS is what the handler's
`Async.CancellationToken` resolves to, and `$/cancelRequest` cancels it via
the `CancelRequest` event.

Rather than introducing a second, independent CTS in `RequestInfo`, the
scheduler simply **holds a reference to the transport's CTS** so it can
cancel from drain-time events.

#### 5a — Plumb the transport CTS into `RequestInfo`

```fsharp
type RequestInfo =
    { ...
      Cts: CancellationTokenSource option   // borrowed from JsonRpcRequestContext
      ... }
```

`EnterRequestContext` gains a `CancellationTokenSource option` parameter:

```fsharp
| EnterRequestContext of
    int64 * string * RequestMode * CancellationTokenSource option *
    AsyncReplyChannel<RequestContext>
```

In `Lsp/Server.fs`, `wrapHandler` forwards `jsonRpcCtx.CancellationTokenSource`
into `EnterRequestContext`.  `registerRequest` stores it on `RequestInfo`.
Lifecycle: the CTS is owned and disposed by the JSON-RPC layer (`Handler*`
epilogue calls `Dispose`); the scheduler only borrows it for the duration of
the request and must not dispose it.

This means the existing handler cancellation path is unchanged: handlers
receive `Async.CancellationToken` from the same CTS as today; the scheduler
just now has a handle to it too.

#### 5b — `cancelBackgroundRequests`

```fsharp
/// Cancels the transport CTS of every running ReadOnlyBackground request.
let cancelBackgroundRequests (requestQueue: RequestQueue) : unit =
    requestQueue.Requests
    |> Map.iter (fun _ req ->
        if req.Mode = ReadOnlyBackground && req.Phase = Running then
            req.Cts |> Option.iter (fun cts ->
                try cts.Cancel() with _ -> ()))
```

(`try/with` because the CTS may already be cancelled or disposed if the
handler finished a moment earlier.)

#### 5c — Call site

In `ServerStateLoop.fs`, immediately after a successful `enterDrainingMode`
in the new `ApplyWorkspaceUpdate` reconfiguration branch and in the existing
`PeriodicTimerTick` reload branch, call:

```fsharp
do cancelBackgroundRequests state.RequestQueue
```

---

### 6  Create the `twoSolutions` test fixture

**Directory:** `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/`

Two minimal solutions, each with one library project and one deliberately
broken source file carrying a unique, easily-matched compile-error literal:

```
twoSolutions/
  SolutionA.sln               ← references ProjectA only
  ProjectA/
    ProjectA.csproj            ← net10.0 library
    SomeClass.cs               ← error: contains the literal "hello_A"
  SolutionB.sln               ← references ProjectB only
  ProjectB/
    ProjectB.csproj            ← net10.0 library
    SomeClass.cs               ← error: contains the literal "42_B"
```

The unique substrings (`hello_A`, `42_B`) let the test assert on
`Diagnostic.Message` content rather than just count.  The exact diagnostic
code (CS0029, CS0103, …) is incidental — the only requirement is that the
literal flows verbatim from the source into at least one diagnostic message.

Triggering reconfiguration in the test:

- The server starts with `solutionPathOverride = Some ".../SolutionA.sln"` in
  `ServerConfig`.  This is set via `activateFixtureExt` with a custom
  `clientProfile`.  The fixture is copied to a temp dir by the test harness,
  so the test patches the `solutionPathOverride` (and the
  `workspace/didChangeConfiguration` payload) to the temp-dir absolute path
  via a `patchSolutionDir` callback.
- To switch to SolutionB, the test sends `workspace/didChangeConfiguration`
  with `{ "settings": { "csharp": { "solution": "<tmp>/SolutionB.sln" } } }`
  to update `state.Config.solutionPathOverride`.
- Then it sends `workspace/didChangeWorkspaceFolders` with
  `{ event: { added: [], removed: [] } }` (no actual folder change, but
  enough to emit a `FolderReconfiguration` from
  `Workspace.didChangeWorkspaceFolders`).  With the new implementation this
  queues the reconfiguration, enters drain, and sets `Phase = Reconfiguring`.

---

### 7  Write `ReconfigurationTests.fs`

**New file:** `tests/CSharpLanguageServer.Tests/ReconfigurationTests.fs`

Single test: `testReadyToReconfiguringToConfiguredPhaseTransition`

```
1. activateFixtureExt "twoSolutions" with:
      debugMode = true
      solutionPathOverride = Some "<fixture>/SolutionA.sln"

2. waitUntilOrTimeout 15s (phase = "Ready")

3. getWorkspaceDiagnosticsForUri client projectAFileUri
   Assert: at least one diagnostic whose Message contains "hello_A"

4. client.Notify("workspace/didChangeConfiguration",
       { settings = { csharp = { solution = "<fixture>/SolutionB.sln" } } })

5. client.Notify("workspace/didChangeWorkspaceFolders",
       { event = { added = []; removed = [] } })

6. waitUntilOrTimeout 5s (GetDebugInfo().workspace.phase = "Reconfiguring")
   — verifies the phase is observable before drain completes

7. waitUntilOrTimeout 30s (GetDebugInfo().workspace.phase = "Ready")

8. getWorkspaceDiagnosticsForUri client projectBFileUri
   Assert: at least one diagnostic whose Message contains "42_B"

9. getWorkspaceDiagnosticsForUri client projectAFileUri
   Assert: empty  (SolutionA is no longer loaded)
```

Also add `ReconfigurationTests.fs` to the `<Compile>` list in
`tests/CSharpLanguageServer.Tests/CSharpLanguageServer.Tests.fsproj`.

---

## Decisions / clarifications captured during planning

These resolve ambiguities in the original draft that came up while reading
the existing code:

1. **Keep the `ConfigurationChange` event handler.** `ApplyWorkspaceUpdate`
   still posts `ConfigurationChange` for its side effects (culture switch,
   `WorkspaceDiagnosticRefresh`, `state.Config` update).  In addition,
   `ApplyWorkspaceUpdate` now also appends a
   `PendingReconfiguration.ConfigurationChange` so the workspace path override
   is re-stamped at drain time.
2. **Remove the `WorkspaceFolderConfigurationChanged` event handler.**  It is
   no longer posted by `ApplyWorkspaceUpdate`; its body lives inside the
   `RequestQueueDrained` fold (`FolderReconfiguration` case) instead.  The
   `ServerEvent.WorkspaceFolderConfigurationChanged` case can be deleted along
   with it.
3. **Preserve the existing reload debounce.** `Workspace.ReloadPending` is
   kept; `PeriodicTimerTick` is the place that decides "deadline reached →
   start draining → append `WorkspaceReload` marker → set
   `Phase = Reconfiguring`".  Carrying the deadline inside the DU and removing
   `ReloadPending` is a follow-up.
4. **Borrow the transport CTS for `RequestInfo.Cts`** (don't create a new
   one).  Simpler lifecycle, no need to link tokens; handlers keep receiving
   their existing `Async.CancellationToken`, the scheduler just gains a handle
   for drain-time cancellation.
5. **`enterDrainingMode` call sites that need `cancelBackgroundRequests`:**
   the new reconfiguration branch (in the `ApplyWorkspaceUpdate` rewrite) and
   the existing reload branch in `PeriodicTimerTick`.  The `shutdown` /
   `exit` paths already tear everything down independently.

## Out of scope (deferred follow-ups)

- Replacing `Workspace.ReloadPending` with the deadline carried inside
  `PendingReconfiguration.WorkspaceReload`, and dropping the
  `PeriodicTimerTick` gating in favour of "drain immediately, fold checks
  deadline, re-queue if too early".
- Auditing every handler to actually observe the new scheduler-driven
  cancellation (i.e. `do! Async.SwitchToThreadPool`-style yield points).
  For now only `workspace/diagnostic` matters and it already reads
  `Async.CancellationToken`.

## File Summary

| Action | File |
|--------|------|
| Modify | `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs` |
| Modify | `src/CSharpLanguageServer/Lsp/Workspace.fs` |
| Modify | `src/CSharpLanguageServer/Runtime/RequestScheduling.fs` |
| Modify | `tests/CSharpLanguageServer.Tests/CSharpLanguageServer.Tests.fsproj` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/SolutionA.sln` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/ProjectA/ProjectA.csproj` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/ProjectA/SomeClass.cs` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/SolutionB.sln` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/ProjectB/ProjectB.csproj` |
| Create | `tests/CSharpLanguageServer.Tests/Fixtures/twoSolutions/ProjectB/SomeClass.cs` |
| Create | `tests/CSharpLanguageServer.Tests/ReconfigurationTests.fs` |
