# Workspace Phase — Design Plan

## 1. Motivation

`LspWorkspacePhase` is intended to be the single source of truth for what the
server is doing at the workspace level.  Currently it is partially implemented:
phases exist, transitions happen, but the relationship between phase transitions
and the request queue is **implicit and scattered** across `ServerStateLoop.fs`,
`Handlers/LifeCycle.fs`, and `Lsp/Workspace.fs`.

The most concrete use-case that requires this to be correct is **runtime
reconfiguration** — e.g. a client (or user) sending a new `solutionPathOverride`
value while the server is already `Ready`.  Today there is no clean path for
that: the pending-operation machinery exists but phase semantics around the drain
→ shutdown → reload cycle are not consistently enforced.

---

## 2. Current State

### 2.1 Phase DU

```fsharp
type LspWorkspacePhase =
    | Uninitialized           // before initialize, or after full shutdown
    | Configured              // initialize received; no load started yet
    | Loading of CancellationTokenSource   // Roslyn load in flight
    | Ready                   // solution loaded; normal operation
    | ShuttingDown            // drain-before-reload or client shutdown
```

### 2.2 Transition map (as-implemented)

```
                ┌──────────────────────────────────────────────────────┐
                │                                                      │
   startup ─► Uninitialized ──ClientInitialize──► Configured           │
                                                      │                │
                                                      │ (lazy, first   │
                                                      │  GetWorkspace  │
                                                      │  withReady)    │
                                                      ▼                │
                                                  Loading cts          │
                                                      │                │
                                              WorkspaceLoadCompleted   │
                                                      │                │
                                                      ▼                │
                                                   Ready               │
                                                      │                │
                              PendingOperation ───────┤                │
                              or ClientShutdown       │                │
                                                      ▼                │
                                               ShuttingDown            │
                                                      │                │
                                           RequestQueueDrained         │
                                                      │                │
                                                      └────────────────┘
                                               (back to Uninitialized,
                                                or if client shutdown: stays)
```

### 2.3 Problems

| # | Problem | Where |
|---|---------|-------|
| P1 | `ShuttingDown` is used both for "drain-before-reload" and for "client called shutdown". They need different post-drain behaviour (reload vs. stop). | `ServerStateLoop.fs` |
| P2 | Phase transitions are set in two places: inside handler return values (`LspWorkspaceUpdate.PhaseTransition`) **and** directly in `ServerStateLoop.fs` event handlers. It is not obvious which takes precedence or when each path is used. | `Handlers/LifeCycle.fs`, `ServerStateLoop.fs` |
| P3 | The `Loading` case carries a `CancellationTokenSource` — mutable, side-effectful state embedded inside a discriminated union that is otherwise meant to be a pure label. | `Lsp/Workspace.fs` |
| P4 | There is no phase that represents "queue is draining because a reconfiguration is pending". `ShuttingDown` is overloaded for this purpose. | everywhere |
| P5 | Handlers can request a phase transition to `ShuttingDown` via `LspWorkspaceUpdate`, but there is no guard preventing a handler from also requesting `Configured` or `Ready` directly. | `ServerStateLoop.fs:ApplyWorkspaceUpdate` |
| P6 | `solutionPathOverride` change via LSP config notification (`workspace/didChangeConfiguration`) today triggers `PendingSolutionPathChange`, but there is no test or documentation of the expected phase sequence. | `Handlers/Workspace.fs`, `ServerStateLoop.fs` |
| P7 | In `DrainIfPendingOperationsReady`, the phase is set to `ShuttingDown` **unconditionally** — even when `shouldDrain` is `false` and no drain actually starts.  This means a stale `PendingReload` whose deadline hasn't arrived yet will still clobber the phase to `ShuttingDown`. | `ServerStateLoop.fs:441–445` |
| P8 | `isDrained` in `RequestScheduling.fs` excludes `ReadOnlyBackground` requests from the drain check.  This means background requests (e.g. `workspace/diagnostics`) can still be running against a workspace that is about to be torn down, risking stale/corrupt reads. | `Runtime/RequestScheduling.fs:isDrained` |
| P9 | `enterDrainingMode` does not cancel running requests — it just stops activating new ones and waits for in-flight requests to finish naturally.  This makes drains slow: a long-running handler (e.g. a `workspace/diagnostics` full scan, or a slow `textDocument/completion`) will block the entire reconfiguration until it completes on its own. | `Runtime/RequestScheduling.fs:enterDrainingMode` |
| P10 | Per-request `CancellationTokenSource` exists at the `JsonRpc.fs` layer (`JsonRpcRequestContext.CancellationTokenSource`, stored in `RunningInboundRequests`) but the scheduling layer (`RequestInfo`, `RequestQueue`) has zero awareness of it.  The two layers are separate `MailboxProcessor` actors with no cross-reference.  There is no mechanism for the state loop to tell the JsonRpc transport "cancel all running requests". | `Runtime/JsonRpc.fs`, `Runtime/RequestScheduling.fs` |

---

## 3. Desired Phase Model

### 3.1 Revised Phase DU

Split the overloaded `ShuttingDown` into two distinct phases and remove the
mutable CTS from the DU:

```fsharp
[<RequireQualifiedAccess>]
type LspWorkspacePhase =
    | Uninitialized                 // before initialize, or after full teardown
    | Configured                    // initialize ACK'd; load not yet started
    | Loading                       // Roslyn async load in flight
    | Ready                         // solution loaded; serving requests normally
    | Reconfiguring                 // drain → rebuild due to config change
    | ShuttingDown                  // drain → stop because client called shutdown
```

The `CancellationTokenSource` for an in-flight load moves to `ServerState`
(already is, effectively — it lives inside `LspWorkspaceFolder`), not inside
the phase tag.

### 3.2 Legal transitions & trigger ownership

**Rule:** only the state loop may mutate the phase.  Handlers never reference
`LspWorkspacePhase` at all — they express **intent** via boolean flags on
`LspWorkspaceUpdate`:

- `InitializeRequested: bool` — set by `handleInitialize`
- `ShutdownRequested: bool` — set by `handleShutdown`

The state loop translates these intents (and internal events) into phase
transitions.  `PhaseTransition` and `WithPhaseTransition` are removed from
`LspWorkspaceUpdate`.

#### Transition table

| From | To | Trigger | Side-effects |
|------|----|---------|-------------|
| `Uninitialized` | `Configured` | `ApplyWorkspaceUpdate` sees `InitializeRequested = true` → posts `ClientInitialize` → state loop sets phase | Register capabilities, set config, start periodic timer |
| `Configured` | `Loading` | `ProcessSolutionAwaiters` — first request calls `GetWorkspaceFolder withSolutionReady=true` | Create `LoadCts`, spawn async Roslyn load |
| `Loading` | `Ready` | `WorkspaceLoadCompleted` (generation matches) | Apply folder solution changes, clear `LoadCts` |
| `Loading` | `Reconfiguring` | `DrainIfPendingOperationsReady` — pending ops are ripe while load is in flight | Cancel `LoadCts`, enter drain mode (cancel all requests) |
| `Loading` | `ShuttingDown` | `ApplyWorkspaceUpdate` sees `ShutdownRequested = true` → posts `ClientShutdown` → state loop sets phase | Cancel `LoadCts`, enter drain mode (cancel all requests) |
| `Ready` | `Reconfiguring` | `DrainIfPendingOperationsReady` — pending ops are ripe | Enter drain mode (cancel all requests) |
| `Ready` | `ShuttingDown` | `ApplyWorkspaceUpdate` sees `ShutdownRequested = true` → posts `ClientShutdown` | Enter drain mode (cancel all requests) |
| `Configured` | `ShuttingDown` | `ApplyWorkspaceUpdate` sees `ShutdownRequested = true` → posts `ClientShutdown` | Enter drain mode (trivially drained if queue empty) |
| `Reconfiguring` | `ShuttingDown` | `ApplyWorkspaceUpdate` sees `ShutdownRequested = true` → posts `ClientShutdown` | Reuse in-flight drain; change post-drain intent from rebuild to halt |
| `Reconfiguring` | `Uninitialized` | `RequestQueueDrained` | Tear down workspace, apply pending ops, rebuild → `Configured`, post `ProcessSolutionAwaiters` |
| `ShuttingDown` | `Uninitialized` | `RequestQueueDrained` | Tear down workspace, signal shutdown channel, halt |

Every transition is **direct** — the state loop sets the phase itself.
Handlers only signal intent (`InitializeRequested`, `ShutdownRequested`);
the state loop decides whether the intent is valid in the current phase and
ignores it otherwise (with a warning log).

#### How pending operations feed into `Reconfiguring`

Pending operations (`PendingReload`, `PendingFolderReplacement`,
`PendingSolutionPathChange`) accumulate on `ServerState.PendingOperations` in
any phase — they are appended during `ApplyWorkspaceUpdate` when a handler
returns reload requests, folder reconfigs, or config changes.  The transition
to `Reconfiguring` is **not** requested by the handler.  Instead:

1. `ApplyWorkspaceUpdate` appends the pending op and posts
   `DrainIfPendingOperationsReady`.
2. `DrainIfPendingOperationsReady` checks whether any pending op is ripe
   (folder/solution changes are always ripe; reloads are ripe when their
   deadline has passed).
3. If ripe **and** phase is `Ready` or `Loading`: set phase to `Reconfiguring`,
   enter drain mode, cancel all requests.
4. If not ripe or phase is already `Reconfiguring`/`ShuttingDown`: no-op.

This means `Reconfiguring` is always an internal state-loop transition — no
handler is involved (handlers have no way to request it; they only have
`InitializeRequested` and `ShutdownRequested`).

#### Which LSP methods can produce pending operations?

| LSP method | Handler | Pending operation produced |
|-----------|---------|--------------------------|
| `workspace/didChangeConfiguration` | `Handlers/Workspace.handleDidChangeConfiguration` | `PendingSolutionPathChange` (if `solutionPathOverride` changed), `PendingReload` (if other config changed) |
| `workspace/didChangeWatchedFiles` | `Handlers/Workspace.handleDidChangeWatchedFiles` | `PendingReload` (debounced) |
| `workspace/didChangeWorkspaceFolders` | `Handlers/Workspace.handleDidChangeWorkspaceFolders` | `PendingFolderReplacement` |

#### Diagram

```
                          ┌──────────────────────────────────────────┐
                          │                                          │
    startup ─► Uninitialized ──[ClientInitialize]──► Configured      │
                  ▲                                      │           │
                  │                               [ProcessSolution-  │
                  │                                Awaiters]         │
                  │                                      ▼           │
                  │                                  Loading         │
                  │                                   │    │         │
                  │                   [LoadCompleted] │    │ [Drain- │
                  │                                   ▼    │ IfPend- │
                  │                                 Ready  │ ingOps] │
                  │                                   │    │         │
                  │                    [DrainIfPend-  │    │         │
                  │                      ingOps]      ▼    ▼         │
                  │                              Reconfiguring       │
                  │                                   │              │
                  │                   [QueueDrained]  │              │
                  │                                   │              │
                  └───────────────────────────────────┘              │
                                                                     │
                  Any non-ShuttingDown ──[shutdown handler]──►       │
                                                ShuttingDown         │
                                                     │               │
                                      [QueueDrained] │               │
                                                     ▼               │
                                               Uninitialized (halt)  │
                                                                     │
                          └──────────────────────────────────────────┘
```

### 3.3 Request queue interaction

The interaction between phase and the request queue must be made explicit:

| Phase | Queue mode | New requests accepted? | Notes |
|-------|-----------|----------------------|-------|
| `Uninitialized` | Dispatching (empty) | Yes — queued but not activated | Server is setting up; requests pile up. |
| `Configured` | Dispatching | Yes — queued but not activated | Load hasn't started; reads that require solution block until `Ready`. |
| `Loading` | Dispatching | Yes — queued; reads requiring solution block; reads NOT requiring solution can run | Non-solution reads (e.g. `textDocument/didOpen` bookkeeping) may proceed. |
| `Ready` | Dispatching | Yes — normal operation | All requests activate per normal concurrency rules. |
| `Reconfiguring` | DrainingUpTo watermark | New requests queued but not activated past watermark | Queue drains **including `ReadOnlyBackground`**; no new activations; after drain the workspace is torn down and rebuilt. |
| `ShuttingDown` | DrainingUpTo watermark | New requests queued but not activated past watermark | Same as Reconfiguring — all requests including background must drain before halt. |

Key invariant: **the queue mode transition to `DrainingUpTo` must happen
atomically with the phase transition to `Reconfiguring` or `ShuttingDown`**.
Currently this is achieved by updating both in the same `ServerEvent` handler,
but it is not enforced.

### 3.4 Request cancellation on drain entry

Currently, entering drain mode (`enterDrainingMode`) is passive: it sets a
watermark ordinal and waits for in-flight requests to finish naturally.  This
makes drains arbitrarily slow — a long `workspace/diagnostics` scan or a slow
completion handler can block the entire reconfiguration.

**Design: when the phase transitions to `Reconfiguring` or `ShuttingDown`,
cancel all in-flight requests immediately.**

#### Current cancellation architecture

```
JsonRpc layer                          Scheduling layer
─────────────                          ────────────────
JsonRpcRequestContext                  RequestInfo
  .CancellationTokenSource ←──────    (no CTS awareness)
  stored in RunningInboundRequests

$/cancelRequest ──► CancelRequest      enterDrainingMode
  ──► cts.Cancel()                       ──► (passive watermark only)
  ──► HandlerCancelled
  ──► -32800 response
```

The two layers are **separate `MailboxProcessor` actors** with no cross-reference.
The state loop cannot currently tell the JsonRpc transport to cancel requests.

#### Proposed cancellation bridge

Add a **per-request `CancellationTokenSource`** to `RequestInfo` in the
scheduling layer.  This CTS is the **same instance** that the JsonRpc layer
uses — threaded through at registration time.

```
JsonRpc layer                          Scheduling layer
─────────────                          ────────────────
JsonRpcRequestContext                  RequestInfo
  .CancellationTokenSource ─shared──► .CancellationTokenSource
  stored in RunningInboundRequests

enterDrainingMode (new behaviour):
  for each Running request:
    cts.Cancel()                       ──► OperationCanceledException
                                       ──► handler finally block
                                       ──► LeaveRequestContext
                                       ──► finishRequest
                                       ──► retireNextFinishedRequest
                                       ──► Drained
```

**Flow:**

1. `wrapHandler` in `Server.fs` already has access to the `JsonRpcRequestContext`
   (which carries the CTS).  When it posts `EnterRequestContext` to the state
   actor, it passes the CTS along.
2. `EnterRequestContext` stores the CTS on `RequestInfo`.
3. When the state loop enters drain mode, it iterates all `Running` requests
   in the queue and calls `cts.Cancel()` on each.
4. The cancellation propagates through the ambient `Async.CancellationToken`
   to the handler async.  The `finally` block in `wrapHandler` posts
   `LeaveRequestContext`, which calls `finishRequest`.
5. `Pending` requests that haven't been activated yet don't need CTS
   cancellation — their `PostAndAsyncReply` on `EnterRequestContext` will
   also be cancelled since the handler async is running under the same CTS.
   But we should also cancel pending requests' CTS to unblock them faster.
6. The normal retirement walk proceeds; once all requests are retired,
   `isDrained` returns true and the drain completes.

**For `Pending` requests beyond the watermark** (registered after drain started):
these are never activated during the drain.  After the drain completes and the
workspace is rebuilt, the queue resets to `Dispatching` mode and these requests
become eligible for activation with the new workspace state.  Their CTS is
**not** cancelled — they are simply deferred.

---

## 4. Concrete Changes

### 4.1 `Lsp/Workspace.fs` — the phase-transition authority

**Architectural rule:** `Lsp/Workspace.fs` owns all `LspWorkspace` mutations
including phase transitions.  `ServerStateLoop.fs` must never write
`{ workspace with Phase = ... }` directly — it always calls a `workspace*`
function that validates the current phase, performs the transition, and
applies any associated workspace-level side-effects (folder teardown,
generation bump, etc.).

This is already the pattern for three transitions today:

| Function | Transition | Validates |
|----------|-----------|-----------|
| `workspaceLoadStarted` | `Configured → Loading` | Asserts `Configured` |
| `workspaceLoadCompleted` | `Loading → Ready` | Asserts `Loading` |
| `workspaceShutdown` | `* → Uninitialized` | (unconditional) |

But two transitions are currently done by raw record-update in
`ServerStateLoop.fs`:

- `{ workspace with Phase = Configured }` in `ClientInitialize` handler
- `{ workspace with Phase = ShuttingDown }` in `DrainIfPendingOperationsReady`

These must be replaced with new `workspace*` functions.

#### Naming convention

All helpers in `Workspace.fs` use a flat `workspace<Noun/Verb>` prefix — no
`With` infix anywhere.  The existing `workspaceWithLoadInitiated` and
`workspaceWithLoadCompleted` have already been renamed to `workspaceLoadStarted`
and `workspaceLoadCompleted`; `workspaceWithFolderUpdated`,
`workspaceWithSolutionPathOverride`, and `workspaceWithPDBacklogUpdatePendingReset`
have been renamed to drop `With` as well.  `workspaceShutdown` already fit the
pattern.

#### Phase-transition functions

| Function | Transition | Validates | Notes |
|----------|-----------|-----------|-------|
| `workspaceConfigured` | `Uninitialized → Configured` | Asserts `Uninitialized` | **NEW.** Currently inlined in state loop. |
| `workspaceLoadStarted` | `Configured → Loading` | Asserts `Configured` | ✅ Renamed. CTS moves out of the phase DU; function still creates and returns it but as a separate value (see §4.4). |
| `workspaceLoadCompleted` | `Loading → Ready` | Asserts `Loading` | ✅ Renamed. |
| `workspaceReconfiguring` | `Ready\|Loading → Reconfiguring` | Asserts `Ready` or `Loading` | **NEW.** If `Loading`, cancels folders' in-flight load markers. |
| `workspaceShuttingDown` | `* → ShuttingDown` (except from `ShuttingDown`) | Asserts not already `ShuttingDown` | **NEW.** Currently inlined in state loop. |
| `workspaceShutdown` | `* → Uninitialized` | (unconditional) | Existing — tears down folders, bumps `Generation`. |

Each function:
1. Asserts the expected source phase (fails/logs on mismatch).
2. Sets the new phase.
3. Performs any workspace-level side-effects (folder state changes, etc.).
4. Returns the updated `LspWorkspace` (pure — no posting of events).

The state loop calls these functions and handles the non-workspace side-effects
itself (entering drain mode, cancelling CTS, posting events, etc.).

#### `LspWorkspacePhase` type changes

- Remove `CancellationTokenSource` from `Loading` case
  (`Loading of CancellationTokenSource` → `Loading`).
- Add `Reconfiguring` case.
- Document the legal transition table as an inline comment on the DU.

#### `LspWorkspaceUpdate` type changes

Remove `PhaseTransition: LspWorkspacePhase option` and `WithPhaseTransition`.
Replace with intent flags:

```fsharp
type LspWorkspaceUpdate =
    { InitializeRequested: bool         // NEW — replaces PhaseTransition(Configured)
      ShutdownRequested: bool           // NEW — replaces PhaseTransition(ShuttingDown)
      ClientCapabilityChange: ClientCapabilities option
      ConfigurationChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      ReloadRequested: TimeSpan list
      FolderReconfiguration: WorkspaceFolder list option
      FolderUpdates: Map<string, LspWorkspaceFolderUpdateFn list> }

    static member Empty =
        { InitializeRequested = false
          ShutdownRequested = false
          ClientCapabilityChange = None
          ... }

    member this.WithInitializeRequested() =
        { this with InitializeRequested = true }

    member this.WithShutdownRequested() =
        { this with ShutdownRequested = true }
```

Handlers no longer import or reference `LspWorkspacePhase`.

### 4.2 `Runtime/ServerStateLoop.fs`

**Architectural rule:** `ServerStateLoop.fs` orchestrates — it decides *when*
to transition (based on events, intents, and pending operations) and handles
non-workspace side-effects (queue mode changes, CTS management, posting
follow-up events).  It must never set `Phase` directly — it calls the
appropriate `workspace*` function from `Workspace.fs`.

Concrete changes:

- In `ApplyWorkspaceUpdate`, replace the `PhaseTransition` dispatch with:
  - `if wsUpdate.InitializeRequested then postServerEvent ClientInitialize`
  - `if wsUpdate.ShutdownRequested then postServerEvent ClientShutdown`
  - No other phase-related logic in `ApplyWorkspaceUpdate` — all phase
    transitions happen in the `ServerEvent` handlers (`ClientInitialize`,
    `ClientShutdown`, `DrainIfPendingOperationsReady`, etc.).
- `ClientInitialize` handler: replace
  `{ workspace with Phase = Configured }` with `workspaceConfigured workspace`.
- `DrainIfPendingOperationsReady` handler: replace
  `{ workspace with Phase = ShuttingDown }` with
  `workspaceReconfiguring workspace` or `workspaceShuttingDown workspace`
  depending on the trigger.  Enter drain mode atomically in the same state
  update.
- `ClientShutdown` handler: call `workspaceShuttingDown workspace`.
  Enter drain mode.
- On `RequestQueueDrained`:
  - If phase is `Reconfiguring`: apply pending operations, call
    `workspaceShutdown`, reconstruct workspace, call
    `workspaceConfigured` (→ `Configured`), then post
    `ProcessSolutionAwaiters` to restart the load.
  - If phase is `ShuttingDown`: call `workspaceShutdown`, signal shutdown
    channel, halt.
- Move the CTS for the in-flight load from being embedded in
  `LspWorkspacePhase.Loading` to a dedicated `LoadCts` field in `ServerState`.
  `workspaceLoadStarted` returns the CTS as a separate value; the state
  loop stores it.  Cancel `LoadCts` before calling `workspaceReconfiguring`
  or `workspaceShuttingDown` when transitioning from `Loading`.
- **`EnterRequestContext`**: Accept and store the per-request
  `CancellationTokenSource` passed from `wrapHandler`.  Thread it into
  `registerRequest` so it ends up on `RequestInfo`.

### 4.3 `Runtime/RequestScheduling.fs`

- **`RequestInfo`**: Add a `CancellationTokenSource option` field.  This is the
  **same CTS instance** created by `JsonRpc.handleInboundRequest` — shared, not
  copied.  It is `Some` for requests that came from the wire, `None` for
  internally-generated events (if any).

- **`isDrained`**: Currently excludes `ReadOnlyBackground` requests from the
  drain check (`r.Mode <> ReadOnlyBackground`).  Remove that exclusion — when
  in `DrainingUpTo` mode, **all** requests (including `ReadOnlyBackground`) must
  be retired before the drain is considered complete.

- **`canActivateRequest`**: `ReadOnlyBackground` requests must **not** be
  activated when the queue is in `DrainingUpTo` mode and their ordinal exceeds
  the drain watermark (this is already enforced by `eligiblePendingRequests`,
  but verify).

- **`retireNextFinishedRequest`**: Currently skips `ReadOnlyBackground` during
  the retirement walk.  During `DrainingUpTo` mode, `ReadOnlyBackground`
  requests must participate in the normal retirement order so they are properly
  drained.

- **`enterDrainingMode`** (changed behaviour): After setting the watermark,
  iterate all requests at or below the watermark and call `cts.Cancel()` on
  each one that has `Phase = Running` or `Phase = Pending`.  This triggers
  immediate cancellation of in-flight handlers and unblocks pending handlers
  waiting on `EnterRequestContext`.  Returns the list of cancelled ordinals
  for logging.

  ```fsharp
  let enterDrainingMode (requestQueue: RequestQueue) : RequestQueue option =
      match requestQueue.Mode with
      | DrainingUpTo _ -> None
      | Dispatching ->
          let maxOrd =
              if Map.isEmpty requestQueue.Requests then 0L
              else requestQueue.Requests |> Map.keys |> Seq.max
          // Cancel all in-flight and pending requests up to the watermark
          requestQueue.Requests
          |> Map.iter (fun ord r ->
              if ord <= maxOrd then
                  r.CancellationTokenSource |> Option.iter (fun cts ->
                      try cts.Cancel() with _ -> ()))
          Some { requestQueue with Mode = DrainingUpTo maxOrd }
  ```

### 4.4 `ServerState` record

```fsharp
type ServerState =
    { ...
      Workspace: LspWorkspace          // Phase no longer carries CTS
      LoadCts: CancellationTokenSource option   // NEW — cancels in-flight load
      RequestQueue: RequestQueue
      PendingOperations: WorkspacePendingOperation list
      ... }
```

### 4.5 `WorkspacePendingOperation` — `PendingSolutionPathChange`

The reconfiguration flow for `solutionPathOverride` changes:

1. `workspace/didChangeConfiguration` handler receives new config.
2. Handler returns `LspWorkspaceUpdate` with
   `ConfigurationChange = Some newConfig`.  The handler does **not** reference
   any phase — it simply reports the config change.
3. `ApplyWorkspaceUpdate` in the state loop processes `ConfigurationChange`,
   detects that `solutionPathOverride` differs from the current config, and
   appends `PendingSolutionPathChange newConfig` to `PendingOperations`.
   Then posts `DrainIfPendingOperationsReady`.
4. `DrainIfPendingOperationsReady` sees a ripe pending op, transitions to
   `Reconfiguring`, enters drain mode (cancelling all requests).
5. Queue drains.
6. `RequestQueueDrained` → apply `PendingSolutionPathChange`, rebuild workspace
   with new path, transition → `Configured`, restart load.

If phase is `Configured` (load never started), just update `Config` in place —
no drain needed.

### 4.6 `Lsp/Server.fs` — `wrapHandler`

- `wrapHandler` already has access to the `JsonRpcRequestContext` which carries
  the `CancellationTokenSource`.  Update the `EnterRequestContext` message to
  include it:

  ```fsharp
  // Before:
  EnterRequestContext(jsonRpcCtx.RequestOrdinal, jsonRpcCtx.MethodName, requestMode, rc)

  // After:
  EnterRequestContext(jsonRpcCtx.RequestOrdinal, jsonRpcCtx.MethodName, requestMode,
                      jsonRpcCtx.CancellationTokenSource, rc)
  ```

- The `ServerEvent` DU must be updated to match:

  ```fsharp
  | EnterRequestContext of int64 * string * RequestMode
                         * CancellationTokenSource option   // NEW
                         * AsyncReplyChannel<RequestContext>
  ```

### 4.7 `Handlers/LifeCycle.fs`

- `handleInitialize`: replace `.WithPhaseTransition(LspWorkspacePhase.Configured)`
  with `.WithInitializeRequested()`.
- `handleShutdown`: replace `.WithPhaseTransition(LspWorkspacePhase.ShuttingDown)`
  with `.WithShutdownRequested()`.
- Both handlers no longer import or reference `LspWorkspacePhase`.

---

## 5. Invariants to Enforce / Assert

1. Phase only ever changes via `workspace*` functions in `Workspace.fs`, called
   from the `MailboxProcessor` state loop.  `ServerStateLoop.fs` never writes
   `{ workspace with Phase = ... }` directly.
2. `DrainingUpTo` queue mode is set if and only if phase is `Reconfiguring` or
   `ShuttingDown`.
3. `LoadCts` is `Some` if and only if phase is `Loading`.
4. `PendingOperations` may accumulate while the server is in any phase (e.g.
   reload delays queue up in `Ready`), but they are only **applied** after the
   phase enters `Reconfiguring` and the queue drains.  Once applied, the list
   is cleared.
5. `Generation` is bumped every time `workspaceShutdown` is called.
6. During `DrainingUpTo`, `isDrained` must consider **all** request modes
   including `ReadOnlyBackground` — no requests may be left running when the
   drain completes.
7. Every `Running` or `Pending` request at or below the drain watermark has had
   its CTS cancelled when `DrainingUpTo` mode was entered.

Add `assertServerStateInvariants` (active only in `DEBUG` builds, or via a
config flag) that runs after every state transition.

---

## 6. Open Questions

- **Q1**: Should `Configured` be collapsed into `Uninitialized`?  The distinction
  matters only for "load hasn't started yet vs. server hasn't received
  `initialize`".  Keeping them separate makes the phase model cleaner at the cost
  of one extra case.  **Recommendation: keep separate.**

- **Q2**: Should background-only requests (`ReadOnlyBackground`, e.g.
  `workspace/diagnostics`) be allowed to continue during `Reconfiguring`?
  **Decision: NO.**  During `Reconfiguring` (and `ShuttingDown`), all requests
  — including `ReadOnlyBackground` — must be drained before the workspace is
  torn down.  This is a change from the current behaviour where `isDrained`
  explicitly excludes `ReadOnlyBackground` (they never block draining today).
  Rationale: background requests hold references to the current workspace/solution
  state; allowing them to run while the workspace is being torn down and rebuilt
  risks reading stale or half-destroyed state.  The `isDrained` function in
  `RequestScheduling.fs` must be updated to **not** skip `ReadOnlyBackground`
  when `DrainingUpTo` mode is active.

- **Q3**: What should happen to in-flight `Loading` when a `PendingOperation`
  arrives?  The current code stores the operation and waits.  With `Reconfiguring`
  we should cancel the load immediately (via `LoadCts`) and start draining.
  **Recommendation: cancel and drain immediately.**

- **Q4**: Multiple rapid `solutionPathOverride` changes — should they coalesce?
  `PendingOperations` is a list; today only the last `PendingSolutionPathChange`
  would be applied.  Need to confirm coalescing is intentional.
  **Recommendation: yes, keep last wins.**

---

## 7. Implementation Order

1. **`LspWorkspaceUpdate`**: Remove `PhaseTransition` and `WithPhaseTransition`.
   Add `InitializeRequested: bool`, `ShutdownRequested: bool`,
   `WithInitializeRequested()`, `WithShutdownRequested()`.  Update handler
   call sites (only two: `handleInitialize`, `handleShutdown`).
2. **`LspWorkspacePhase`**: Add `Reconfiguring`, remove CTS from `Loading`.
   Propagate the type change.
3. **`Workspace.fs` — phase-transition functions**: Add `workspaceConfigured`,
   `workspaceReconfiguring`, `workspaceShuttingDown`.  Rename
   `workspaceLoadStarted` (returns CTS as separate value; already renamed).
   `workspaceLoadCompleted` (already renamed).  Each function validates source phase and performs
   the transition.
4. **`ServerStateLoop.fs` — use `workspace*` functions**: Replace all inline
   `{ workspace with Phase = ... }` with calls to the corresponding
   `workspace*` function.  Replace `PhaseTransition` dispatch in
   `ApplyWorkspaceUpdate` with `InitializeRequested` → `postServerEvent
   ClientInitialize` and `ShutdownRequested` → `postServerEvent ClientShutdown`.
5. Add `LoadCts` to `ServerState`; wire up CTS cancellation on phase change.
6. **Thread per-request CTS from JsonRpc → scheduling layer:**
   a. Add `CancellationTokenSource option` to `RequestInfo`.
   b. Update `EnterRequestContext` in `ServerEvent` to carry the CTS.
   c. Update `wrapHandler` in `Server.fs` to pass `jsonRpcCtx.CancellationTokenSource`.
   d. Update `registerRequest` to store the CTS on `RequestInfo`.
7. **Update `enterDrainingMode`** to cancel all in-flight and pending requests
   at or below the watermark (call `cts.Cancel()` on each).
8. Update `isDrained` and `retireNextFinishedRequest` to include
   `ReadOnlyBackground` requests during `DrainingUpTo` mode.
9. Split `RequestQueueDrained` handler into reload vs. shutdown branches.
10. Update `ApplyWorkspaceUpdate` to detect `solutionPathOverride` changes in
    `ConfigurationChange` and append `PendingSolutionPathChange` to pending ops.
11. Add `assertServerStateInvariants` (debug-only).
12. Write integration tests covering:
    - Normal init → load → ready cycle.
    - `solutionPathOverride` change while `Ready` → drain → reload.
    - `solutionPathOverride` change while `Loading` → cancel + drain → reload.
    - Client `shutdown` during `Loading`.
    - Client `shutdown` during `Reconfiguring`.
    - Verify that in-flight handlers receive `OperationCanceledException`
      when drain starts (cancellation propagation test).
