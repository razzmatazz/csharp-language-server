# Workspace Phase — Design Plan

> **Status:** Partially implemented. See §7 (Implementation Order) for what is done,
> deferred, and what newly needs fixing. See §8 for a known regression introduced by the
> active-drain change (step 7) and the plan to fix it.

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
2. ~~**`LspWorkspacePhase`**: Add `Reconfiguring`, remove CTS from `Loading`.
   Propagate the type change.~~ **DEFERRED.** `Reconfiguring` was added
   (`57496a77`) but the CTS has not been removed from `Loading of cts:`.
   The private `cancelLoadCts` helper in `Workspace.fs` already extracts and
   cancels it by pattern-matching the phase, so correctness is unaffected.
   The only remaining impurity is the mutable value inside the DU case.
   Removing it requires adding `LoadCts` to `ServerState` (step 5) and
   threading the CTS out of `workspaceLoadStarted` — deferred to keep scope
   manageable; revisit when integration tests (step 12) are passing.
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
5. ~~Add `LoadCts` to `ServerState`; wire up CTS cancellation on phase change.~~
   **DEFERRED** (depends on step 2). See note on step 2 above.
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

---

## 8. Regression: active drain cancels requests during initial load

### 8.1 Symptom

After step 7 (`enterDrainingMode` now cancels in-flight handlers), any handler
test that issues a request before the workspace reaches `Ready` fails.
Confirmed failures include `testDefinitionWorks` and `testHoverWorks`; the
failure mode is generic to every handler that calls
`GetWorkspaceFolderReadySolution` (Hover, Definition, References, Completion,
Rename, InlayHint, CodeAction, TypeDefinition, Implementation, DocumentHighlight,
DocumentSymbol, SignatureHelp, CodeLens, SemanticTokens, Diagnostic, …).
The handler returns `None` because the awaiter parked on
`SolutionReadyAwaiters` is replied with `None` (and/or the request CTS is
cancelled) when an active drain fires during initial load.

### 8.2 Root Cause

The sequence of events during startup:

```
initialize
  → handleInitialize returns:
      WithFolderReconfiguration([workspaceFolders])   ← workspace folders set here
      WithInitializeRequested()

  ApplyWorkspaceUpdate:
    → PendingFolderReplacement appended to PendingOperations
    → DrainIfPendingOperationsReady posted

  ClientInitialize (posted by ApplyWorkspaceUpdate):
    → workspaceConfigured: Uninitialized → Configured

  DrainIfPendingOperationsReady:
    → phase is Configured → canReconfigure = false → no drain  ✓

initialized
  → handleInitialized pulls workspace/configuration
  → returns WithConfigurationChange(...)

  ApplyWorkspaceUpdate:
    → DrainIfPendingOperationsReady posted
    → phase is still Configured → no drain  ✓

textDocument/didOpen
textDocument/definition
  → handler calls GetWorkspaceFolderReadySolution(uri, withSolutionReady=true)
  → state loop adds caller to SolutionReadyAwaiters
  → ProcessSolutionAwaiters posted

  ProcessSolutionAwaiters:
    → phase is Configured → workspaceLoadStarted → phase: Configured → Loading cts
    → ASYNC Roslyn load starts in background

  ApplyWorkspaceUpdate (from textDocument/didOpen retiring):
    → DrainIfPendingOperationsReady posted

  DrainIfPendingOperationsReady:
    → phase is NOW Loading
    → PendingFolderReplacement is ripe (always ripe)
    → canReconfigure = true  ✗ BUG
    → enterDrainingMode called
    → definition request CTS cancelled!
    → handler throws OperationCanceledException → returns None
```

The `PendingFolderReplacement` from `handleInitialize` is the **initial folder
setup**, not a runtime reconfiguration.  It is unconditionally ripe and, once the
phase reaches `Loading`, it triggers an immediate drain that cancels the very
handlers that are waiting for the solution to load.

### 8.3 Design overview: two complementary layers

The fix is structured as two layers of defense.  Each layer alone closes
most of the bug; together they make the bug class statically impossible
*and* protocol-correct.

| Layer | What it adds | Bug class addressed |
|-------|--------------|---------------------|
| **L1 — `Initializing` phase + load gate** | A new phase between `Uninitialized` and `Configured`; `workspaceLoadStarted` is type-restricted to `Configured`, which is only entered after a graduation predicate (`InitializedReceived ∧ PendingOperations = []`) holds. | Premature load in any phase that still has pending ops; type-level enforcement of the gate. |
| **L2 — Synchronous handler completion** | `handleInitialize` and `handleInitialized` do not return their LSP response until the workspace has settled.  The next request from the client is therefore queued behind a fully-configured server. | The protocol-level race: a client sending `didOpen`/`hover` before `initialized` (or before its `workspace/configuration` response has been processed). |

Both layers refer to the same `Initializing` phase but address different
failure modes:

- L1 closes the *internal* race (queued ops + first awaiter triggering
  `Configured → Loading` while a stale pending op is still in the queue).
- L2 closes the *external* race (a misbehaving or fast client racing past
  `initialize`/`initialized` before server-side settlement).

L1 is sufficient to fix the *currently observed* bug (`testHoverWorks`,
`testDefinitionWorks`).  L2 is sufficient to keep the
`Configured`-phase window invisible to clients.  Together they form
defense-in-depth.

### 8.4 Layer 1 — `Initializing` phase + graduation predicate

#### 8.4.1 New state machine

A new phase is added between `Uninitialized` and `Configured`:

```
Uninitialized → Initializing → Configured → Loading → Ready
                ↑               ↑
                │               └── workspaceConfigured (semantics changed:
                │                    source phase is now Initializing)
                └── workspaceInitializeStarted (NEW; Uninitialized → Initializing)
```

Phase contracts:

- **`Initializing`** — `initialize` has retired and the periodic timer is
  running, but at least one of:
  - `initialized` has not yet been received from the client, *or*
  - `PendingOperations` is non-empty.

  In this phase `workspaceLoadStarted` is structurally forbidden — its
  pattern match accepts only source phase `Configured`, so the type system
  prevents the bug entirely.

- **`Configured`** — `initialize` and `initialized` have both retired,
  workspace folders/config have been fully applied, and the workspace is
  ready to begin loading on the next awaiter.  Entered atomically via
  `workspaceConfigured` only when both gating conditions are met.

The reconfiguration path (`Reconfiguring → Uninitialized → … → Configured`)
is unchanged in spirit: after the rebuild, the workspace transitions
`Uninitialized → Initializing → Configured` in a single step, since
`InitializedReceived` is already true and pending ops have been applied by
the rebuild fold.

#### 8.4.2 Workspace.fs changes

Add `Initializing` to the phase enum:

```fsharp
type LspWorkspacePhase =
    | Uninitialized
    | Initializing      // NEW
    | Configured
    | Loading of CancellationTokenSource
    | Ready
    | Reconfiguring
    | ShuttingDown
```

Add a new transition function:

```fsharp
let workspaceInitializeStarted (workspace: LspWorkspace) : LspWorkspace =
    match workspace.Phase with
    | LspWorkspacePhase.Uninitialized ->
        { workspace with Phase = LspWorkspacePhase.Initializing }
    | _ -> failwithf "workspaceInitializeStarted: expected Uninitialized but got '%A'" workspace.Phase
```

Change `workspaceConfigured`'s source phase from `Uninitialized` to
`Initializing`:

```fsharp
let workspaceConfigured (workspace: LspWorkspace) : LspWorkspace =
    match workspace.Phase with
    | LspWorkspacePhase.Initializing ->
        { workspace with Phase = LspWorkspacePhase.Configured }
    | _ -> failwithf "workspaceConfigured: expected Initializing but got '%A'" workspace.Phase
```

Add a new field to `LspWorkspaceUpdate`:

```fsharp
type LspWorkspaceUpdate =
    { …
      InitializedReceived: bool   // NEW
      … }

    member this.WithInitializedReceived() =
        { this with InitializedReceived = true }
```

`workspaceLoadStarted`, `workspaceReconfiguring`, `workspaceShuttingDown`,
and `workspaceShutdown` are unchanged.

#### 8.4.3 ServerStateLoop.fs changes

Add a new server event:

```fsharp
type ServerEvent =
    …
    | ClientInitialized
```

Add a new field to `ServerState`:

```fsharp
type ServerState =
    { …
      InitializedReceived: bool   // default false
      … }
```

Add a graduation helper:

```fsharp
let private tryGraduateInitializing (state: ServerState) postServerEvent : ServerState =
    match state.Workspace.Phase, state.InitializedReceived, state.PendingOperations with
    | LspWorkspacePhase.Initializing, true, [] ->
        postServerEvent ProcessSolutionAwaiters
        // (L2 will additionally signal handler-completion waiters here; see §8.5)
        { state with Workspace = workspaceConfigured state.Workspace }
    | _ -> state
```

Hoist `applyPendingOperation` to module scope (currently inlined inside
`RequestQueueDrained`'s `Reconfiguring` branch):

```fsharp
let private applyPendingOperation
    (config: CSharpConfiguration)
    (ws: LspWorkspace)
    (op: WorkspacePendingOperation) =
    match op with
    | PendingReload _ -> ws
    | PendingFolderReplacement newFolders ->
        workspaceFrom newFolders |> workspaceSolutionPathOverride config
    | PendingSolutionPathChange _ ->
        workspaceSolutionPathOverride config ws
```

`ApplyWorkspaceUpdate`:

- Continue to post `ClientInitialize` when `wsUpdate.InitializeRequested`.
- Additionally post `ClientInitialized` when `wsUpdate.InitializedReceived`.
- **Remove** the early-phase inline `solutionPathOverride` handling
  (`solutionPathChangedConfig` / `fromSolutionPathChange` / `newWorkspace`
  blocks).  All `solutionPathOverride` changes now uniformly produce a
  `PendingSolutionPathChange` regardless of phase, applied either inline
  (via the `Initializing` branch of `DrainIfPendingOperationsReady`) or
  destructively (via the `Ready/Loading` branch).

`ClientInitialize`:

```fsharp
| ClientInitialize ->
    let timer = …
    let newState =
        { state with
            Workspace = workspaceInitializeStarted state.Workspace   // ← was workspaceConfigured
            PeriodicTickTimer = Some timer }
    return tryGraduateInitializing newState postServerEvent
```

(Graduation will fail here because `InitializedReceived` is still false,
but the call site is uniform.)

`ClientInitialized` (new):

```fsharp
| ClientInitialized ->
    let newState = { state with InitializedReceived = true }
    return tryGraduateInitializing newState postServerEvent
```

`DrainIfPendingOperationsReady`:

```fsharp
| DrainIfPendingOperationsReady ->
    let shouldDrain = … // unchanged ripeness check

    match state.Workspace.Phase with
    | LspWorkspacePhase.Initializing when shouldDrain ->
        // Apply pending ops inline.  No CTS cancellation, no torn solution —
        // there is no live Roslyn solution yet.  Awaiters stay parked and are
        // re-evaluated by tryGraduateInitializing.
        let applied =
            state.PendingOperations
            |> List.fold (applyPendingOperation state.Config) state.Workspace
        let newState =
            { state with
                Workspace = applied
                PendingOperations = [] }
        return tryGraduateInitializing newState postServerEvent

    | LspWorkspacePhase.Ready | LspWorkspacePhase.Loading _ when shouldDrain ->
        // existing destructive path
        match enterDrainingMode state.RequestQueue with
        | None -> return state
        | Some updatedRequestQueue ->
            postServerEvent ProcessRequestQueue
            return { state with
                       RequestQueue = updatedRequestQueue
                       Workspace = workspaceReconfiguring state.Workspace }

    | _ -> return state
```

`ProcessSolutionAwaiters`:

```fsharp
match state.Workspace.Phase with
| LspWorkspacePhase.Configured ->
    // Gate is now structural: we are only here if InitializedReceived is true
    // and PendingOperations was empty at graduation time.
    // Fire workspaceLoadStarted as before.
    …

| LspWorkspacePhase.Ready ->
    // existing immediate-reply logic
    …

| _ -> return state    // Initializing, Loading, etc. — keep awaiters parked
```

`RequestQueueDrained.Reconfiguring`:

```fsharp
let configuredWorkspace =
    rebuiltWorkspace
    |> workspaceInitializeStarted   // Uninitialized → Initializing
    |> workspaceConfigured          // Initializing → Configured
                                    // (safe: InitializedReceived = true,
                                    //  PendingOperations = [] after rebuild)
```

#### 8.4.4 LifeCycle.fs change

`handleInitialized` adds the new flag unconditionally:

```fsharp
let mutable wsUpdate = LspWorkspaceUpdate.Empty.WithInitializedReceived()
```

Set regardless of whether the client supports `workspace/configuration`,
since the gating signal is "`initialized` notification was received" —
independent of whether config was successfully pulled.

### 8.5 Layer 2 — Synchronous handler completion

L1 alone leaves a window during which the workspace is `Initializing` but
the client has already received `InitializeResult` and may have moved on.
A misbehaving client (or simply a fast one) can race past this window with
`didOpen`/`hover`.  L1 still parks those awaiters safely until graduation,
but only ReadWrite serialization on `handleInitialized` keeps subsequent
ReadWrite requests from interleaving — and ReadOnly requests may or may
not be serialized similarly (see §8.8.4).

L2 closes this race by making the relevant lifecycle handlers block until
the server is fully configured.

#### 8.5.1 Mechanism

A new `ServerEvent` carries a one-shot reply channel:

```fsharp
type ServerEvent =
    …
    | WaitForWorkspaceConfigured of AsyncReplyChannel<unit>
```

A new field on `ServerState`:

```fsharp
type ServerState =
    { …
      WorkspaceConfiguredWaiters: AsyncReplyChannel<unit> list   // NEW
      … }
```

Handler logic:

```fsharp
| WaitForWorkspaceConfigured replyChannel ->
    match state.Workspace.Phase with
    | LspWorkspacePhase.Configured
    | LspWorkspacePhase.Loading _
    | LspWorkspacePhase.Ready ->
        // Already configured (or further along) — reply immediately.
        replyChannel.Reply()
        return state
    | LspWorkspacePhase.ShuttingDown ->
        // Server halting; do not block the handler.  Reply (the handler
        // can detect ShutdownReceived via context if it wishes).
        replyChannel.Reply()
        return state
    | _ ->
        // Initializing or Uninitialized — park until graduation.
        return { state with
                   WorkspaceConfiguredWaiters = replyChannel :: state.WorkspaceConfiguredWaiters }
```

`tryGraduateInitializing` is extended to flush waiters on the
`Initializing → Configured` transition:

```fsharp
let private tryGraduateInitializing (state: ServerState) postServerEvent : ServerState =
    match state.Workspace.Phase, state.InitializedReceived, state.PendingOperations with
    | LspWorkspacePhase.Initializing, true, [] ->
        postServerEvent ProcessSolutionAwaiters
        for rc in state.WorkspaceConfiguredWaiters do rc.Reply()
        { state with
            Workspace = workspaceConfigured state.Workspace
            WorkspaceConfiguredWaiters = [] }
    | _ -> state
```

Similarly, `ClientShutdown` flushes waiters so handlers stuck on
`WaitForWorkspaceConfigured` during shutdown don't deadlock:

```fsharp
| ClientShutdown ->
    …
    for rc in state.WorkspaceConfiguredWaiters do rc.Reply()
    return { state with
               …
               WorkspaceConfiguredWaiters = [] }
```

#### 8.5.2 RequestContext extension

Expose the wait via `RequestContext`:

```fsharp
type RequestContext with
    member _.WaitForWorkspaceConfigured() : Async<unit> =
        inbox.PostAndAsyncReply(fun rc -> WaitForWorkspaceConfigured rc)
```

#### 8.5.3 Handler usage

`handleInitialize` does **not** wait — graduation cannot fire until
`initialized` arrives, so waiting here would deadlock.  It returns
synchronously as today.

`handleInitialized` waits at the end of its body, after building wsUpdate
and triggering the lifecycle that will eventually graduate the workspace:

```fsharp
let handleInitialized lspClient getDynamicRegistrations context (_p: unit) = async {
    …
    // Build wsUpdate, including .WithInitializedReceived() and any
    // .WithConfigurationChange(...) from pulled config.

    // Block until the workspace has graduated to Configured.
    // The wsUpdate we are about to return triggers the graduation chain
    // (InitializedReceived flips true; pending ops drain; tryGraduateInitializing
    //  fires).  By the time this method returns, the next request from the
    //  client (queued behind us in the request scheduler) will dispatch into
    //  a fully-configured server.
    do! context.WaitForWorkspaceConfigured()

    return Ok(), wsUpdate
}
```

#### 8.5.4 Critical sequencing constraint

**The handler must register the waiter before its wsUpdate has been
applied** — otherwise graduation may fire before the waiter is parked, and
the waiter waits forever.

Today, the handler returns `(result, wsUpdate)`, the framework calls
`LeaveRequestContext`, which queues the wsUpdate; only after that does
`ApplyWorkspaceUpdate` run and `ClientInitialized` fire.  So the natural
ordering is:

```
handler body running
  → handler awaits WaitForWorkspaceConfigured  (waiter parked)
  → handler returns (result, wsUpdate)
  → framework calls LeaveRequestContext
  → ApplyWorkspaceUpdate posted
  → ClientInitialized posted (sets InitializedReceived = true)
  → DrainIfPendingOperationsReady (applies pending ops)
  → tryGraduateInitializing (replies to waiter)
  → handler resumes → response sent to client
```

The waiter is parked **before** the wsUpdate is even submitted — this is
correct: the mailbox is single-threaded, so the
`WaitForWorkspaceConfigured` event runs to completion (parking the waiter
on the state list) before any subsequent event can fire graduation.

### 8.6 Event-flow walk-through (post-fix, both layers)

```
initialize retires:
  Handler body completes; framework wraps:
    → LeaveRequestContext (with wsUpdate having InitializeRequested + FolderReconfiguration)
    → ApplyWorkspaceUpdate posted

  ApplyWorkspaceUpdate:
    → PendingFolderReplacement queued
    → ClientInitialize posted
    → DrainIfPendingOperationsReady posted

  ClientInitialize:
    → workspaceInitializeStarted: Uninitialized → Initializing
    → tryGraduateInitializing: InitializedReceived=false → no transition

  DrainIfPendingOperationsReady:
    → phase = Initializing, shouldDrain = true
    → applyPendingOperation folds → folders applied
    → PendingOperations = []
    → tryGraduateInitializing: still InitializedReceived=false → no transition

  Client receives InitializeResult.

initialized retires:
  Handler body running:
    → builds wsUpdate with WithInitializedReceived() and WithConfigurationChange(...)
    → do! WaitForWorkspaceConfigured()       ← waiter parked
    → handler return is suspended

  WaitForWorkspaceConfigured event:
    → phase = Initializing → waiter added to WorkspaceConfiguredWaiters

  Handler returns (Ok(), wsUpdate).
  LeaveRequestContext → ApplyWorkspaceUpdate posted:
    → PendingSolutionPathChange queued (if config has solutionPathOverride)
    → ConfigurationChange event posted (inline application)
    → ClientInitialized posted
    → DrainIfPendingOperationsReady posted

  ClientInitialized:
    → InitializedReceived = true
    → tryGraduateInitializing: PendingOperations may be non-empty → no transition (yet)

  DrainIfPendingOperationsReady:
    → phase = Initializing, shouldDrain = true (or false if no pending ops)
    → applies any pending ops
    → tryGraduateInitializing: InitializedReceived=true, PendingOperations=[]
        → workspaceConfigured: Initializing → Configured
        → ProcessSolutionAwaiters posted
        → WorkspaceConfiguredWaiters flushed → handler resumes

  Handler completes; framework sends `initialized` ack (no-op for a notification,
  but the request scheduler now retires it and dispatches the next request).

textDocument/didOpen, textDocument/hover (queued behind handleInitialized):
  Hover handler → GetWorkspaceFolderReadySolution
    → awaiter parked
    → ProcessSolutionAwaiters posted

  ProcessSolutionAwaiters:
    → phase = Configured → workspaceLoadStarted: Configured → Loading cts
    → Roslyn load starts in background

  Hover awaiter is satisfied when WorkspaceLoadCompleted fires.
```

Even if a misbehaving client sends `hover` *before* `initialized`:

```
…
  ProcessSolutionAwaiters (posted by GetWorkspaceFolder):
    → phase = Initializing → fall-through; awaiter parked
…
  (eventually `initialized` arrives → graduation fires →
   ProcessSolutionAwaiters reposted → load starts → awaiter satisfied)
```

The handler waits, but is never cancelled, never gets `None`.

### 8.7 Why this is correct (defense-in-depth)

- **Type-level enforcement (L1).**  `workspaceLoadStarted` accepts source
  phase `Configured` only.  Reaching `Configured` requires going through
  `tryGraduateInitializing`, which checks `InitializedReceived ∧
  PendingOperations = []`.  The bug class is statically impossible.

- **Protocol-level enforcement (L2).**  Even if L1 had a hole, the
  blocking `handleInitialized` ensures that the `Initializing` window is
  invisible to the client: by the time the request scheduler dispatches
  the *next* request after `handleInitialized`, the workspace has
  graduated.

- **No CTS cancellation during init.**  `enterDrainingMode` is only
  called from the `Ready/Loading` branch of
  `DrainIfPendingOperationsReady`.  The `Initializing` branch is
  non-destructive: pending ops are applied inline; awaiters remain
  parked.

- **Reconfiguration semantics preserved.**  Genuine reconfigurations
  after `Ready` go through the destructive `Ready/Loading` drain branch,
  exactly as before.  The post-drain rebuild transitions
  `Uninitialized → Initializing → Configured` in one step (graduation
  succeeds immediately because `InitializedReceived = true` and pending
  ops were applied by the rebuild fold).

- **Shutdown safety.**  `WorkspaceConfiguredWaiters` is flushed in
  `ClientShutdown`, so handlers blocked on
  `WaitForWorkspaceConfigured()` during shutdown unblock and return
  cleanly.

### 8.8 Open design decisions

These are deliberately left for resolution at implementation time.

1. **Should `handleInitialize` also block?**  L1 alone makes
   `handleInitialize` blocking unnecessary (its wsUpdate will be applied
   regardless before any subsequent request retires).  But waiting on
   "wsUpdate applied to state" (a single mailbox round-trip, not full
   graduation) would let `handleInitialize` guarantee folder-list
   visibility before the client moves on.  Marginal benefit; default
   choice: do not block.

2. **Naming.** `DrainIfPendingOperationsReady` becomes a misnomer when
   one branch applies inline rather than draining.  Options:
   - Keep the name, branch internally (smallest diff).
   - Rename to `ApplyPendingOperationsIfReady`.
   - Split into two events (`ApplyPendingOperationsInline` for
     `Initializing` and `BeginReconfigurationDrain` for
     `Ready`/`Loading`); cleanest separation, more wiring.

   Default choice: keep the name, branch internally.

3. **`InitializedReceived` lifecycle.**  Stays sticky across
   reconfigurations (no need to reset), so the post-drain
   `Uninitialized → Initializing → Configured` rebuild graduates
   immediately.  Reset to `false` on `ClientShutdown` for cleanliness.

4. **Scheduler audit needed.**  Confirm that
   `processRequestQueue` (in `Runtime/RequestScheduling.fs`, line ~256)
   does not activate ReadOnly requests concurrently with a still-running
   ReadWrite handler.  If it does, ReadOnly requests can slip past a
   blocked `handleInitialized`, and L2's protocol guarantee weakens —
   though L1 still saves us in that case (awaiters park in
   `Initializing`).  Audit deferred to implementation.

5. **`Initializing` in `assertServerStateInvariants`.**  Invariant B's
   `phaseExpectsTimer` set must include `Initializing` (the timer is
   created in `ClientInitialize`, which now flips to `Initializing`).

### 8.9 Debug invariants

- **Invariant B (updated).** `PeriodicTickTimer` is `Some` for every
  phase in `{ Initializing, Configured, Loading, Ready, Reconfiguring }`
  (extended to include `Initializing`).

- **Invariant C — Load gate.**  `workspaceLoadStarted` is only ever
  invoked from a `Configured` phase whose graduation observed
  `InitializedReceived = true ∧ PendingOperations = []`.  Enforced
  structurally (only `tryGraduateInitializing` produces `Configured`)
  *and* by the precondition pattern in `workspaceLoadStarted`.

- **Invariant D — Initialization order.**  If
  `state.InitializedReceived = true` then
  `state.Workspace.Phase ∉ { Uninitialized }`.  Catches the
  (impossible-by-construction) case where `ClientInitialized` somehow
  fires before `ClientInitialize`.

- **Invariant E — Waiter discipline.**
  `state.WorkspaceConfiguredWaiters` is non-empty only when
  `state.Workspace.Phase ∈ { Uninitialized, Initializing }`.  Waiters
  are flushed on `Initializing → Configured` and on `* → ShuttingDown`.

### 8.10 Files to change

- `src/CSharpLanguageServer/Lsp/Workspace.fs`:
  - `LspWorkspacePhase`: add `Initializing`.
  - Add `workspaceInitializeStarted` (Uninit → Initializing).
  - Change `workspaceConfigured` source phase to `Initializing`.
  - `LspWorkspaceUpdate`: add `InitializedReceived: bool` +
    `WithInitializedReceived()`.

- `src/CSharpLanguageServer/Handlers/LifeCycle.fs`:
  - `handleInitialized`: emit `.WithInitializedReceived()` unconditionally.
  - `handleInitialized`: insert `do! context.WaitForWorkspaceConfigured()`
    before returning.

- `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs`:
  - Add `ClientInitialized` and `WaitForWorkspaceConfigured` events.
  - Add `InitializedReceived: bool` and
    `WorkspaceConfiguredWaiters: AsyncReplyChannel<unit> list` to
    `ServerState`.
  - Add `tryGraduateInitializing` helper (with waiter flush).
  - Hoist `applyPendingOperation` to module scope.
  - `ApplyWorkspaceUpdate`: post `ClientInitialized` for the new flag;
    remove early-phase inline `solutionPathOverride` handling
    (`solutionPathChangedConfig` / `fromSolutionPathChange` /
    `newWorkspace`).
  - `ClientInitialize`: switch to `workspaceInitializeStarted` + try
    graduation.
  - Add `ClientInitialized` handler.
  - Add `WaitForWorkspaceConfigured` handler (immediate reply for
    `Configured`/`Loading`/`Ready`/`ShuttingDown`; park otherwise).
  - `DrainIfPendingOperationsReady`: phase-branch into Initializing
    (non-destructive) / Ready+Loading (destructive) / other (noop).
  - `ProcessSolutionAwaiters`: keep `Configured`-fires-load and
    `Ready`-replies-immediately branches; fall through in `Initializing`.
  - `RequestQueueDrained.Reconfiguring`: chain
    `workspaceInitializeStarted >> workspaceConfigured` for rebuild.
  - `ClientShutdown`: flush `WorkspaceConfiguredWaiters`; reset
    `InitializedReceived`.
  - `assertServerStateInvariants`: extend Invariant B's timer set with
    `Initializing`; add Invariants C, D, E.

- `src/CSharpLanguageServer/Runtime/RequestScheduling.fs`:
  - `RequestContext`: add `WaitForWorkspaceConfigured() : Async<unit>`.
  - Audit `processRequestQueue`'s `workspacePhase` switch (line ~256) to
    confirm `Initializing` is treated equivalently to `Configured` for
    request activation purposes (per §8.8.4).

No changes in any handler module other than `LifeCycle.fs`.

### 8.11 Test coverage

State-loop unit tests (new):

- **Graduation ordering — `initialized` last.**  Drive
  `Uninitialized → Initializing` (via `ClientInitialize`); apply a
  pending folder reconfig; assert phase is still `Initializing` and any
  awaiter registered now is parked.  Fire `ClientInitialized`; assert
  `Initializing → Configured` happens and the awaiter triggers
  `workspaceLoadStarted`.

- **Graduation ordering — pending op last.**  Fire `ClientInitialize`
  then `ClientInitialized` (without applying pending ops); assert phase
  remains `Initializing`.  Then trigger drain; assert graduation fires.

- **Blocking handler — happy path.**  Mock the framework: invoke
  `handleInitialized`; assert it does not return until the state loop
  reaches `Configured`.

- **Blocking handler — shutdown unblocks.**  Invoke `handleInitialized`,
  then immediately drive `ClientShutdown`; assert handler returns
  (via the waiter flush).

- **Misbehaving client — early request.**  Drive `ClientInitialize`,
  then issue a `GetWorkspaceFolder(uri, withSolutionReady = true)`
  *before* `ClientInitialized`; assert awaiter parks; fire
  `ClientInitialized`; assert awaiter is satisfied with `Some wf`.

- **Reconfiguration regression.**  Drive the loop to `Ready`; trigger
  a runtime folder reconfig; assert destructive drain →
  `Uninitialized → Initializing → Configured` rebuild path runs and
  graduates immediately (because `InitializedReceived = true`).

Existing handler integration tests (`testHoverWorks`,
`testDefinitionWorks`, etc.) resolve as a side-effect.
