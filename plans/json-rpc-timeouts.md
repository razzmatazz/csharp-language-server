# JSON-RPC transport call timeout

## Background

`sendJsonRpcCall` currently waits forever for a response from the remote peer.  In
the test harness (`LspTestClient.Request`) this means a hung LSP server — deadlock,
infinite loop, or simply a slow response — will stall the calling test thread
indefinitely, with no way to recover short of killing the process.

The issue was discovered while investigating why `Async.RunSynchronously(timeout=…)`
did not terminate as expected when a call hung.  That investigation produced two
insights that shape the design here.

---

## Why `Async.RunSynchronously(timeout)` does not help

`Async.RunSynchronously(computation, timeout = N)` implements its timeout via
**cooperative cancellation**:

1. It arms a timer for `N` milliseconds.
2. When the timer fires it signals a `CancellationTokenSource`.
3. It relies on the async workflow to notice the cancellation at the next `let!` /
   `do!` boundary and unwind.

This works correctly for cooperative primitives (`Async.Sleep`, `Async.AwaitTask`
on a cancellable task, etc.).  It silently fails for uncooperative ones.

`sendJsonRpcCall` is:

```fsharp
let sendJsonRpcCall transport method methodParams =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, rc))
```

`MailboxProcessor.PostAndAsyncReply` (FSharp.Core) has two internal code paths:

```fsharp
member x.PostAndAsyncReply(buildMessage, ?timeout) =
    let timeout = defaultArg timeout defaultTimeout    // = Timeout.Infinite
    match timeout with
    | Threading.Timeout.Infinite when not cancellationToken.CanBeCanceled ->
        // FAST PATH — taken here because:
        //   (a) no timeout is passed → Timeout.Infinite
        //   (b) the transport was created with MailboxProcessor.Start(…) — no CT,
        //       so cancellationToken.CanBeCanceled = false
        let resultCell = …
        x.Post(msg)
        resultCell.AwaitResult_NoDirectCancelOrTimeout    // ← name says it all
    | _ ->
        // Slow path — arms a timer and wires cancellation into the result cell
        …
```

Because the transport mailbox is created without a `CancellationToken`:

```fsharp
let transport = MailboxProcessor.Start(eventLoop initialState)   // no CT
```

`PostAndAsyncReply` takes the fast path and returns
`AwaitResult_NoDirectCancelOrTimeout`.  That primitive **does not register a
callback on the ambient cancellation token** — it just waits on the result cell.
When `RunSynchronously`'s timer fires and cancels the token, nothing wakes up the
cell, and the call waits forever.

`Task.Wait(timeout)` avoids the hang differently: the timeout is on the *wait
handle* itself (an OS-level primitive), not on the async work.  After `N` ms the
calling thread is released regardless of whether the task has finished.  The
underlying async keeps running, however — the result cell remains orphaned in
`PendingOutboundCalls` indefinitely.

**`Async.RunSynchronously(timeout)` is not broken** — it faithfully implements
.NET's cooperative-cancellation contract.  The API name is misleading: it looks like
a hard wall-clock guarantee but is actually "request cancellation after N ms and
trust the workflow to cooperate".  For this transport, the workflow does not
cooperate on the fast path, so the timeout is a no-op.

---

## Current workaround in `Tooling.fs`

As an interim fix, `LspTestClient.Request` was changed to use
`Async.StartAsTask` + `Task.Wait()`:

```fsharp
member __.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
    let task =
        sendJsonRpcCall (rpcTransport ()) method (serialize request)
        |> Async.StartAsTask

    task.Wait()

    match task.Result with
    | Ok token  -> token |> deserialize<'Response>
    | Error err -> failwithf "request to method \"%s\" has failed with error: %s" method (string err)
```

This is not idiomatic F# and does not yet add a timeout — it is a stepping-stone
that will be superseded by the design below.

---

## Design: per-call timeout with a single global earliest-deadline timer

### Why not `PostAndAsyncReply`'s built-in `timeout` parameter?

The first instinct is to pass the timeout directly to `PostAndAsyncReply`:

```fsharp
let sendJsonRpcCall transport method methodParams (timeoutMs: int) =
    transport.PostAndAsyncReply(
        (fun rc -> SendCall(method, methodParams, rc)),
        timeout = timeoutMs)
```

`PostAndAsyncReply`'s built-in `timeout` parameter triggers the slow path, which
**does** properly arm a timer, wire cancellation into the result cell, and unblock
the caller cleanly.

However it has a critical problem: when the timeout fires, the `PostAndAsyncReply`
slow path aborts the *caller's wait* but **does not notify the actor**.  The reply
channel entry remains in `PendingOutboundCalls` inside the actor forever — a state
leak that grows with every timed-out call, and that would cause a spurious
late-reply resolution if the server eventually did respond.

### Per-call timeout managed by the actor

Instead of relying on `PostAndAsyncReply`'s built-in mechanism, the timeout value
is carried inside the `SendCall` **event** so that the **actor itself** manages the
deadline:

1. A new `sendJsonRpcCallWithTimeout` accepts a `TimeSpan option` and embeds it in
   the `SendCall` event it posts.  The existing `sendJsonRpcCall` is preserved as a
   thin wrapper that passes `None`, so production call sites that partially apply
   it (e.g. `CSharpLspClient` construction in `Lsp/Server.fs`) compile unchanged.
2. When the actor processes `SendCall`, it records an absolute deadline
   (`DateTimeOffset.UtcNow + timeout`) on the new pending-call entry alongside the
   reply channel and the method name.
3. A single `System.Threading.Timer` instance owned by the actor is armed to fire
   at the **earliest** deadline across all pending calls; when it fires it posts
   `CheckTimeouts` to the actor.
4. The actor handles `CheckTimeouts` by sweeping every entry whose deadline has
   passed: it replies with an error, removes the entry from
   `PendingOutboundCalls`, and rearms the timer for the next earliest remaining
   deadline.
5. Calls without a timeout (`None`) are stored without a deadline — the timer
   simply ignores them, preserving the current infinite-wait behaviour.

### Single `System.Threading.Timer` instance owned by the actor

Rather than allocating a fresh `Async.Sleep` task per reschedule, the actor owns a
single `System.Threading.Timer`.  Reschedules happen via `timer.Change(dueTime,
Timeout.Infinite)` which atomically cancels the pending fire and arms a new one
without allocation.  When the deadline arrives the timer's callback posts
`CheckTimeouts` to the inbox.  The actor sweeps all expired calls in one pass and
calls `rescheduleTimer` again to set up the next fire.

**Advantages over `Async.Start` + `Async.Sleep`:**
- True cancellation: `Change` cancels the previously-armed fire so a superseded
  deadline doesn't generate a stale event in the steady state.
- No per-reschedule allocation — the same `Timer` instance is reused for the
  lifetime of the transport.
- No accumulation of orphaned sleeping tasks waiting to post events the actor will
  discard.
- Deadlines remain explicit, inspectable data on each `PendingCall` entry (stored
  as `DateTimeOffset`), so the timer is purely a wakeup mechanism — all decision
  logic stays in the actor.

**Reschedule-during-callback race:**

`Timer.Change` cancels the *pending* fire but cannot pre-empt a callback that is
already executing on a thread-pool thread.  In that narrow window the actor may
receive a `CheckTimeouts` whose deadline has already been superseded by a newer
reschedule.  This is harmless: `CheckTimeouts` is **idempotent** — its work is
`Map.partition` over `DateTimeOffset.UtcNow`, so a spurious wakeup with no
expired entries simply reschedules and returns.  No generation counter is needed.

### Late responses after a timeout

When `CheckTimeouts` fires first, the entry is already removed before the late
response arrives.  Because the entry is gone, `handleInboundResponse` no longer has
the per-call context — it only knows the ID is unknown.

To distinguish an innocent late response from a genuine protocol violation, track
recently timed-out IDs in a separate field on the state:

```fsharp
RecentlyTimedOutCalls: Map<int, DateTimeOffset>   // id → instant the timeout fired
```

Entries are added when `CheckTimeouts` removes a pending call, and pruned on every
`CheckTimeouts` sweep using a 60 s TTL.  60 s is long enough that any realistic
late response would have arrived, short enough that the map stays bounded.

`handleInboundResponse` then has three cases:

| Case | Log level | Message |
|---|---|---|
| ID found in `PendingOutboundCalls` | — | Normal path; resolve the call |
| ID found in `RecentlyTimedOutCalls` | `RpcWarn` | `"Received late response for call id={id} — already timed out; response discarded"` |
| ID not found in either | `RpcError` | `"Received response for unknown call id={id}; response discarded"` (genuine protocol violation) |

### Alignment with existing shutdown behaviour

When the transport shuts down or detects EOF it already fails all pending calls
via `failPendingOutboundCalls`:

```fsharp
let shutdownErr = makeError -32099 "Transport shut down" :> JToken
for KeyValue(id, replyChannel) in state.PendingOutboundCalls do
    postEvent (WriteRpcLogEntry(
        RpcWarn(sprintf "failPendingOutboundCalls: failing pending outbound call id=%d on shutdown" id)))
    replyChannel.Reply(Error shutdownErr)
```

After Step 1 (below), `PendingOutboundCalls` becomes `Map<int, PendingCall>`, so
this loop is updated to dereference `pending.ReplyChannel`.

A timed-out call surfaces to callers in the same shape — an `Error` result with a
well-known code — so callers do not need to handle a new exception type.  The
proposed error code is `-32000` ("Call timed out") which falls in the
implementation-defined error range reserved by the JSON-RPC spec.

---

## Proposed implementation

### Step 1 — Enrich `PendingOutboundCalls` entries; add timer fields to state

Replace `PendingOutboundCalls: Map<int, AsyncReplyChannel<…>>` with a richer entry
that carries the **method name** (for logging), the original **timeout duration**
(also for logging), and the absolute **deadline** (for scheduling):

```fsharp
type PendingCall =
    { Method: string
      Timeout: TimeSpan option           // None ⇒ infinite-wait, no deadline tracked
      Deadline: DateTimeOffset option    // Some only when Timeout is Some
      ReplyChannel: AsyncReplyChannel<Result<JToken, JToken>> }
```

`Timeout` and `Deadline` are `option` fields so a single map shape covers both
timed and untimed calls.  Untimed calls are filtered out by the timer logic via
`Seq.choose _.Deadline`.

Update the state type:

```fsharp
type JsonRpcTransportState =
    { …
      PendingOutboundCalls: Map<int, PendingCall>
      Timer: System.Threading.Timer option                // single shared wakeup timer
      RecentlyTimedOutCalls: Map<int, DateTimeOffset> }   // id → timeout instant; 60 s TTL
```

Update `emptyTransportState` accordingly (`Timer = None`,
`RecentlyTimedOutCalls = Map.empty`).  The `Timer` is created lazily on first
arming and reused for the lifetime of the transport.

Update every existing site that touches `PendingOutboundCalls`:

- `failPendingOutboundCalls` — replace the `for KeyValue(id, replyChannel)` loop
  with `for KeyValue(id, pending) in …` and call `pending.ReplyChannel.Reply(...)`.
- `handleInboundResponse` — `Map.tryFind idVal` now returns `PendingCall option`;
  reply via `pending.ReplyChannel`.  This function also gains the three-case
  handling described in Step 4a.
- `SendCall` branch in `processEvent` — see Step 3.

The `Timer` itself is disposed and cleared on every shutdown path:

- `Shutdown rc` and the EOF branch of `InboundMessage` (the two places that
  currently clear `PendingOutboundCalls = Map.empty` and move to `ShuttingDown`)
  must additionally `state.Timer |> Option.iter _.Dispose()` and set
  `Timer = None` in the new state.

After dispose, any callback already in flight will still post one final
`CheckTimeouts` to the actor.  The phase guard in Step 4 (`Active` only) means
this stale wakeup is dropped harmlessly.

### Step 2 — Add `callTimeout` to the `SendCall` event and a new send function

Extend `SendCall`:

```fsharp
| SendCall of method: string
            * methodParams: JToken
            * callTimeout: TimeSpan option
            * AsyncReplyChannel<Result<JToken, JToken>>
```

Add a new send function and keep the existing `sendJsonRpcCall` as a thin wrapper
that passes `None`.  This preserves the partial-application call sites in
`Lsp/Server.fs` that feed `sendJsonRpcCall rpcTransport` into `CSharpLspClient`
(which expects `string -> JToken -> Async<Result<JToken, JToken>>`):

```fsharp
let sendJsonRpcCallWithTimeout
        (transport: MailboxProcessor<JsonRpcTransportEvent>)
        (method: string)
        (methodParams: JToken)
        (callTimeout: TimeSpan option) =
    transport.PostAndAsyncReply(fun rc ->
        SendCall(method, methodParams, callTimeout, rc))

let sendJsonRpcCall transport method methodParams =
    sendJsonRpcCallWithTimeout transport method methodParams None
```

Add `CheckTimeouts` (no payload) to `JsonRpcTransportEvent`.

### Step 3 — `rescheduleTimer` helper and `SendCall` integration

`rescheduleTimer` is called whenever `PendingOutboundCalls` changes (call added,
call resolved normally, call timed out).  It finds the minimum deadline across all
pending calls that have one and arms the shared `Timer`, creating it on first use
and reusing it via `Change` thereafter:

```fsharp
let rescheduleTimer (state: JsonRpcTransportState) (inbox: MailboxProcessor<_>) =
    let nextDeadline =
        state.PendingOutboundCalls
        |> Map.values
        |> Seq.choose _.Deadline
        |> Seq.sortBy id
        |> Seq.tryHead

    match nextDeadline, state.Timer with
    | None, Some t ->
        // No deadlines remain — disarm but keep the Timer for future reuse.
        t.Change(Timeout.Infinite, Timeout.Infinite) |> ignore
        state
    | None, None ->
        state
    | Some deadline, existing ->
        let delayMs = max 0 (int (deadline - DateTimeOffset.UtcNow).TotalMilliseconds)
        match existing with
        | Some t ->
            // Reuse: Change atomically cancels the prior fire and arms the new one.
            t.Change(delayMs, Timeout.Infinite) |> ignore
            state
        | None ->
            // First-ever arming: create the Timer with a stable callback that
            // simply posts CheckTimeouts. The callback is never replaced.
            let cb = TimerCallback(fun _ -> inbox.Post CheckTimeouts)
            let t = new Timer(cb, null, delayMs, Timeout.Infinite)
            { state with Timer = Some t }
```

Note that the callback closes over `inbox` only.  It carries no per-fire data, so
once the `Timer` exists we never need to recreate it — every reschedule is just
`Change`.

When `SendCall` is processed, build the `PendingCall`, add it to the map, then
call `rescheduleTimer`:

```fsharp
| SendCall(method, methodParams, callTimeout, replyChannel) ->
    match state.Phase with
    | ShuttingDown | Stopped ->
        postEvent (WriteRpcLogEntry(
            RpcWarn(sprintf "SendCall: dropping '%s', transport is shutting down" method)))
        replyChannel.Reply(Error(makeError -32099 "Transport shut down" :> JToken))
        state
    | Active ->
        let id = state.NextOutboundRequestId
        let msg =
            JObject(
                JProperty("jsonrpc", "2.0"),
                JProperty("id", id),
                JProperty("method", method),
                JProperty("params", methodParams))
        let pending =
            { Method       = method
              Timeout      = callTimeout
              Deadline     = callTimeout |> Option.map (fun t -> DateTimeOffset.UtcNow + t)
              ReplyChannel = replyChannel }
        let newState =
            { state with
                NextOutboundRequestId = id + 1
                PendingOutboundCalls  = state.PendingOutboundCalls |> Map.add id pending }
        let newState = enqueueOutboundMessage postEvent newState { Payload = msg; CompletionRC = None }
        rescheduleTimer newState inbox
```

`rescheduleTimer` is a no-op when no entry has a `Deadline`, so untimed-only
workloads pay zero overhead.

### Step 4 — Handle `CheckTimeouts` in `processEvent`

```fsharp
| CheckTimeouts ->
    match state.Phase with
    | ShuttingDown | Stopped ->
        // Timer callback raced with shutdown disposal — ignore.
        state
    | Active ->
        let now = DateTimeOffset.UtcNow

        // Partition expired vs. live entries.
        let expired, live =
            state.PendingOutboundCalls
            |> Map.partition (fun _ p ->
                p.Deadline |> Option.exists (fun d -> d <= now))

        // Fail every expired call.
        for KeyValue(id, pending) in expired do
            let timeoutDesc =
                pending.Timeout
                |> Option.map string
                |> Option.defaultValue "<no timeout>"
            postEvent (WriteRpcLogEntry(
                RpcWarn(sprintf "CheckTimeouts: call id=%d method=%s timed out after %s"
                                id pending.Method timeoutDesc)))
            pending.ReplyChannel.Reply(Error(makeError -32000 "Call timed out" :> JToken))

        // Stamp expired IDs into RecentlyTimedOutCalls and prune entries older than 60 s.
        let ttl = TimeSpan.FromSeconds 60.0
        let stamped =
            (state.RecentlyTimedOutCalls, expired)
            ||> Map.fold (fun acc id _ -> acc |> Map.add id now)
        let pruned =
            stamped |> Map.filter (fun _ stampedAt -> now - stampedAt <= ttl)

        let state' =
            { state with
                PendingOutboundCalls  = live
                RecentlyTimedOutCalls = pruned }

        rescheduleTimer state' inbox
```

**Idempotence covers the reschedule-during-callback race.** If `rescheduleTimer`
calls `Change` while the previous fire's callback is already running, the actor
will receive one more `CheckTimeouts` whose deadline has already been moved.  The
`Map.partition` simply finds nothing expired; the function reschedules and
returns.  No special guard is needed.

**Why sweep all expired entries at once:** if two calls have nearly identical
deadlines, the timer wakes for the earlier one; by the time the actor processes
`CheckTimeouts`, the second call's deadline may already have passed too.
Sweeping in one pass handles both cleanly without scheduling a second timer that
would fire in microseconds.

### Step 4a — Three-case `handleInboundResponse`

Replace the current single-arm "unknown id" branch with explicit case handling:

```fsharp
let handleInboundResponse state postEvent (responseId: JToken) (msg: JObject) =
    let idVal = int responseId

    match state.PendingOutboundCalls |> Map.tryFind idVal with
    | Some pending ->
        match msg.SelectToken("error") |> Option.ofObj with
        | Some err -> pending.ReplyChannel.Reply(Error err)
        | None ->
            let result =
                msg.SelectToken("result")
                |> Option.ofObj
                |> Option.defaultWith (fun () -> JValue.CreateNull() :> JToken)
            pending.ReplyChannel.Reply(Ok result)

        { state with
            PendingOutboundCalls = state.PendingOutboundCalls |> Map.remove idVal }

    | None ->
        if state.RecentlyTimedOutCalls |> Map.containsKey idVal then
            postEvent (WriteRpcLogEntry(
                RpcWarn(sprintf "handleInboundResponse: late response for call id=%d — already timed out; response discarded" idVal)))
        else
            postEvent (WriteRpcLogEntry(
                RpcError(sprintf "handleInboundResponse: response for unknown call id=%d; response discarded" idVal)))

        state
```

Late-response IDs remain in `RecentlyTimedOutCalls` for up to 60 s after their
timeout fired, so any realistic delayed reply will hit the `RpcWarn` branch rather
than the `RpcError` branch.

### Step 5 — Update `Tooling.fs` to pass a per-call timeout

`startJsonRpcTransport` requires no changes.  `LspTestClient.Request` switches to
`sendJsonRpcCallWithTimeout` (e.g. 30 s) and reverts to idiomatic
`Async.RunSynchronously`:

```fsharp
member __.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
    let result =
        sendJsonRpcCallWithTimeout
            (rpcTransport ())
            method
            (serialize request)
            (Some (TimeSpan.FromSeconds 30.0))
        |> Async.RunSynchronously

    match result with
    | Ok token  -> token |> deserialize<'Response>
    | Error err -> failwithf "request to method \"%s\" has failed with error: %s" method (string err)
```

Because the actor now fails the call after 30 s, the async returned by
`sendJsonRpcCallWithTimeout` completes naturally — `Async.RunSynchronously`
returns cleanly without needing to race against a timeout itself.  Individual
test call sites that need a longer (or no) timeout can pass a different `Some _`
or `None`.

### Step 6 — Add/update tests

- **Single-call timeout (JsonRpcTests.fs):** call
  `sendJsonRpcCallWithTimeout transport "test/hang" (JObject()) (Some (TimeSpan.FromMilliseconds 200.0))`
  against a peer that never replies.  Assert the call returns `Error(-32000, "Call timed out")` within ~500 ms.
- **No leak after timeout:** after the previous call returns, run a second call
  that *does* receive a reply and assert it returns `Ok` — proves the actor is
  still healthy and the timed-out entry was cleaned up.
- **Reply-before-deadline:** call with timeout 5 s, peer replies after 50 ms;
  assert `Ok`.  The actor's `Timer.Change` to disarm/rearm is exercised; any
  `CheckTimeouts` that races through finds nothing expired and is a no-op.
- **Two concurrent timeouts in one sweep:** schedule two calls with nearly equal
  deadlines (e.g. 200 ms and 220 ms); assert both fail in a single sweep.
- **New earlier deadline arrives mid-arm:** schedule a 5 s call, then a 100 ms
  call; assert the 100 ms one times out at ~100 ms (not after 5 s) — proves
  `Change` actually re-arms the existing `Timer`.
- **Shutdown disposes the Timer:** start a transport with a long-deadline call,
  then `shutdownJsonRpcTransport`; assert the call returns `Error(-32099)` and
  no `CheckTimeouts` work runs after the phase moves to `ShuttingDown`/`Stopped`
  (verified by the phase guard in Step 4).
- **Late response after timeout:** call with 100 ms timeout, peer replies after
  300 ms; assert the call returned `Error(-32000)`, and that the test's
  rpc-log-callback received the `RpcWarn` "late response" entry but no `RpcError`.
- **Untimed call still works:** existing `sendJsonRpcCall` (no timeout) passes
  `None`; verify infinite-wait behaviour against an unresponsive peer is preserved
  (use a short test-side `Task.Wait(timeout)` to bound the test itself, then
  assert the call has *not* completed).
- **Existing tests:** all pass unchanged — `sendJsonRpcCall` without a timeout
  preserves the prior behaviour.

---

## Open questions

| # | Question | Resolution |
|---|---|---|
| 1 | What timeout value should `LspTestClient.Request` use by default? | 30 s.  Conservative starting point; call sites that need more can wrap their own helper. |
| 2 | What TTL for `RecentlyTimedOutCalls`? | **60 s, pruned on every `CheckTimeouts` sweep.**  Long enough that realistic late replies land in the `RpcWarn` branch, short enough that the map stays bounded. |
| 3 | Should `sendJsonRpcNotification` get a timeout? | No.  Its reply channel only signals that the actor has *enqueued* the outbound message, which is near-instant. |

---

## Testing checklist

- [ ] Single timed call, no response → `Error(-32000)` returned at deadline; `PendingOutboundCalls` empty afterwards
- [ ] Single timed call, response arrives before deadline → `Ok` returned; `Timer` disarmed via `Change(Infinite, Infinite)`
- [ ] Two concurrent calls with different timeouts → earlier one times out first; `Timer.Change` rearms; later one times out at its own deadline
- [ ] Two calls whose deadlines fall within the same `CheckTimeouts` sweep → both expire in one pass; only one timer fire
- [ ] New call with earlier deadline arrives while timer is armed → `Timer.Change` rearms for the new minimum; new call times out at correct time
- [ ] Actor remains healthy after one or more timeouts (a subsequent call succeeds)
- [ ] `sendJsonRpcCall` (no timeout) → `Timer` is never armed; infinite-wait behaviour preserved; all existing tests pass
- [ ] `Lsp/Server.fs` partial-application sites (`sendJsonRpcCall rpcTransport` fed to `CSharpLspClient`) compile unchanged
- [ ] `CheckTimeouts` racing with `Change` (callback already running) → idempotent sweep finds nothing; no state mutation
- [ ] `CheckTimeouts` arriving after `ShuttingDown`/`Stopped` → phase guard drops it; no late replies issued
- [ ] Timeout fires → `RpcWarn` log contains method name and original timeout duration
- [ ] Late response after timeout → ID found in `RecentlyTimedOutCalls` → `RpcWarn` emitted; no crash, no double-reply; actor still healthy
- [ ] Response for genuinely unknown ID (no timeout ever configured for that ID) → `RpcError` emitted
- [ ] `RecentlyTimedOutCalls` pruned to < 60 s of entries on every sweep
- [ ] Shutdown / EOF disposes the `Timer` and clears the field
- [ ] `LspTestClient.Request` reverted to `Async.RunSynchronously` (no `Task.Wait`)

## Out of scope

- Timeout for inbound requests (handlers that take too long on the server side) —
  a separate concern handled by the request scheduler.
- A `--timeout` CLI flag for the production server binary — callers that want a
  default timeout can wrap `sendJsonRpcCallWithTimeout` at a higher layer.
