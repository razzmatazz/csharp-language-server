# JsonRpc.fs

An F# implementation of a [JSON-RPC 2.0](https://www.jsonrpc.org/specification) transport layer over stdio for use with the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). It is symmetric by design and can be used on **either end** of an LSP connection — as the language server receiving requests from an editor, or as the client (e.g. in tests or a driver process) sending requests to a server.

## How it works

The transport runs as a `MailboxProcessor` actor. All I/O and state transitions go through this single actor, keeping state mutation serialised without manual locking. Inbound request and notification handlers run on the thread pool and report back to the actor via events when complete.

Messages are framed using HTTP-style `Content-Length` headers, as required by LSP.

Outbound messages are serialised through an internal write queue: if a write is already in progress when a new message is enqueued, the message waits rather than racing. This means delivery order matches enqueue order.

If a transient error occurs while reading from stdin (e.g. a stream exception or malformed JSON), the transport logs a `RpcError` entry and retries the next read. A read error does not shut down the transport — EOF is the canonical signal for that.

### Transport lifecycle

The transport moves through three phases, tracked by the `TransportPhase` discriminated union:

| Phase | Meaning |
|---|---|
| `Active` | Normal operation — reading, writing, dispatching handlers. |
| `ShuttingDown` | `Shutdown` received or EOF on stdin; stdin closed, handler cancellations fired, waiting for all in-flight handlers to finish. |
| `Stopped` | All handlers have drained; every `ShutdownWaiters` channel has been notified. |

In `ShuttingDown` and `Stopped`, any new `sendJsonRpcNotification` or `sendJsonRpcCall` is rejected immediately — the caller is unblocked with a failure response and a `RpcWarn` log entry is emitted. This prevents callers from blocking indefinitely or leaking reply channels into `PendingOutboundCalls` after the peer has gone away.

## Starting the transport

```fsharp
let transport =
    startJsonRpcTransport stdin stdout rpcLogCallback (fun transport ->
        let callHandlers =
            Map.ofList [
                "myMethod", (fun ctx -> async {
                    // ctx.Params contains the request params as JToken option
                    return Ok (JValue("hello") :> JToken)
                })
            ]

        let notificationHandlers =
            Map.ofList [
                "myNotification", (fun ctx -> async {
                    // fire-and-forget, no response needed
                    ()
                })
            ]

        callHandlers, notificationHandlers
    )
```

The `configure` callback receives the transport actor before any messages are processed, so it is safe to capture it (e.g. to call `sendJsonRpcNotification` or `sendJsonRpcCall` from within handlers).

The `rpcLogCallback` is invoked on every `JsonRpcLogEntry` — reads, writes, errors, warnings, and debug messages. Pass `None` to disable logging. You can combine multiple callbacks by composing them into one:

```fsharp
let combined =
    Some (fun entry ->
        fileCallback entry
        loggerCallback entry)
```

## Receiving requests

Register an async function of type `JsonRpcCallHandler` per method name. The function receives a `JsonRpcRequestContext` and must return `Async<Result<JToken, JToken>>` — `Ok` for a successful response, `Error` for a JSON-RPC error object.

The context includes a `RequestOrdinal` — a monotonically increasing `int64` assigned to each inbound message (both requests and notifications) in the order it was received. This can be used for logging or to reason about ordering.

```fsharp
let handler : JsonRpcCallHandler = fun ctx -> async {
    let paramValue =
        ctx.Params
        |> Option.map (fun p -> p.ToObject<MyParams>())

    match paramValue with
    | Some p -> return Ok (serialize (doSomething p))
    | None   -> return Error (makeError -32602 "Invalid params")
}
```

Handlers support **cancellation** — if the client sends a `$/cancelRequest` notification, the handler's `CancellationTokenSource` is cancelled. The transport automatically sends a `-32800` error response if cancellation is observed.

If a handler throws an **unhandled exception**, the transport catches it and automatically sends a `-32603` (Internal error) response to the client. The exception type name is included in the error message. A `RpcError` log entry is emitted.

## Receiving notifications

Register an async function of type `JsonRpcNotificationHandler` per method name. No response is sent.

```fsharp
let handler : JsonRpcNotificationHandler = fun ctx -> async {
    doSomethingWith ctx.Params
}
```

If an inbound notification arrives for a method with no registered handler, it is silently dropped and a `RpcError` log entry is emitted. No response is sent (per the JSON-RPC spec, notifications never receive responses).

If a notification handler throws an unhandled exception, a `RpcError` log entry is emitted. No response is sent.

## Sending notifications

```fsharp
do! sendJsonRpcNotification transport "window/showMessage" (serialize payload)
```

Returns `Async<unit>` that completes once the message has been written and flushed to the output stream (not just enqueued).

## Sending requests (outbound calls)

```fsharp
let! result = sendJsonRpcCall transport "client/registerCapability" (serialize payload)

match result with
| Ok value  -> // use value : JToken
| Error err -> // handle err : JToken
```

Returns `Async<Result<JToken, JToken>>` that completes when the peer's response arrives.

## Shutdown sequence

Both an explicit `shutdownJsonRpcTransport` call and EOF on stdin trigger the same shutdown sequence:

1. All pending outbound calls are failed immediately with error `-32099`.
2. A `CancelRequest` event is posted for every entry in `RunningInboundRequests`, which cancels each handler's `CancellationTokenSource`.
3. `StdIn` is cleared and the phase moves to `ShuttingDown`.
4. As each handler posts `HandlerCancelled`, `HandlerCompleted`, or `HandlerFailed` back to the actor, it is removed from `RunningInboundRequests`.
5. Once the map is empty, all parked waiter channels are notified and the phase moves to `Stopped`.

If there are no running handlers at the moment shutdown begins, steps 4–5 happen immediately.

**Pending outbound calls are failed immediately.** Any `sendJsonRpcCall` that is still awaiting a response will be unblocked with an `Error` result carrying code `-32099` and the message `"Transport shut down"`. This prevents callers from hanging indefinitely when the peer disappears.

**Late `sendJsonRpc*` calls are rejected.** Once the phase is `ShuttingDown` or `Stopped`, any new `sendJsonRpcNotification` or `sendJsonRpcCall` is dropped: the notification caller is unblocked immediately, and the call caller receives the same `-32099` error. A `RpcWarn` log entry is emitted in both cases.

### Explicit shutdown

```fsharp
do! shutdownJsonRpcTransport transport
```

The call **does not return until all in-flight inbound handlers have finished** — it parks a reply channel that is fired at step 5 above.

### EOF on stdin

When the transport reads EOF (e.g. the peer process exits or the pipe is closed), the same sequence runs automatically. The only difference is that there is no caller reply channel — use `awaitJsonRpcTransportShutdown` if you need to wait for the drain to complete after EOF.

## Waiting for shutdown

```fsharp
do! awaitJsonRpcTransportShutdown transport
```

Parks a waiter that is notified when the transport reaches the `Stopped` phase — i.e. after the shutdown sequence (triggered by either an explicit `shutdownJsonRpcTransport` call or EOF on stdin) has drained all in-flight handlers. Use this as the main wait at the end of your server's entry point.

## Testing

Because the transport accepts arbitrary `Stream` values, it can be exercised in unit tests without spawning a process. Pass a pair of in-memory streams (e.g. `System.IO.Pipelines` or linked `MemoryStream`s) in place of stdin/stdout:

```fsharp
let clientToServer = new System.IO.MemoryStream()
let serverToClient = new System.IO.MemoryStream()

let transport =
    startJsonRpcTransport clientToServer serverToClient None (fun _ ->
        Map.ofList [ "ping", pingHandler ],
        Map.empty
    )
```

Write well-formed `Content-Length`-framed JSON into the input stream and read responses from the output stream to verify handler behaviour end-to-end.

## Error codes

| Code    | Meaning             | When sent                                                                                         |
|---------|---------------------|---------------------------------------------------------------------------------------------------|
| -32099  | Transport shut down | Pending `sendJsonRpcCall` on shutdown or stdin EOF; late `sendJsonRpcCall` in `ShuttingDown`/`Stopped` |
| -32601  | Method not found    | Inbound request for an unregistered method                                                        |
| -32603  | Internal error      | Inbound handler threw an unhandled exception                                                      |
| -32800  | Request cancelled   | Inbound handler was cancelled via `$/cancelRequest`, transport shutdown, or EOF on stdin           |
