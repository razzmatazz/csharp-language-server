# JsonRpc.fs

An F# implementation of a [JSON-RPC 2.0](https://www.jsonrpc.org/specification) transport layer over stdio for use with the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). It is symmetric by design and can be used on **either end** of an LSP connection — as the language server receiving requests from an editor, or as the client (e.g. in tests or a driver process) sending requests to a server.

> **JSON-RPC version:** This implementation is **JSON-RPC 2.0 only**. JSON-RPC 1.0 messages (which lack the `jsonrpc` field and use `"id": null` for notifications) are not supported. This is intentional — LSP mandates JSON-RPC 2.0 exclusively.

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
                    // ctx.Params contains the request params as JsonElement option
                    let node = System.Text.Json.Nodes.JsonObject()
                    node["result"] <- System.Text.Json.Nodes.JsonValue.Create("hello")
                    return Ok (System.Text.Json.JsonSerializer.SerializeToElement(node))
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

The `rpcLogCallback` is invoked on every `JsonRpcLogEntry` — reads, writes, errors, warnings, and debug messages. Pass `None` to disable logging.

The DU cases are:

| Case | Payload | When emitted |
|---|---|---|
| `RpcRead` | `string` — raw JSON text of the inbound message | Every successfully parsed inbound message |
| `RpcWrite` | `string` — raw JSON text of the outbound message | Every message written to stdout |
| `RpcError` | `string` — human-readable description | Protocol violations, handler exceptions, unknown response IDs |
| `RpcWarn` | `string` — human-readable description | Late responses after timeout, dropped post-shutdown sends |
| `RpcDebug` | `string` — human-readable description | Verbose diagnostic events |

You can combine multiple callbacks by composing them into one:

```fsharp
let combined =
    Some (fun entry ->
        fileCallback entry
        loggerCallback entry)
```

## Version validation

Every inbound message must carry `"jsonrpc": "2.0"`. Messages that fail this check are rejected before any dispatch:

| Message shape | Behaviour |
|---|---|
| Has `id` (request-shaped) | Replied to immediately with `-32600 Invalid Request`, echoing the `id` from the message so the caller can correlate the error. No handler is invoked. |
| No `id` (notification-shaped) | Silently dropped. A `RpcError` log entry is emitted. No response is sent. |

This applies to both a missing `jsonrpc` field and a wrong value (e.g. `"1.0"`).

## Receiving requests

Register an async function of type `JsonRpcCallHandler` per method name. The function receives a `JsonRpcRequestContext` and must return `Async<Result<JsonElement, JsonElement>>` — `Ok` for a successful response, `Error` for a JSON-RPC error object.

The context includes a `RequestOrdinal` — a monotonically increasing `int64` assigned to each inbound message (both requests and notifications) in the order it was received. This can be used for logging or to reason about ordering.

`ctx.Params` and `ctx.WireId` are `System.Text.Json.JsonElement option`. Use standard `JsonElement` navigation methods — `.GetProperty(...)`, `.TryGetProperty(...)`, `.GetString()`, `.GetInt32()`, etc. — to inspect them.

```fsharp
open System.Text.Json
open System.Text.Json.Nodes

let handler : JsonRpcCallHandler = fun ctx -> async {
    match ctx.Params with
    | Some p ->
        let name = p.GetProperty("name").GetString()
        let result = JsonObject()
        result["greeting"] <- JsonValue.Create($"hello {name}")
        return Ok (JsonSerializer.SerializeToElement(result))
    | None ->
        let err = JsonObject()
        err["code"] <- JsonValue.Create(-32602)
        err["message"] <- JsonValue.Create("Invalid params")
        return Error (JsonSerializer.SerializeToElement(err))
}
```

Handlers support **cancellation** — if the client sends a `$/cancelRequest` notification, the handler's `CancellationTokenSource` is cancelled. The transport automatically sends a `-32800` error response if cancellation is observed.

If a handler throws an **unhandled exception**, the transport catches it and automatically sends a `-32603` (Internal error) response to the client. The exception type name is included in the error message. A `RpcError` log entry is emitted.

## Receiving notifications

Register an async function of type `JsonRpcNotificationHandler` per method name. No response is sent.

```fsharp
let handler : JsonRpcNotificationHandler = fun ctx -> async {
    ctx.Params |> Option.iter (fun p -> doSomethingWith p)
}
```

If an inbound notification arrives for a method with no registered handler, it is silently dropped and a `RpcError` log entry is emitted. No response is sent (per the JSON-RPC spec, notifications never receive responses).

If a notification handler throws an unhandled exception, a `RpcError` log entry is emitted. No response is sent.

## Sending notifications

```fsharp
let body = JsonObject()
body["message"] <- JsonValue.Create("hello")
do! sendJsonRpcNotification transport "window/showMessage" (JsonSerializer.SerializeToElement(body))
```

The params argument is `JsonElement`. Build it with `System.Text.Json.Nodes.JsonObject` / `JsonValue`, or use `JsonSerializer.SerializeToElement` to convert any serialisable value.

Returns `Async<unit>` that completes once the message has been written and flushed to the output stream (not just enqueued).

## Sending requests (outbound calls)

```fsharp
let! result =
    sendJsonRpcCall transport "client/registerCapability"
        (JsonSerializer.SerializeToElement(payload))

match result with
| Ok value  -> // value : JsonElement — use .GetProperty(...), .GetString(), etc.
| Error err -> // err   : JsonElement — e.g. err.GetProperty("code").GetInt32()
```

The params argument is `JsonElement`. Returns `Async<Result<JsonElement, JsonElement>>` that completes when the peer's response arrives.

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

## Inspecting runtime state (`getJsonRpcStats`)

```fsharp
let! stats = getJsonRpcStats transport
```

Sends a `GetRpcStats` event to the actor and returns a `JsonRpcStats` snapshot assembled from the current state, without blocking any other processing. The record contains:

| Field | Type | Meaning |
|---|---|---|
| `Phase` | `string` | Current `TransportPhase` as a string — `"Active"`, `"ShuttingDown"`, or `"Stopped"`. |
| `WriteQueueLength` | `int` | Number of outbound messages queued behind the one currently being written. |
| `PendingOutboundCallCount` | `int` | Number of `sendJsonRpcCall` calls awaiting a response from the peer. |
| `RunningInboundRequestCount` | `int` | Number of inbound request handlers currently executing on the thread pool. |
| `TimerArmed` | `bool` | Whether the shared deadline timer is currently armed (i.e. at least one timed outbound call is pending). |
| `RecentlyTimedOutCallCount` | `int` | Number of call IDs in the 60-second grace window used to distinguish late responses from genuine unknown-ID protocol violations. |

This is intended for diagnostics and debug dumps rather than operational control. The snapshot is consistent with the actor's state at the moment the event is processed, but may be stale by the time the caller acts on it.

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
| -32600  | Invalid Request     | Inbound message with a missing or non-`"2.0"` `jsonrpc` field (request-shaped, i.e. has `id`)    |
| -32099  | Transport shut down | Pending `sendJsonRpcCall` on shutdown or stdin EOF; late `sendJsonRpcCall` in `ShuttingDown`/`Stopped` |
| -32601  | Method not found    | Inbound request for an unregistered method                                                        |
| -32603  | Internal error      | Inbound handler threw an unhandled exception                                                      |
| -32800  | Request cancelled   | Inbound handler was cancelled via `$/cancelRequest`, transport shutdown, or EOF on stdin           |
