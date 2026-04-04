# JsonRpc.fs

An F# implementation of a [JSON-RPC 2.0](https://www.jsonrpc.org/specification) transport layer over stdio, designed for use in language servers and similar tools that communicate via the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

## How it works

The transport runs as a `MailboxProcessor` actor. All I/O and state transitions go through this single actor, keeping state mutation serialised without manual locking. Inbound request and notification handlers run on the thread pool and report back to the actor via events when complete.

Messages are framed using HTTP-style `Content-Length` headers, as required by LSP.

Outbound messages are serialised through an internal write queue: if a write is already in progress when a new message is enqueued, the message waits rather than racing. This means delivery order matches enqueue order.

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

## Receiving notifications

Register an async function of type `JsonRpcNotificationHandler` per method name. No response is sent.

```fsharp
let handler : JsonRpcNotificationHandler = fun ctx -> async {
    doSomethingWith ctx.Params
}
```

## Sending notifications

```fsharp
do! sendJsonRpcNotification transport "window/showMessage" (serialize payload)
```

Returns `Async<unit>` that completes once the message is queued for writing.

## Sending requests (outbound calls)

```fsharp
let! result = sendJsonRpcCall transport "client/registerCapability" (serialize payload)

match result with
| Ok value  -> // use value : JToken
| Error err -> // handle err : JToken
```

Returns `Async<Result<JToken, JToken>>` that completes when the peer's response arrives.

## Waiting for shutdown

```fsharp
do! awaitJsonRpcTransportShutdown transport
```

Blocks until the transport detects EOF on stdin or receives a `Shutdown` event. Use this as the main wait at the end of your server's entry point.

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

| Code    | Meaning           | When sent                                    |
|---------|-------------------|----------------------------------------------|
| -32601  | Method not found  | Inbound request for an unregistered method   |
| -32603  | Internal error    | Inbound handler threw an unhandled exception |
| -32800  | Request cancelled | Inbound handler was cancelled                |
