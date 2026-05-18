/// JSON-RPC 2.0 transport implementation for the Language Server Protocol.
/// This module is JSON-RPC 2.0 only — JSON-RPC 1.0 messages are not supported.
/// LSP mandates JSON-RPC 2.0 exclusively, so no 1.0 compatibility is needed or provided.
module CSharpLanguageServer.Runtime.JsonRpc

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading

type JsonRpcLogEntry =
    | RpcRead of JsonElement
    | RpcWrite of JsonElement
    | RpcError of string
    | RpcWarn of string
    | RpcDebug of string

/// A point-in-time snapshot of transport internals, assembled on demand.
type JsonRpcStats =
    { Phase: string
      WriteQueueLength: int
      PendingOutboundCallCount: int
      RunningInboundRequestCount: int
      TimerArmed: bool
      RecentlyTimedOutCallCount: int }

type JsonRpcRequestContext =
    { MethodName: string
      RequestOrdinal: int64
      WireId: JsonElement option
      Params: JsonElement option
      CancellationTokenSource: CancellationTokenSource option }

type JsonRpcCallHandler = JsonRpcRequestContext -> Async<Result<JsonElement, JsonElement>>

type JsonRpcCallHandlerMap = Map<string, JsonRpcCallHandler>

type JsonRpcNotificationHandler = JsonRpcRequestContext -> Async<unit>

type JsonRpcNotificationHandlerMap = Map<string, JsonRpcNotificationHandler>

type OutboundMessage =
    { Payload: JsonDocument
      CompletionRC: option<AsyncReplyChannel<unit>> }

type TransportPhase =
    | Active
    | ShuttingDown // Shutdown received, handler cancellations fired, waiting for RunningInboundRequests to drain
    | Stopped // all handlers drained, ShutdownWaiters fired

type PendingCall =
    { Method: string
      Timeout: TimeSpan option // None ⇒ infinite-wait, no deadline tracked
      Deadline: DateTimeOffset option // Some only when Timeout is Some
      ReplyChannel: AsyncReplyChannel<Result<JsonElement, JsonElement>> }

type JsonRpcTransportState =
    {
        StdIn: Stream option
        StdOut: Stream option
        PendingRead: Async<unit> option
        PendingWrite: Async<unit> option
        WriteQueue: OutboundMessage list
        CallHandlers: JsonRpcCallHandlerMap
        NotificationHandlers: JsonRpcNotificationHandlerMap
        NextOutboundRequestId: int
        NextInboundRequestOrdinal: int64
        /// Outbound calls awaiting a response, keyed by the request ID we assigned.
        PendingOutboundCalls: Map<int, PendingCall>
        /// Inbound requests currently being handled, keyed by wire ID (string representation).
        RunningInboundRequests: Map<string, JsonRpcRequestContext>
        /// Channels waiting to be notified when the transport shuts down.
        ShutdownWaiters: AsyncReplyChannel<unit> list
        /// Optional callback invoked with each raw RPC log entry. Callback should .Clone() JsonElement if it needs to store it.
        RpcLogEntryCallback: (JsonRpcLogEntry -> unit) option
        Phase: TransportPhase
        /// Single shared wakeup timer, created lazily on first armed deadline, reused via Change().
        Timer: System.Threading.Timer option
        /// Recently timed-out call IDs kept for 60 s so late responses can be distinguished
        /// from genuine unknown-ID protocol violations.
        RecentlyTimedOutCalls: Map<int, DateTimeOffset>
    }

let emptyTransportState =
    { StdIn = None
      StdOut = None
      PendingRead = None
      PendingWrite = None
      WriteQueue = []
      CallHandlers = Map.empty
      NotificationHandlers = Map.empty
      NextOutboundRequestId = 1
      NextInboundRequestOrdinal = 0
      PendingOutboundCalls = Map.empty
      RunningInboundRequests = Map.empty
      ShutdownWaiters = []
      RpcLogEntryCallback = None
      Timer = None
      RecentlyTimedOutCalls = Map.empty
      Phase = Active }

type JsonRpcTransportEvent =
    | Start of Stream * Stream * Map<string, JsonRpcCallHandler> * Map<string, JsonRpcNotificationHandler>
    | Shutdown of AsyncReplyChannel<unit>
    | InboundMessage of Result<byte[] option, Exception>
    | MessageWriteComplete of success: bool * message: OutboundMessage
    | SendNotification of method: string * methodParams: JsonElement * AsyncReplyChannel<unit>
    | SendCall of
        method: string *
        methodParams: JsonElement *
        callTimeout: TimeSpan option *
        AsyncReplyChannel<Result<JsonElement, JsonElement>>
    | HandlerCompleted of wireIdKey: string * result: Result<JsonElement, JsonElement>
    | HandlerFailed of wireIdKey: string * exn
    | HandlerCancelled of wireIdKey: string
    | CancelRequest of wireIdKey: string
    | NotificationHandlerFailed of method: string * exn
    | AwaitShutdown of AsyncReplyChannel<unit>
    | WriteRpcLogEntry of JsonRpcLogEntry
    | CheckTimeouts
    | GetRpcStats of AsyncReplyChannel<JsonRpcStats>

let readBytes (stream: Stream) (buffer: byte[]) (count: int) = async {
    let rec loop pos remaining = async {
        if remaining = 0 then
            return pos
        else
            let! bytesRead = stream.ReadAsync(buffer, pos, remaining) |> Async.AwaitTask

            if bytesRead = 0 then
                return pos // EOF
            else
                return! loop (pos + bytesRead) (remaining - bytesRead)
    }

    return! loop 0 count
}

/// Read a \r\n-terminated line as raw bytes from stream
let readLine (stream: Stream) = async {
    let buf = Array.zeroCreate 1

    let rec loop (acc: byte list) = async {
        let! bytesRead = readBytes stream buf 1

        if bytesRead = 0 then
            if acc.IsEmpty then
                return None // EOF before any data
            else
                return Some(acc |> List.rev |> Array.ofList)
        else
            let b = buf.[0]

            match b with
            | 0xAuy (* \n *) ->
                // strip trailing \r if present
                let bytes =
                    match acc with
                    | 0xDuy :: rest -> rest |> List.rev |> Array.ofList
                    | _ -> acc |> List.rev |> Array.ofList

                return Some bytes
            | c -> return! loop (c :: acc)
    }

    return! loop []
}

/// Read headers until a blank line. Returns None on EOF, or Some map of header name → value.
/// Header names are lowercased per HTTP semantics (header fields are case-insensitive).
let readHeaders (stream: Stream) = async {
    let rec loop (headers: Map<string, string>) = async {
        let! lineOpt = readLine stream

        match lineOpt with
        | None -> return None
        | Some [||] -> return Some headers
        | Some bytes ->
            let line = Encoding.ASCII.GetString(bytes)
            let parts = line.Split(": ", 2, StringSplitOptions.None)

            let headers =
                if parts.Length = 2 then
                    headers |> Map.add (parts.[0].ToLowerInvariant()) parts.[1]
                else
                    headers

            return! loop headers
    }

    return! loop Map.empty
}

let readMessage (stream: Stream) = async {
    let! headers = readHeaders stream

    let contentLength =
        headers
        |> Option.bind (Map.tryFind "content-length")
        |> Option.bind (fun s ->
            match Int32.TryParse(s) with
            | true, n -> Some n
            | _ -> None)

    match contentLength with
    | None
    | Some 0 -> return None
    | Some contentLength ->
        let buffer = Array.zeroCreate contentLength
        let! bytesRead = readBytes stream buffer contentLength

        if bytesRead < contentLength then
            return None // EOF mid-message
        else
            return Some buffer
}

let startInboundMessageRead postEvent (stdin: Stream) =
    let readOp = async {
        try
            let! msg = readMessage stdin
            postEvent (InboundMessage(Ok msg))
        with ex ->
            postEvent (InboundMessage(Error ex))
    }

    Async.Start readOp
    readOp

let writeMessage postEvent (stdout: Stream) msg = async {
    try
        let msgPayloadJson = msg.Payload.RootElement.ToString()
        let responseBytes = Encoding.UTF8.GetBytes(msgPayloadJson)

        let header =
            sprintf "Content-Length: %d\r\n\r\n" responseBytes.Length
            |> Encoding.ASCII.GetBytes

        do! stdout.WriteAsync(header, 0, header.Length) |> Async.AwaitTask
        do! stdout.WriteAsync(responseBytes, 0, responseBytes.Length) |> Async.AwaitTask
        do! stdout.FlushAsync() |> Async.AwaitTask

        let msgPayloadJsonElement =
            use doc = JsonSerializer.SerializeToDocument(msg.Payload)
            doc.RootElement.Clone()

        postEvent (WriteRpcLogEntry(RpcWrite msgPayloadJsonElement))
        postEvent (MessageWriteComplete(true, msg))
    with ex ->
        postEvent (WriteRpcLogEntry(RpcError(sprintf "startWrite: write error: %s" (string ex))))
        postEvent (MessageWriteComplete(false, msg))
}

let startMessageWrite postEvent (stdout: Stream) msg =
    let writeOp = writeMessage postEvent stdout msg
    Async.Start writeOp
    writeOp

/// Enqueue a message for writing. If no write is in progress, start one immediately;
/// otherwise append to the queue.
let enqueueOutboundMessage postEvent (state: JsonRpcTransportState) (msg: OutboundMessage) =
    match state.PendingWrite, state.StdOut with
    | None, Some stdout ->
        let writeOp = startMessageWrite postEvent stdout msg

        { state with
            PendingWrite = Some writeOp }
    | _, _ ->
        { state with
            WriteQueue = state.WriteQueue @ [ msg ] }

let makeError (code: int) (message: string) : JsonElement =
    let node = JsonObject()
    node["code"] <- JsonValue.Create(code)
    node["message"] <- JsonValue.Create(message)
    JsonSerializer.SerializeToElement(node)

/// Drain all pending outbound calls by replying with a synthetic transport-shutdown error.
/// Call this from any shutdown path before clearing PendingOutboundCalls.
let failPendingOutboundCalls postEvent (state: JsonRpcTransportState) =
    let shutdownErr = makeError -32099 "Transport shut down"

    for KeyValue(id, pendingCall) in state.PendingOutboundCalls do
        let rpcLogEntry =
            RpcWarn(sprintf "failPendingOutboundCalls: failing pending outbound call id=%d on shutdown" id)

        postEvent (WriteRpcLogEntry rpcLogEntry)

        pendingCall.ReplyChannel.Reply(Error shutdownErr)

/// Arm (or rearm) the single shared timer to fire at the earliest pending deadline.
/// Disposes and clears the timer when no timed calls remain, so that Timer = None
/// reliably means "not armed". Creates the timer lazily on first use; reuses it via
/// Change() when rescheduling between live calls — no per-call allocation in steady state.
/// Returns the (possibly updated) state with the Timer field set or cleared.
let rescheduleTimer (state: JsonRpcTransportState) (postEvent: JsonRpcTransportEvent -> unit) =
    let nextDeadline =
        state.PendingOutboundCalls
        |> Map.values
        |> Seq.choose (fun p -> p.Deadline)
        |> Seq.sort
        |> Seq.tryHead

    match nextDeadline, state.Timer with
    | None, Some t ->
        // No timed calls remain — dispose the timer so Option.isSome correctly reflects armed state.
        t.Dispose()
        { state with Timer = None }
    | None, None -> state
    | Some deadline, existing ->
        let delayMs = max 0 (int (deadline - DateTimeOffset.UtcNow).TotalMilliseconds)

        match existing with
        | Some t ->
            // Reuse: Change atomically cancels the prior fire and arms the new deadline.
            t.Change(delayMs, Threading.Timeout.Infinite) |> ignore
            state
        | None ->
            // First-ever arming: create the timer with a stable callback.
            // The callback is never replaced — every reschedule is just Change().
            let cb = Threading.TimerCallback(fun _ -> postEvent CheckTimeouts)
            let t = new Threading.Timer(cb, null, delayMs, Threading.Timeout.Infinite)
            { state with Timer = Some t }

let makeErrorResponse (id: JsonElement) (error: JsonElement) : JsonNode =
    let obj = JsonObject()
    obj["jsonrpc"] <- JsonValue.Create("2.0")
    obj["id"] <- JsonNode.Parse(id.GetRawText())
    obj["error"] <- JsonNode.Parse(error.GetRawText())
    obj :> JsonNode

/// Start an inbound request handler asynchronously with a CancellationTokenSource.
/// The handler runs on the thread pool; on completion/failure/cancellation, an event
/// is posted back to the actor.
let startCallHandler postEvent (handler: JsonRpcCallHandler) (context: JsonRpcRequestContext) =
    let wireIdKey = context.WireId.Value.GetRawText()
    let cts = context.CancellationTokenSource

    let handlerAsync = async {
        let! response = handler context
        postEvent (HandlerCompleted(wireIdKey, response))
    }

    Async.StartWithContinuations(
        handlerAsync,
        (fun () -> ()), // success: already posted HandlerCompleted inside the async
        (fun ex ->
            match ex with
            | :? OperationCanceledException -> postEvent (HandlerCancelled wireIdKey)
            | _ -> postEvent (HandlerFailed(wireIdKey, ex))),
        (fun _cancelEx -> postEvent (HandlerCancelled wireIdKey)),
        cancellationToken = (cts |> Option.map _.Token |> Option.defaultValue CancellationToken.None)
    )

/// Start a notification handler asynchronously. Failures are posted back to the actor for logging.
let startNotificationHandler postEvent (handler: JsonRpcNotificationHandler) (context: JsonRpcRequestContext) =
    let handlerAsync = async { do! handler context }

    Async.StartWithContinuations(
        handlerAsync,
        (fun () -> ()),
        (fun ex -> postEvent (NotificationHandlerFailed(context.MethodName, ex))),
        (fun _cancelEx -> ()),
        cancellationToken = CancellationToken.None
    )

/// Reject a message that lacks a valid "jsonrpc": "2.0" field.
/// If an id is present, send -32600 Invalid Request; otherwise drop silently.
let handleInvalidVersion state postEvent (id: JsonElement option) =
    match id with
    | Some requestId ->
        let errorResponse =
            makeErrorResponse requestId (makeError -32600 "Invalid Request: jsonrpc must be \"2.0\"")
            |> JsonSerializer.SerializeToDocument

        enqueueOutboundMessage
            postEvent
            state
            { Payload = errorResponse
              CompletionRC = None }
    | None ->
        postEvent (
            WriteRpcLogEntry(RpcError "handleInboundMessage: dropping message with missing or invalid jsonrpc field")
        )

        state

/// Dispatch an inbound request (method + id present) to a registered call handler.
/// Sends -32601 Method Not Found if no handler is registered.
let handleInboundRequest state postEvent (m: string) (requestId: JsonElement) (msgParams: JsonElement option) =
    let wireIdKey = requestId.GetRawText()

    match state.CallHandlers |> Map.tryFind m with
    | Some handler ->
        let cts = new CancellationTokenSource()

        let context =
            { MethodName = m
              RequestOrdinal = state.NextInboundRequestOrdinal
              WireId = Some requestId
              Params = msgParams
              CancellationTokenSource = Some cts }

        do startCallHandler postEvent handler context

        { state with
            NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1)
            RunningInboundRequests = state.RunningInboundRequests |> Map.add wireIdKey context }
    | None ->
        let errorResponse =
            makeErrorResponse requestId (makeError -32601 (sprintf "Method not found: '%s'" m))
            |> JsonSerializer.SerializeToDocument

        enqueueOutboundMessage
            postEvent
            state
            { Payload = errorResponse
              CompletionRC = None }

/// Dispatch an inbound notification (method present, no id) to a registered notification handler.
/// $/cancelRequest is handled specially. No response is ever sent.
let handleInboundNotification state postEvent (m: string) (msg: JsonElement) =
    if m = "$/cancelRequest" then
        let idParam =
            match msg.TryGetProperty("params") with
            | true, paramsEl ->
                match paramsEl.TryGetProperty("id") with
                | true, idEl -> Some(idEl.GetRawText())
                | _ -> None
            | _ -> None

        match idParam with
        | Some cancelId -> postEvent (CancelRequest cancelId)
        | None -> postEvent (WriteRpcLogEntry(RpcError "handleInboundMessage: $/cancelRequest missing 'id' parameter"))

        state
    else
        let context =
            { MethodName = m
              RequestOrdinal = state.NextInboundRequestOrdinal
              WireId = None
              Params =
                match msg.TryGetProperty("params") with
                | true, p -> Some p
                | _ -> None
              CancellationTokenSource = None }

        match state.NotificationHandlers |> Map.tryFind m with
        | Some handler -> do startNotificationHandler postEvent handler context
        | None ->
            postEvent (
                WriteRpcLogEntry(RpcError(sprintf "handleInboundMessage: no notification handler for method '%s'" m))
            )

        { state with
            NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1) }

/// Resolve a pending outbound call using an inbound response (no method, id present).
/// Three cases: normal resolution, late response after timeout (RpcWarn), genuine unknown ID (RpcError).
let handleInboundResponse state postEvent (responseId: JsonElement) (msg: JsonElement) =
    let idVal = responseId.GetInt32()

    match state.PendingOutboundCalls |> Map.tryFind idVal with
    | Some pending ->
        match msg.TryGetProperty("error") with
        | true, err -> pending.ReplyChannel.Reply(Error err)
        | _ ->
            let result =
                match msg.TryGetProperty("result") with
                | true, r -> r
                | _ -> JsonDocument.Parse("null").RootElement

            pending.ReplyChannel.Reply(Ok result)

        let state =
            { state with
                PendingOutboundCalls = state.PendingOutboundCalls |> Map.remove idVal }

        rescheduleTimer state postEvent

    | None ->
        if state.RecentlyTimedOutCalls |> Map.containsKey idVal then
            postEvent (
                WriteRpcLogEntry(
                    RpcWarn(
                        sprintf
                            "handleInboundResponse: late response for call id=%d — already timed out; response discarded"
                            idVal
                    )
                )
            )
        else
            postEvent (
                WriteRpcLogEntry(
                    RpcError(sprintf "handleInboundResponse: response for unknown call id=%d; response discarded" idVal)
                )
            )

        state

/// Top-level inbound message router. Validates the jsonrpc version field, then
/// classifies the message and delegates to the appropriate handler.
let handleInboundMessage state postEvent (msgDoc: JsonDocument) =
    let msg = msgDoc.RootElement.Clone()

    let jsonrpc =
        match msg.TryGetProperty("jsonrpc") with
        | true, v -> v.GetString() |> Some
        | _ -> None

    let id =
        match msg.TryGetProperty("id") with
        | true, v -> Some v
        | _ -> None

    if jsonrpc <> Some "2.0" then
        handleInvalidVersion state postEvent id
    else

        let method =
            match msg.TryGetProperty("method") with
            | true, v -> v.GetString() |> Some
            | _ -> None

        let msgParams =
            match msg.TryGetProperty("params") with
            | true, v -> Some v
            | _ -> None

        match method, id with
        | Some m, Some requestId -> handleInboundRequest state postEvent m requestId msgParams
        | Some m, None -> handleInboundNotification state postEvent m msg
        | None, Some responseId -> handleInboundResponse state postEvent responseId msg
        | None, None ->
            postEvent (WriteRpcLogEntry(RpcError "handleInboundMessage: message has no 'method' or 'id' field"))
            state

/// Common shutdown entry point for both EOF and explicit Shutdown.
/// Fails all pending outbound calls, disposes the timer, cancels all running inbound
/// handlers, and moves to ShuttingDown. The optional reply channel is appended to
/// ShutdownWaiters so it is fired once all handlers have drained.
let beginShutdown postEvent (shutdownWaiter: AsyncReplyChannel<unit> option) (state: JsonRpcTransportState) =
    failPendingOutboundCalls postEvent state
    state.Timer |> Option.iter _.Dispose()

    for KeyValue(wireIdKey, _) in state.RunningInboundRequests do
        postEvent (CancelRequest wireIdKey)

    let shutdownWaiters =
        match shutdownWaiter with
        | Some rc -> rc :: state.ShutdownWaiters
        | None -> state.ShutdownWaiters

    { state with
        StdIn = None
        PendingRead = None
        PendingOutboundCalls = Map.empty
        Timer = None
        Phase = ShuttingDown
        ShutdownWaiters = shutdownWaiters }

/// If we are draining after a Shutdown and the last handler just finished, fire all
/// ShutdownWaiters (including the original Shutdown reply channel) and move to Stopped.
let tryFireShutdownWaiters (state: JsonRpcTransportState) =
    match state.Phase with
    | ShuttingDown when state.RunningInboundRequests.IsEmpty ->
        state.ShutdownWaiters |> List.iter _.Reply()

        { state with
            ShutdownWaiters = []
            Phase = Stopped }
    | _ -> state

/// Shared epilogue for HandlerCompleted / HandlerFailed / HandlerCancelled.
let completeInboundRequest postEvent (state: JsonRpcTransportState) wireIdKey (responsePayload: JsonNode option) =
    let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey
    ctx |> Option.bind _.CancellationTokenSource |> Option.iter _.Dispose()

    let newState =
        { state with
            RunningInboundRequests = state.RunningInboundRequests |> Map.remove wireIdKey }

    let newState =
        match responsePayload with
        | Some payload ->
            enqueueOutboundMessage
                postEvent
                newState
                { Payload = payload |> JsonSerializer.SerializeToDocument
                  CompletionRC = None }
        | None -> newState

    tryFireShutdownWaiters newState

let processEvent state postEvent ev =
    match ev with
    | Start(stdin, stdout, callHandlers, notificationHandlers) ->
        let readOp = stdin |> startInboundMessageRead postEvent

        { state with
            StdIn = Some stdin
            StdOut = Some stdout
            CallHandlers = callHandlers
            NotificationHandlers = notificationHandlers
            PendingRead = Some readOp }

    | MessageWriteComplete(_success, msg) ->
        msg.Payload.Dispose()
        msg.CompletionRC |> Option.iter _.Reply()

        match state.WriteQueue, state.StdOut with
        | nextMsg :: rest, Some stdout ->
            let writeOp = startMessageWrite postEvent stdout nextMsg

            { state with
                PendingWrite = Some writeOp
                WriteQueue = rest }
        | _ -> { state with PendingWrite = None }

    | InboundMessage result ->
        match result with
        | Ok msgBytes ->
            match msgBytes with
            | Some msgBytes ->
                use doc = JsonDocument.Parse(ReadOnlyMemory<byte>(msgBytes))

                postEvent (WriteRpcLogEntry(RpcRead(doc.RootElement.Clone())))
                let updatedState = handleInboundMessage state postEvent doc

                let pendingRead =
                    updatedState.StdIn |> Option.map (startInboundMessageRead postEvent)

                { updatedState with
                    PendingRead = pendingRead }

            | None ->
                // EOF on stdin — full shutdown: fail pending outbound calls, cancel all
                // running inbound handlers, move to ShuttingDown, then drain.
                // Unlike explicit Shutdown there is no reply channel to push, but any
                // existing AwaitShutdown waiters will be fired once the map empties.
                state |> beginShutdown postEvent None |> tryFireShutdownWaiters
        | Error ex ->
            postEvent (WriteRpcLogEntry(RpcError(sprintf "InboundMessage: read error (retrying): %s" (string ex))))
            let pendingRead = state.StdIn |> Option.map (startInboundMessageRead postEvent)
            { state with PendingRead = pendingRead }

    | Shutdown rc ->
        // Fail pending calls, cancel all running handlers, move to ShuttingDown.
        // rc is parked in ShutdownWaiters and fired once all handlers have drained.
        state |> beginShutdown postEvent (Some rc) |> tryFireShutdownWaiters

    | SendNotification(method, methodParams, rc) ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            postEvent (
                WriteRpcLogEntry(RpcWarn(sprintf "SendNotification: dropping '%s', transport is shutting down" method))
            )

            rc.Reply()
            state
        | Active ->
            let msg = JsonObject()
            msg["jsonrpc"] <- JsonValue.Create("2.0")
            msg["method"] <- JsonValue.Create(method)
            msg["params"] <- JsonNode.Parse(methodParams.GetRawText())

            enqueueOutboundMessage
                postEvent
                state
                { Payload = msg |> JsonSerializer.SerializeToDocument
                  CompletionRC = Some rc }

    | SendCall(method, methodParams, callTimeout, replyChannel) ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            postEvent (WriteRpcLogEntry(RpcWarn(sprintf "SendCall: dropping '%s', transport is shutting down" method)))
            replyChannel.Reply(Error(makeError -32099 "Transport shut down"))
            state
        | Active ->
            let id = state.NextOutboundRequestId

            let msg = JsonObject()
            msg["jsonrpc"] <- JsonValue.Create("2.0")
            msg["id"] <- JsonValue.Create(id)
            msg["method"] <- JsonValue.Create(method)
            msg["params"] <- JsonNode.Parse(methodParams.GetRawText())

            let pendingCall =
                { Method = method
                  Timeout = callTimeout
                  Deadline = callTimeout |> Option.map (fun t -> DateTimeOffset.UtcNow + t)
                  ReplyChannel = replyChannel }

            let newState =
                { state with
                    NextOutboundRequestId = id + 1
                    PendingOutboundCalls = state.PendingOutboundCalls |> Map.add id pendingCall }

            let outboundMessage =
                { Payload = msg |> JsonSerializer.SerializeToDocument
                  CompletionRC = None }

            let newState = outboundMessage |> enqueueOutboundMessage postEvent newState

            rescheduleTimer newState postEvent

    | HandlerCompleted(wireIdKey, result) ->
        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        let responsePayload =
            ctx
            |> Option.map (fun ctx ->
                match result with
                | Ok returnValue ->
                    let obj = JsonObject()
                    obj["jsonrpc"] <- JsonValue.Create("2.0")
                    obj["id"] <- JsonNode.Parse(ctx.WireId.Value.GetRawText())
                    obj["result"] <- JsonNode.Parse(returnValue.GetRawText())
                    obj :> JsonNode
                | Error error -> makeErrorResponse ctx.WireId.Value error)

        completeInboundRequest postEvent state wireIdKey responsePayload

    | HandlerFailed(wireIdKey, ex) ->
        postEvent (
            WriteRpcLogEntry(RpcError(sprintf "processEvent: handler failed for request %s: %s" wireIdKey (string ex)))
        )

        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        let responsePayload =
            ctx
            |> Option.map (fun ctx ->
                makeErrorResponse
                    ctx.WireId.Value
                    (makeError -32603 (sprintf "Internal error: %s" (ex.GetType().Name))))

        completeInboundRequest postEvent state wireIdKey responsePayload

    | HandlerCancelled wireIdKey ->
        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        let responsePayload =
            ctx
            |> Option.map (fun ctx -> makeErrorResponse ctx.WireId.Value (makeError -32800 "Request cancelled"))

        completeInboundRequest postEvent state wireIdKey responsePayload

    | NotificationHandlerFailed(method, ex) ->
        postEvent (
            WriteRpcLogEntry(
                RpcError(sprintf "processEvent: notification handler failed for '%s': %s" method (string ex))
            )
        )

        state

    | AwaitShutdown rc ->
        { state with
            ShutdownWaiters = rc :: state.ShutdownWaiters }

    | CancelRequest wireIdKey ->
        let cancelCts (cts: CancellationTokenSource) =
            try
                cts.Cancel()
            with ex ->
                postEvent (
                    WriteRpcLogEntry(
                        RpcError(sprintf "processEvent: error cancelling request %s: %s" wireIdKey (string ex))
                    )
                )

        state.RunningInboundRequests
        |> Map.tryFind wireIdKey
        |> Option.bind _.CancellationTokenSource
        |> Option.iter cancelCts

        state

    | CheckTimeouts ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            // Timer callback raced with shutdown disposal — ignore.
            state
        | Active ->
            let now = DateTimeOffset.UtcNow

            // Partition into expired (deadline passed) and still-live entries.
            let expired, live =
                state.PendingOutboundCalls
                |> Map.partition (fun _ p -> p.Deadline |> Option.exists (fun d -> d <= now))

            // Fail every expired call and log with method name + original timeout duration.
            for KeyValue(id, pending) in expired do
                let timeoutDesc =
                    pending.Timeout |> Option.map string |> Option.defaultValue "<no timeout>"

                postEvent (
                    WriteRpcLogEntry(
                        RpcWarn(
                            sprintf
                                "CheckTimeouts: call id=%d method=%s timed out after %s"
                                id
                                pending.Method
                                timeoutDesc
                        )
                    )
                )

                pending.ReplyChannel.Reply(Error(makeError -32000 "Call timed out"))

            // Stamp expired IDs into RecentlyTimedOutCalls, then prune entries older than 60 s.
            let ttl = TimeSpan.FromSeconds 60.0

            let stamped =
                (state.RecentlyTimedOutCalls, expired)
                ||> Map.fold (fun acc id _ -> acc |> Map.add id now)

            let pruned = stamped |> Map.filter (fun _ stamped -> now - stamped <= ttl)

            let state =
                { state with
                    PendingOutboundCalls = live
                    RecentlyTimedOutCalls = pruned }

            rescheduleTimer state postEvent

    | GetRpcStats replyChannel ->
        let phaseStr =
            match state.Phase with
            | Active -> "Active"
            | ShuttingDown -> "ShuttingDown"
            | Stopped -> "Stopped"

        replyChannel.Reply(
            { Phase = phaseStr
              WriteQueueLength = state.WriteQueue.Length
              PendingOutboundCallCount = state.PendingOutboundCalls.Count
              RunningInboundRequestCount = state.RunningInboundRequests.Count
              TimerArmed = state.Timer |> Option.isSome
              RecentlyTimedOutCallCount = state.RecentlyTimedOutCalls.Count }
            : JsonRpcStats
        )

        state

    | WriteRpcLogEntry entry ->
        state.RpcLogEntryCallback |> Option.iter (fun cb -> cb entry)
        state

let eventLoop (initialState: JsonRpcTransportState) (inbox: MailboxProcessor<JsonRpcTransportEvent>) =
    let rec loop state = async {
        let! ev = inbox.Receive()

        let newState =
            try
                processEvent state inbox.Post ev
            with ex ->
                inbox.Post(WriteRpcLogEntry(RpcError(sprintf "eventLoop: failed %s" (string ex))))
                raise ex

        return! loop newState
    }

    loop initialState

let startJsonRpcTransport
    (stdin: Stream)
    (stdout: Stream)
    (rpcLogCallback: (JsonRpcLogEntry -> unit) option)
    (configure: MailboxProcessor<JsonRpcTransportEvent> -> JsonRpcCallHandlerMap * JsonRpcNotificationHandlerMap)
    =

    let initialState =
        { emptyTransportState with
            RpcLogEntryCallback = rpcLogCallback }

    let transport = MailboxProcessor.Start(eventLoop initialState)
    let callHandlers, notificationHandlers = configure transport
    transport.Post(Start(stdin, stdout, callHandlers, notificationHandlers))
    transport

let sendJsonRpcNotification
    (server: MailboxProcessor<JsonRpcTransportEvent>)
    (method: string)
    (methodParams: JsonElement)
    =
    server.PostAndAsyncReply(fun rc -> SendNotification(method, methodParams, rc))

let sendJsonRpcCall (transport: MailboxProcessor<JsonRpcTransportEvent>) (method: string) (methodParams: JsonElement) =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, None, rc))

let sendJsonRpcCallWithTimeout
    (transport: MailboxProcessor<JsonRpcTransportEvent>)
    (method: string)
    (methodParams: JsonElement)
    (callTimeout: TimeSpan option)
    =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, callTimeout, rc))

let shutdownJsonRpcTransport (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(Shutdown)

let awaitJsonRpcTransportShutdown (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(AwaitShutdown)

let getJsonRpcStats (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(GetRpcStats)
