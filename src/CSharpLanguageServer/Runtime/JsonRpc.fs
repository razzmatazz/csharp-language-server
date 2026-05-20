/// JSON-RPC 2.0 transport implementation for the Language Server Protocol.
/// This module is JSON-RPC 2.0 only — JSON-RPC 1.0 messages are not supported.
/// LSP mandates JSON-RPC 2.0 exclusively, so no 1.0 compatibility is needed or provided.
module CSharpLanguageServer.Runtime.JsonRpc

open System
open System.Buffers
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
    { Payload: JsonDocument
      RequestOrdinal: int64
      WireId: JsonElement option
      MethodName: string
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
    | ShuttingDown // Shutdown received, handler cancellations fired, waiting for RunningInboundHandlers to drain
    | Stopped // all handlers drained, ShutdownWaiters fired

type PendingCall =
    { MethodName: string
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
        /// All inbound handlers (requests and notifications) currently running, keyed by RequestOrdinal.
        RunningInboundHandlers: Map<int64, JsonRpcRequestContext>
        /// Side-index for inbound *requests* only: maps wire-ID string → RequestOrdinal.
        /// Used by CancelRequest (which arrives with a wire ID) to find the right CTS,
        /// and by the HandlerTerminated arm to build responses.
        RunningInboundCallsByWireId: Map<string, int64>
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
      RunningInboundHandlers = Map.empty
      RunningInboundCallsByWireId = Map.empty
      ShutdownWaiters = []
      RpcLogEntryCallback = None
      Timer = None
      RecentlyTimedOutCalls = Map.empty
      Phase = Active }

type HandlerOutcome =
    | HandlerReturnedOk of JsonElement
    | HandlerReturnedError of JsonElement
    | HandlerFailed of exn
    | HandlerCancelled

type JsonRpcTransportEvent =
    | Start of Stream * Stream * Map<string, JsonRpcCallHandler> * Map<string, JsonRpcNotificationHandler>
    | Shutdown of AsyncReplyChannel<unit>
    | InboundMessage of Result<JsonDocument option, Exception> // JsonDocument should be disposed by the handler
    | MessageWriteComplete of success: bool * message: OutboundMessage
    | SendNotification of methodName: string * methodParams: JsonElement * AsyncReplyChannel<unit>
    | SendCall of
        methodName: string *
        methodParams: JsonElement *
        callTimeout: TimeSpan option *
        AsyncReplyChannel<Result<JsonElement, JsonElement>>
    | HandlerTerminated of ordinal: int64 * outcome: HandlerOutcome
    | CancelRequest of wireIdKey: string
    | AwaitShutdown of AsyncReplyChannel<unit>
    | CheckTimeouts
    | GetRpcStats of AsyncReplyChannel<JsonRpcStats>

let nullJE =
    use nullDoc = JsonDocument.Parse("null")
    nullDoc.RootElement.Clone()

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

let startInboundMessageRead log postEvent (stdin: Stream) =
    let readOp = async {
        try
            let! msg = readMessage stdin

            let doc =
                msg |> Option.map (fun msg -> JsonDocument.Parse(ReadOnlyMemory<byte>(msg)))

            doc |> Option.iter (fun doc -> log (RpcRead doc.RootElement))

            postEvent (InboundMessage(Ok doc))
        with ex ->
            postEvent (InboundMessage(Error ex))
    }

    Async.Start readOp
    readOp

let writeMessage log postEvent (stdout: Stream) msg = async {
    try
        let jsonBuffer = ArrayBufferWriter<byte>()

        do
            use jsonWriter = new Utf8JsonWriter(jsonBuffer)
            msg.Payload.WriteTo(jsonWriter)
            jsonWriter.Flush()

        let jsonRpcHeader =
            sprintf "Content-Length: %d\r\n\r\n" jsonBuffer.WrittenCount
            |> Encoding.ASCII.GetBytes

        do! stdout.WriteAsync(jsonRpcHeader, 0, jsonRpcHeader.Length) |> Async.AwaitTask
        do! stdout.WriteAsync(jsonBuffer.WrittenMemory) |> _.AsTask() |> Async.AwaitTask
        do! stdout.FlushAsync() |> Async.AwaitTask

        log (RpcWrite msg.Payload.RootElement)
        postEvent (MessageWriteComplete(true, msg))
    with ex ->
        log (RpcError(sprintf "startWrite: write error: %s" (string ex)))
        postEvent (MessageWriteComplete(false, msg))
}

let startMessageWrite log postEvent (stdout: Stream) msg =
    let writeOp = writeMessage log postEvent stdout msg
    Async.Start writeOp
    writeOp

/// Enqueue a message for writing. If no write is in progress, start one immediately;
/// otherwise append to the queue.
let enqueueOutboundMessage postEvent (msg: OutboundMessage) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    match state.PendingWrite, state.StdOut with
    | None, Some stdout ->
        let writeOp = startMessageWrite log postEvent stdout msg

        { state with
            PendingWrite = Some writeOp }
    | _, _ ->
        { state with
            WriteQueue = state.WriteQueue @ [ msg ] }

/// Write JSON directly into an ArrayBufferWriter via Utf8JsonWriter, then parse the
/// resulting bytes into a JsonDocument in a single pass — no intermediate JsonObject,
/// JsonNode.Parse, or SerializeToDocument round-trip.
let makePayload (write: Utf8JsonWriter -> unit) : JsonDocument =
    let buf = ArrayBufferWriter<byte>()
    use w = new Utf8JsonWriter(buf)
    write w
    w.Flush()
    JsonDocument.Parse(buf.WrittenMemory)

let makeError (code: int) (message: string) : JsonElement =
    let node = JsonObject()
    node["code"] <- JsonValue.Create(code)
    node["message"] <- JsonValue.Create(message)
    JsonSerializer.SerializeToElement(node)

/// Drain all pending outbound calls by replying with a synthetic transport-shutdown error.
/// Call this from any shutdown path before clearing PendingOutboundCalls.
let failPendingOutboundCalls state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore
    let shutdownErr = makeError -32099 "Transport shut down"

    for KeyValue(id, pendingCall) in state.PendingOutboundCalls do
        log (RpcWarn(sprintf "failPendingOutboundCalls: failing pending outbound call id=%d on shutdown" id))
        pendingCall.ReplyChannel.Reply(Error shutdownErr)

/// Arm (or rearm) the single shared timer to fire at the earliest pending deadline.
/// Disposes and clears the timer when no timed calls remain, so that Timer = None
/// reliably means "not armed". Creates the timer lazily on first use; reuses it via
/// Change() when rescheduling between live calls — no per-call allocation in steady state.
/// Returns the (possibly updated) state with the Timer field set or cleared.
let rescheduleTimer (postEvent: JsonRpcTransportEvent -> unit) state =
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

let makeErrorPayload (id: JsonElement) (error: JsonElement) : JsonDocument =
    makePayload (fun w ->
        w.WriteStartObject()
        w.WriteString("jsonrpc", "2.0")
        w.WritePropertyName("id")
        id.WriteTo(w)
        w.WritePropertyName("error")
        error.WriteTo(w)
        w.WriteEndObject())

/// Start an inbound request handler asynchronously with a CancellationTokenSource.
/// The handler runs on the thread pool; on completion/failure/cancellation, an event
/// is posted back to the actor.
let startCallHandler postEvent (handler: JsonRpcCallHandler) (context: JsonRpcRequestContext) =
    let cts = context.CancellationTokenSource

    let handlerAsync = async {
        let! response = handler context

        let outcome =
            match response with
            | Ok result -> HandlerReturnedOk result
            | Error err -> HandlerReturnedError err

        postEvent (HandlerTerminated(context.RequestOrdinal, outcome))
    }

    Async.StartWithContinuations(
        handlerAsync,
        (fun () -> ()), // success: already posted HandlerTerminated inside the async
        (fun ex ->
            let outcome =
                match ex with
                | :? OperationCanceledException -> HandlerCancelled
                | _ -> HandlerFailed ex

            postEvent (HandlerTerminated(context.RequestOrdinal, outcome))),
        (fun _cancelEx -> postEvent (HandlerTerminated(context.RequestOrdinal, HandlerCancelled))),
        cancellationToken = (cts |> Option.map _.Token |> Option.defaultValue CancellationToken.None)
    )

/// Start a notification handler asynchronously.
/// Completion, failure and cancellation are all posted back via the shared HandlerTerminated
/// event (keyed on ordinal), so the HandlerTerminated arm disposes the payload and removes
/// the entry from RunningInboundHandlers uniformly.
let startNotificationHandler postEvent (handler: JsonRpcNotificationHandler) (context: JsonRpcRequestContext) =
    let ordinal = context.RequestOrdinal
    let cts = context.CancellationTokenSource

    let handlerAsync = async { do! handler context }

    Async.StartWithContinuations(
        handlerAsync,
        (fun () -> postEvent (HandlerTerminated(ordinal, HandlerReturnedOk nullJE))),
        (fun ex ->
            let outcome =
                match ex with
                | :? OperationCanceledException -> HandlerCancelled
                | _ -> HandlerFailed ex

            postEvent (HandlerTerminated(ordinal, outcome))),
        (fun _cancelEx -> postEvent (HandlerTerminated(ordinal, HandlerCancelled))),
        cancellationToken = (cts |> Option.map _.Token |> Option.defaultValue CancellationToken.None)
    )

/// Reject a message that lacks a valid "jsonrpc": "2.0" field.
/// If an id is present, send -32600 Invalid Request; otherwise drop silently.
let handleInvalidVersionPayload postEvent (payload: JsonDocument) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    let id =
        match payload.RootElement.TryGetProperty("id") with
        | true, v -> Some v
        | _ -> None

    let newState =
        match id with
        | Some requestId ->
            let errorResponse =
                makeErrorPayload requestId (makeError -32600 "Invalid Request: jsonrpc must be \"2.0\"")

            state
            |> enqueueOutboundMessage
                postEvent
                { Payload = errorResponse
                  CompletionRC = None }

        | None ->
            log (RpcError "handleInboundMessage: dropping message with missing or invalid jsonrpc field")
            state

    payload.Dispose()
    newState

/// Dispatch an inbound request (method + id present) to a registered call handler.
/// Sends -32601 Method Not Found if no handler is registered.
let handleInboundRequest postEvent (methodName: string) (payload: JsonDocument) state =
    let requestWireId = payload.RootElement.GetProperty("id")

    match state.CallHandlers |> Map.tryFind methodName with
    | Some handler ->
        let cts = new CancellationTokenSource()

        let paramsEl =
            match payload.RootElement.TryGetProperty("params") with
            | true, v -> Some v
            | _ -> None

        let context =
            { Payload = payload
              RequestOrdinal = state.NextInboundRequestOrdinal
              WireId = Some requestWireId
              MethodName = methodName
              Params = paramsEl
              CancellationTokenSource = Some cts }

        do startCallHandler postEvent handler context

        { state with
            NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1)
            RunningInboundHandlers = state.RunningInboundHandlers |> Map.add context.RequestOrdinal context
            RunningInboundCallsByWireId =
                state.RunningInboundCallsByWireId
                |> Map.add (requestWireId.GetRawText()) context.RequestOrdinal }

    | None ->
        let errorResponse =
            makeErrorPayload requestWireId (makeError -32601 (sprintf "Method not found: '%s'" methodName))

        payload.Dispose()

        state
        |> enqueueOutboundMessage
            postEvent
            { Payload = errorResponse
              CompletionRC = None }

/// Dispatch an inbound notification (method present, no id) to a registered notification handler.
/// Handle a $/cancelRequest notification: extract the target id and post a CancelRequest event.
let handleCancelRequest postEvent (payload: JsonDocument) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    let idJson: string option =
        match payload.RootElement.TryGetProperty("params") with
        | true, paramsEl ->
            match paramsEl.TryGetProperty("id") with
            | true, idEl -> Some(idEl.GetRawText())
            | _ -> None
        | _ -> None

    match idJson with
    | Some cancelId -> postEvent (CancelRequest cancelId)
    | None -> log (RpcError "handleInboundMessage: $/cancelRequest missing 'id' parameter")

    payload.Dispose()
    state

/// Handle any ordinary inbound notification by dispatching to its registered handler.
let handleRegularNotification postEvent (methodName: string) (payload: JsonDocument) state =
    let paramsEl =
        match payload.RootElement.TryGetProperty("params") with
        | true, p -> Some p
        | _ -> None

    let cts = new CancellationTokenSource()

    let context =
        { Payload = payload
          RequestOrdinal = state.NextInboundRequestOrdinal
          WireId = None
          MethodName = methodName
          Params = paramsEl
          CancellationTokenSource = Some cts }

    match state.NotificationHandlers |> Map.tryFind methodName with
    | Some handler -> do startNotificationHandler postEvent handler context
    | None ->
        let log = state.RpcLogEntryCallback |> Option.defaultValue ignore
        log (RpcError(sprintf "handleInboundMessage: no notification handler for method '%s'" methodName))

        // No handler will post HandlerCompleted, so clean up immediately.
        cts.Dispose()
        payload.Dispose()

    match state.NotificationHandlers |> Map.tryFind methodName with
    | Some _ ->
        { state with
            NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1)
            RunningInboundHandlers = state.RunningInboundHandlers |> Map.add context.RequestOrdinal context }
    | None ->
        { state with
            NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1) }

/// $/cancelRequest is handled specially. All other notifications are dispatched to registered handlers.
let handleInboundNotification postEvent (methodName: string) (payload: JsonDocument) state =
    match methodName with
    | "$/cancelRequest" -> handleCancelRequest postEvent payload state
    | _ -> handleRegularNotification postEvent methodName payload state

/// Resolve a pending outbound call using an inbound response (no method, id present).
/// Three cases: normal resolution, late response after timeout (RpcWarn), genuine unknown ID (RpcError).
let handleInboundResponse postEvent (payload: JsonDocument) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore
    let idVal = payload.RootElement.GetProperty("id").GetInt32()

    match state.PendingOutboundCalls |> Map.tryFind idVal with
    | Some pending ->
        // Clone() is required here: ReplyChannel.Reply() unblocks the waiting async
        // continuation on another thread, which will call GetRawText() on the returned
        // JsonElement. Without Clone(), that element is just a view into payload's buffer,
        // which we free with payload.Dispose() immediately below — a race we cannot solve
        // by reordering because the consumer lives outside the event loop.
        match payload.RootElement.TryGetProperty("error") with
        | true, err -> pending.ReplyChannel.Reply(Error(err.Clone()))
        | _ ->
            let result =
                match payload.RootElement.TryGetProperty("result") with
                | true, r -> r.Clone()
                | _ -> nullJE

            pending.ReplyChannel.Reply(Ok result)

        payload.Dispose()

        let newState =
            { state with
                PendingOutboundCalls = state.PendingOutboundCalls |> Map.remove idVal }

        newState |> rescheduleTimer postEvent

    | None ->
        payload.Dispose()

        if state.RecentlyTimedOutCalls |> Map.containsKey idVal then
            log (
                RpcWarn(
                    sprintf
                        "handleInboundResponse: late response for call id=%d — already timed out; response discarded"
                        idVal
                )
            )
        else
            log (RpcError(sprintf "handleInboundResponse: response for unknown call id=%d; response discarded" idVal))

        state

/// Top-level inbound message router. Validates the jsonrpc version field, then
/// classifies the message and delegates to the appropriate handler.
let handleInboundMessagePayload postEvent (payload: JsonDocument) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    let jsonrpc =
        match payload.RootElement.TryGetProperty("jsonrpc") with
        | true, v -> v.GetString() |> Some
        | _ -> None

    match jsonrpc with
    | Some "2.0" ->
        let methodName: string option =
            match payload.RootElement.TryGetProperty("method") with
            | true, v -> v.GetString() |> Some
            | _ -> None

        let haveId = payload.RootElement.TryGetProperty("id") |> fst

        match methodName, haveId with
        | Some m, true -> handleInboundRequest postEvent m payload state
        | Some m, false -> handleInboundNotification postEvent m payload state
        | None, true -> handleInboundResponse postEvent payload state
        | None, false ->
            log (RpcError "handleInboundMessage: message has no 'method' or 'id' field")
            payload.Dispose()
            state

    | _ -> state |> handleInvalidVersionPayload postEvent payload

/// Common shutdown entry point for both EOF and explicit Shutdown.
/// Fails all pending outbound calls, disposes the timer, cancels all running inbound
/// handlers, and moves to ShuttingDown. The optional reply channel is appended to
/// ShutdownWaiters so it is fired once all handlers have drained.
let beginShutdown postEvent (shutdownWaiter: AsyncReplyChannel<unit> option) state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    failPendingOutboundCalls state
    state.Timer |> Option.iter _.Dispose()

    for KeyValue(ordinal, ctx) in state.RunningInboundHandlers do
        ctx.CancellationTokenSource
        |> Option.iter (fun cts ->
            try
                cts.Cancel()
            with ex ->
                log (RpcError(sprintf "beginShutdown: error cancelling handler ordinal=%d: %s" ordinal (string ex))))

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
let tryFireShutdownWaiters state =
    match state.Phase with
    | ShuttingDown when state.RunningInboundHandlers.IsEmpty ->
        state.ShutdownWaiters |> List.iter _.Reply()

        { state with
            ShutdownWaiters = []
            Phase = Stopped }
    | _ -> state

let processEvent postEvent ev state =
    let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

    match ev with
    | Start(stdin, stdout, callHandlers, notificationHandlers) ->
        let readOp = stdin |> startInboundMessageRead log postEvent

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
            let writeOp = startMessageWrite log postEvent stdout nextMsg

            { state with
                PendingWrite = Some writeOp
                WriteQueue = rest }
        | _ -> { state with PendingWrite = None }

    | InboundMessage result ->
        match result with
        | Ok payload ->
            match payload with
            | Some payload ->
                let newState = state |> handleInboundMessagePayload postEvent payload

                let pendingRead =
                    newState.StdIn |> Option.map (startInboundMessageRead log postEvent)

                { newState with
                    PendingRead = pendingRead }

            | None ->
                // EOF on stdin — full shutdown: fail pending outbound calls, cancel all
                // running inbound handlers, move to ShuttingDown, then drain.
                // Unlike explicit Shutdown there is no reply channel to push, but any
                // existing AwaitShutdown waiters will be fired once the map empties.
                state |> beginShutdown postEvent None |> tryFireShutdownWaiters

        | Error ex ->
            log (RpcError(sprintf "InboundMessage: read error (retrying): %s" (string ex)))
            let pendingRead = state.StdIn |> Option.map (startInboundMessageRead log postEvent)

            { state with PendingRead = pendingRead }

    | Shutdown rc ->
        // Fail pending calls, cancel all running handlers, move to ShuttingDown.
        // rc is parked in ShutdownWaiters and fired once all handlers have drained.
        state |> beginShutdown postEvent (Some rc) |> tryFireShutdownWaiters

    | SendNotification(methodName, methodParams, rc) ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            log (RpcWarn(sprintf "SendNotification: dropping '%s', transport is shutting down" methodName))
            rc.Reply()
            state

        | Active ->
            let payload =
                makePayload (fun w ->
                    w.WriteStartObject()
                    w.WriteString("jsonrpc", "2.0")
                    w.WriteString("method", methodName)
                    w.WritePropertyName("params")
                    methodParams.WriteTo(w)
                    w.WriteEndObject())

            state
            |> enqueueOutboundMessage
                postEvent
                { Payload = payload
                  CompletionRC = Some rc }

    | SendCall(methodName, methodParams, callTimeout, replyChannel) ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            log (RpcWarn(sprintf "SendCall: dropping '%s', transport is shutting down" methodName))
            replyChannel.Reply(Error(makeError -32099 "Transport shut down"))
            state
        | Active ->
            let id = state.NextOutboundRequestId

            let payload =
                makePayload (fun w ->
                    w.WriteStartObject()
                    w.WriteString("jsonrpc", "2.0")
                    w.WriteNumber("id", id)
                    w.WriteString("method", methodName)
                    w.WritePropertyName("params")
                    methodParams.WriteTo(w)
                    w.WriteEndObject())

            let pendingCall =
                { MethodName = methodName
                  Timeout = callTimeout
                  Deadline = callTimeout |> Option.map (fun t -> DateTimeOffset.UtcNow + t)
                  ReplyChannel = replyChannel }

            let newState =
                { state with
                    NextOutboundRequestId = id + 1
                    PendingOutboundCalls = state.PendingOutboundCalls |> Map.add id pendingCall }

            newState
            |> enqueueOutboundMessage
                postEvent
                { Payload = payload
                  CompletionRC = None }
            |> rescheduleTimer postEvent

    | HandlerTerminated(ordinal, outcome) ->
        let ctx = state.RunningInboundHandlers |> Map.tryFind ordinal

        let newCallsByWireId =
            match ctx |> Option.bind _.WireId with
            | Some wireId -> state.RunningInboundCallsByWireId |> Map.remove (wireId.GetRawText())
            | None -> state.RunningInboundCallsByWireId

        let newState =
            { state with
                RunningInboundHandlers = state.RunningInboundHandlers |> Map.remove ordinal
                RunningInboundCallsByWireId = newCallsByWireId }

        let newState =
            let wireId = ctx |> Option.bind _.WireId

            match wireId with
            | None -> newState
            | Some wireId ->
                let responsePayload =
                    match outcome with
                    | HandlerReturnedOk returnValue ->
                        makePayload (fun w ->
                            w.WriteStartObject()
                            w.WriteString("jsonrpc", "2.0")
                            w.WritePropertyName("id")
                            wireId.WriteTo(w)
                            w.WritePropertyName("result")
                            returnValue.WriteTo(w)
                            w.WriteEndObject())

                    | HandlerReturnedError error -> makeErrorPayload wireId error

                    | HandlerFailed ex ->
                        let errorMessage =
                            sprintf
                                "processEvent: handler failed for request w/ id='%s': %s"
                                (wireId.GetRawText())
                                (string ex)

                        log (RpcError(errorMessage))

                        makeErrorPayload wireId (makeError -32603 (sprintf "Internal error: %s" (ex.GetType().Name)))

                    | HandlerCancelled -> makeErrorPayload wireId (makeError -32800 "Request cancelled")

                newState
                |> enqueueOutboundMessage
                    postEvent
                    { Payload = responsePayload
                      CompletionRC = None }

        match ctx with
        | Some ctx ->
            ctx.CancellationTokenSource |> Option.iter _.Dispose()
            ctx.Payload.Dispose()
        | None -> ()

        tryFireShutdownWaiters newState

    | AwaitShutdown rc ->
        { state with
            ShutdownWaiters = rc :: state.ShutdownWaiters }

    | CancelRequest wireIdKey ->
        let cancelCts (cts: CancellationTokenSource) =
            try
                cts.Cancel()
            with ex ->
                log (RpcError(sprintf "processEvent: error cancelling request %s: %s" wireIdKey (string ex)))

        state.RunningInboundCallsByWireId
        |> Map.tryFind wireIdKey
        |> Option.bind (fun ordinal -> state.RunningInboundHandlers |> Map.tryFind ordinal)
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

                log (
                    RpcWarn(
                        sprintf
                            "CheckTimeouts: call id=%d method='%s' timed out after %s"
                            id
                            pending.MethodName
                            timeoutDesc
                    )
                )

                pending.ReplyChannel.Reply(Error(makeError -32000 "Call timed out"))

            // Stamp expired IDs into RecentlyTimedOutCalls, then prune entries older than 60 s.
            let ttl = TimeSpan.FromSeconds 60.0

            let stamped =
                (state.RecentlyTimedOutCalls, expired)
                ||> Map.fold (fun acc id _ -> acc |> Map.add id now)

            let pruned = stamped |> Map.filter (fun _ stamped -> now - stamped <= ttl)

            let newState =
                { state with
                    PendingOutboundCalls = live
                    RecentlyTimedOutCalls = pruned }

            newState |> rescheduleTimer postEvent

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
              RunningInboundRequestCount = state.RunningInboundHandlers.Count
              TimerArmed = state.Timer |> Option.isSome
              RecentlyTimedOutCallCount = state.RecentlyTimedOutCalls.Count }
            : JsonRpcStats
        )

        state

let eventLoop (initialState: JsonRpcTransportState) (inbox: MailboxProcessor<JsonRpcTransportEvent>) =
    let rec loop state = async {
        let! ev = inbox.Receive()

        let newState =
            try
                state |> processEvent inbox.Post ev
            with ex ->
                // Call the callback directly — posting back through the mailbox won't work
                // because raise ex kills the loop before the posted event is ever processed.
                let log = state.RpcLogEntryCallback |> Option.defaultValue ignore

                log (RpcError(sprintf "eventLoop: processEvent crashed: %s" (string ex)))
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
