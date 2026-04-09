module CSharpLanguageServer.Runtime.JsonRpc

open System
open System.IO
open System.Text
open System.Threading

open Newtonsoft.Json.Linq

open CSharpLanguageServer.Types

type JsonRpcLogEntry =
    | RpcRead of JObject
    | RpcWrite of JObject
    | RpcError of string
    | RpcWarn of string
    | RpcDebug of string

type JsonRpcRequestContext =
    { MethodName: string
      RequestOrdinal: int64
      WireId: JToken option
      Params: JToken option
      CancellationTokenSource: CancellationTokenSource option }

type JsonRpcCallHandler = JsonRpcRequestContext -> Async<Result<JToken, JToken>>

type JsonRpcCallHandlerMap = Map<string, JsonRpcCallHandler>

type JsonRpcNotificationHandler = JsonRpcRequestContext -> Async<unit>

type JsonRpcNotificationHandlerMap = Map<string, JsonRpcNotificationHandler>

type OutboundMessage =
    { Payload: JObject
      CompletionRC: option<AsyncReplyChannel<unit>> }

type TransportPhase =
    | Active
    | ShuttingDown // Shutdown received, handler cancellations fired, waiting for RunningInboundRequests to drain
    | Stopped // all handlers drained, ShutdownWaiters fired

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
        PendingOutboundCalls: Map<int, AsyncReplyChannel<Result<JToken, JToken>>>
        /// Inbound requests currently being handled, keyed by wire ID (string representation).
        RunningInboundRequests: Map<string, JsonRpcRequestContext>
        /// Channels waiting to be notified when the transport shuts down.
        ShutdownWaiters: AsyncReplyChannel<unit> list
        /// Optional callback invoked with each raw RPC log entry.
        RpcLogEntryCallback: (JsonRpcLogEntry -> unit) option
        Phase: TransportPhase
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
      Phase = Active }

type JsonRpcTransportEvent =
    | Start of Stream * Stream * Map<string, JsonRpcCallHandler> * Map<string, JsonRpcNotificationHandler>
    | Shutdown of AsyncReplyChannel<unit>
    | InboundMessage of Result<JObject option, Exception>
    | MessageWriteComplete of success: bool * message: OutboundMessage
    | SendNotification of method: string * methodParams: JToken * AsyncReplyChannel<unit>
    | SendCall of method: string * methodParams: JToken * AsyncReplyChannel<Result<JToken, JToken>>
    | HandlerCompleted of wireIdKey: string * result: Result<JToken, JToken>
    | HandlerFailed of wireIdKey: string * exn
    | HandlerCancelled of wireIdKey: string
    | CancelRequest of wireIdKey: string
    | NotificationHandlerFailed of method: string * exn
    | AwaitShutdown of AsyncReplyChannel<unit>
    | WriteRpcLogEntry of JsonRpcLogEntry

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
            let json = Encoding.UTF8.GetString(buffer)
            return Some(JObject.Parse json)
}

let startRead postEvent (stdin: Stream) =
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
        let responseBytes = msg.Payload |> string |> Encoding.UTF8.GetBytes

        let header =
            sprintf "Content-Length: %d\r\n\r\n" responseBytes.Length
            |> Encoding.ASCII.GetBytes

        do! stdout.WriteAsync(header, 0, header.Length) |> Async.AwaitTask
        do! stdout.WriteAsync(responseBytes, 0, responseBytes.Length) |> Async.AwaitTask
        do! stdout.FlushAsync() |> Async.AwaitTask

        postEvent (WriteRpcLogEntry(RpcWrite msg.Payload))
        postEvent (MessageWriteComplete(true, msg))
    with ex ->
        postEvent (WriteRpcLogEntry(RpcError(sprintf "startWrite: write error: %s" (string ex))))
        postEvent (MessageWriteComplete(false, msg))
}

let startMessageWrite postEvent (stdout: Stream) msg =
    let writeOp = (writeMessage postEvent stdout msg)
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

let makeError (code: int) (message: string) =
    JObject(JProperty("code", code), JProperty("message", message))

/// Drain all pending outbound calls by replying with a synthetic transport-shutdown error.
/// Call this from any shutdown path before clearing PendingOutboundCalls.
let failPendingOutboundCalls postEvent (state: JsonRpcTransportState) =
    let shutdownErr = makeError -32099 "Transport shut down" :> JToken

    for KeyValue(id, replyChannel) in state.PendingOutboundCalls do
        let rpcLogEntry =
            RpcWarn(sprintf "failPendingOutboundCalls: failing pending outbound call id=%d on shutdown" id)

        postEvent (WriteRpcLogEntry rpcLogEntry)

        replyChannel.Reply(Error shutdownErr)

let makeErrorResponse (id: JToken) (error: JToken) =
    JObject(JProperty("jsonrpc", "2.0"), JProperty("id", id), JProperty("error", error))

/// Start an inbound request handler asynchronously with a CancellationTokenSource.
/// The handler runs on the thread pool; on completion/failure/cancellation, an event
/// is posted back to the actor.
let startCallHandler postEvent (handler: JsonRpcCallHandler) (context: JsonRpcRequestContext) =
    let wireIdKey = string context.WireId.Value
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

let handleInboundMessage state postEvent (msg: JObject) =
    let method =
        msg.SelectToken("method") |> Option.ofObj |> Option.map _.Value<string>()

    let id = msg.SelectToken("id") |> Option.ofObj

    match method with
    | Some m ->
        match id with
        | Some requestId ->
            // Request (has "id") — dispatch to request handlers asynchronously
            let wireIdKey = string requestId

            match state.CallHandlers |> Map.tryFind m with
            | Some handler ->
                let cts = new CancellationTokenSource()

                let context =
                    { MethodName = m
                      RequestOrdinal = state.NextInboundRequestOrdinal
                      WireId = Some requestId
                      Params = msg.SelectToken("params") |> Option.ofObj
                      CancellationTokenSource = Some cts }

                do startCallHandler postEvent handler context

                { state with
                    NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1)
                    RunningInboundRequests = state.RunningInboundRequests |> Map.add wireIdKey context }
            | None ->
                let errorResponse =
                    makeErrorResponse requestId (makeError -32601 (sprintf "Method not found: '%s'" m))

                enqueueOutboundMessage
                    postEvent
                    state
                    { Payload = errorResponse
                      CompletionRC = None }
        | None ->
            // Notification (no "id") — dispatch to notification handlers, never send a response
            // Special handling for $/cancelRequest
            if m = "$/cancelRequest" then
                let idParam = msg.SelectToken("params.id") |> Option.ofObj

                match idParam with
                | Some cancelId -> postEvent (CancelRequest(string cancelId))
                | None ->
                    postEvent (
                        WriteRpcLogEntry(RpcError "handleInboundMessage: $/cancelRequest missing 'id' parameter")
                    )

                state
            else
                let context =
                    { MethodName = m
                      RequestOrdinal = state.NextInboundRequestOrdinal
                      WireId = None
                      Params = msg.SelectToken("params") |> Option.ofObj
                      CancellationTokenSource = None }

                match state.NotificationHandlers |> Map.tryFind m with
                | Some handler -> do startNotificationHandler postEvent handler context
                | None ->
                    postEvent (
                        WriteRpcLogEntry(
                            RpcError(sprintf "handleInboundMessage: no notification handler for method '%s'" m)
                        )
                    )

                { state with
                    NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1) }
    | None ->
        match id with
        | Some responseId ->
            // No method + has id = this is a response to one of our outbound calls
            let idVal = int responseId

            match state.PendingOutboundCalls |> Map.tryFind idVal with
            | Some replyChannel ->
                match msg.SelectToken("error") |> Option.ofObj with
                | Some err -> replyChannel.Reply(Error err)
                | None ->
                    let result =
                        msg.SelectToken("result")
                        |> Option.ofObj
                        |> Option.defaultWith (fun () -> JValue.CreateNull() :> JToken)

                    replyChannel.Reply(Ok result)

                { state with
                    PendingOutboundCalls = state.PendingOutboundCalls |> Map.remove idVal }
            | None ->
                postEvent (
                    WriteRpcLogEntry(
                        RpcError(
                            sprintf "handleInboundMessage: received response for unknown outbound call id %d" idVal
                        )
                    )
                )

                state
        | None ->
            postEvent (WriteRpcLogEntry(RpcError "handleInboundMessage: message has no 'method' or 'id' field"))
            state

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
let completeInboundRequest postEvent (state: JsonRpcTransportState) wireIdKey (responsePayload: JObject option) =
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
                { Payload = payload
                  CompletionRC = None }
        | None -> newState

    tryFireShutdownWaiters newState

let processEvent state postEvent ev =
    match ev with
    | Start(stdin, stdout, callHandlers, notificationHandlers) ->
        let readOp = stdin |> startRead postEvent

        { state with
            StdIn = Some stdin
            StdOut = Some stdout
            CallHandlers = callHandlers
            NotificationHandlers = notificationHandlers
            PendingRead = Some readOp }

    | MessageWriteComplete(_success, msg) ->
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
        | Ok msg ->
            match msg with
            | Some msg ->
                postEvent (WriteRpcLogEntry(RpcRead msg))
                let updatedState = handleInboundMessage state postEvent msg

                let pendingRead = updatedState.StdIn |> Option.map (startRead postEvent)

                { updatedState with
                    PendingRead = pendingRead }

            | None ->
                // EOF on stdin — full shutdown: fail pending outbound calls, cancel all
                // running inbound handlers, move to ShuttingDown, then drain.
                // Unlike explicit Shutdown there is no reply channel to push, but any
                // existing AwaitShutdown waiters will be fired once the map empties.
                failPendingOutboundCalls postEvent state

                for KeyValue(wireIdKey, _) in state.RunningInboundRequests do
                    postEvent (CancelRequest wireIdKey)

                let state =
                    { state with
                        StdIn = None
                        PendingRead = None
                        PendingOutboundCalls = Map.empty
                        Phase = ShuttingDown }

                tryFireShutdownWaiters state
        | Error ex ->
            postEvent (WriteRpcLogEntry(RpcError(sprintf "InboundMessage: read error (retrying): %s" (string ex))))
            let pendingRead = state.StdIn |> Option.map (startRead postEvent)
            { state with PendingRead = pendingRead }

    | Shutdown rc ->
        // Stop accepting inbound messages and fail any pending outbound calls.
        failPendingOutboundCalls postEvent state

        // Cancel every in-flight inbound handler. Each one will eventually post
        // HandlerCancelled/HandlerCompleted/HandlerFailed back, and tryFireShutdownWaiters
        // will fire rc (and any AwaitShutdown waiters) once the map is empty.
        for KeyValue(wireIdKey, _) in state.RunningInboundRequests do
            postEvent (CancelRequest wireIdKey)

        // Park rc alongside any existing AwaitShutdown waiters — all fired together on drain.
        let state =
            { state with
                StdIn = None
                PendingRead = None
                PendingOutboundCalls = Map.empty
                Phase = ShuttingDown
                ShutdownWaiters = rc :: state.ShutdownWaiters }

        // If there are no running handlers at all, we are already drained.
        tryFireShutdownWaiters state

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
            let msg =
                JObject(JProperty("jsonrpc", "2.0"), JProperty("method", method), JProperty("params", methodParams))

            enqueueOutboundMessage
                postEvent
                state
                { Payload = msg
                  CompletionRC = Some rc }

    | SendCall(method, methodParams, replyChannel) ->
        match state.Phase with
        | ShuttingDown
        | Stopped ->
            postEvent (WriteRpcLogEntry(RpcWarn(sprintf "SendCall: dropping '%s', transport is shutting down" method)))
            replyChannel.Reply(Error(makeError -32099 "Transport shut down" :> JToken))
            state
        | Active ->
            let id = state.NextOutboundRequestId

            let msg =
                JObject(
                    JProperty("jsonrpc", "2.0"),
                    JProperty("id", id),
                    JProperty("method", method),
                    JProperty("params", methodParams)
                )

            let newState =
                { state with
                    NextOutboundRequestId = id + 1
                    PendingOutboundCalls = state.PendingOutboundCalls |> Map.add id replyChannel }

            enqueueOutboundMessage postEvent newState { Payload = msg; CompletionRC = None }

    | HandlerCompleted(wireIdKey, result) ->
        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        let responsePayload =
            ctx
            |> Option.map (fun ctx ->
                match result with
                | Ok returnValue ->
                    JObject(
                        JProperty("jsonrpc", "2.0"),
                        JProperty("id", ctx.WireId.Value),
                        JProperty("result", returnValue)
                    )
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

let sendJsonRpcNotification (server: MailboxProcessor<JsonRpcTransportEvent>) (method: string) (methodParams: JToken) =
    server.PostAndAsyncReply(fun rc -> SendNotification(method, methodParams, rc))

let sendJsonRpcCall (transport: MailboxProcessor<JsonRpcTransportEvent>) (method: string) (methodParams: JToken) =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, rc))

let shutdownJsonRpcTransport (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(Shutdown)

let awaitJsonRpcTransportShutdown (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(AwaitShutdown)
