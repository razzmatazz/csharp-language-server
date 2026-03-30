module CSharpLanguageServer.Runtime.JsonRpc

open System
open System.IO
open System.Text
open System.Threading

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Mappings
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

type JsonRpcWriteQueue =
    | NormalWriteQueue
    | LowPriorityWriteQueue

type JsonRpcCallHandler = JsonRpcRequestContext -> Async<Result<JToken, JToken>>

type JsonRpcCallHandlerMap = Map<string, JsonRpcWriteQueue * JsonRpcCallHandler>

type JsonRpcNotificationHandler = JsonRpcRequestContext -> Async<unit>

type JsonRpcNotificationHandlerMap = Map<string, JsonRpcNotificationHandler>

type JsonRpcTransportState =
    {
        StdIn: Stream option
        StdOut: Stream option
        PendingRead: Async<unit> option
        PendingWrite: Async<unit> option
        NormalWriteQueue: JObject list
        LowPriorityWriteQueue: JObject list
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
    }

let emptyTransportState =
    { StdIn = None
      StdOut = None
      PendingRead = None
      PendingWrite = None
      NormalWriteQueue = []
      LowPriorityWriteQueue = []
      CallHandlers = Map.empty
      NotificationHandlers = Map.empty
      NextOutboundRequestId = 1
      NextInboundRequestOrdinal = 0
      PendingOutboundCalls = Map.empty
      RunningInboundRequests = Map.empty
      ShutdownWaiters = []
      RpcLogEntryCallback = None }

type JsonRpcTransportEvent =
    | Start of Stream * Stream * JsonRpcCallHandlerMap * JsonRpcNotificationHandlerMap
    | Shutdown
    | InboundMessage of Result<JObject option, Exception>
    | WriteComplete
    | SendNotification of method: string * methodParams: JToken * writeQueue: JsonRpcWriteQueue
    | SendCall of
        method: string *
        methodParams: JToken *
        writeQueue: JsonRpcWriteQueue *
        AsyncReplyChannel<Result<JToken, JToken>>
    | HandlerCompleted of wireIdKey: string * writeQueue: JsonRpcWriteQueue * result: Result<JToken, JToken>
    | HandlerFailed of wireIdKey: string * writeQueue: JsonRpcWriteQueue * exn
    | HandlerCancelled of wireIdKey: string * writeQueue: JsonRpcWriteQueue
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

let writeMessage (stdout: Stream) (msg: JObject) = async {
    let responseBytes = msg |> string |> Encoding.UTF8.GetBytes

    let header =
        sprintf "Content-Length: %d\r\n\r\n" responseBytes.Length
        |> Encoding.ASCII.GetBytes

    do! stdout.WriteAsync(header, 0, header.Length) |> Async.AwaitTask
    do! stdout.WriteAsync(responseBytes, 0, responseBytes.Length) |> Async.AwaitTask
    do! stdout.FlushAsync() |> Async.AwaitTask
}

let startWrite postEvent (stdout: Stream) (msg: JObject) =
    let writeOp = async {
        try
            do! writeMessage stdout msg
            postEvent (WriteRpcLogEntry(RpcWrite msg))
        with ex ->
            postEvent (WriteRpcLogEntry(RpcError(sprintf "startWrite: write error: %s" (string ex))))

        postEvent WriteComplete
    }

    Async.Start writeOp
    writeOp

/// Enqueue a message for writing. If no write is in progress, start one immediately;
/// otherwise append to the appropriate queue based on write queue/priority.
/// Normal-priority messages are drained before low-priority ones.
let enqueueWrite postEvent (state: JsonRpcTransportState) (writeQueue: JsonRpcWriteQueue) (msg: JObject) =
    match state.PendingWrite, state.StdOut with
    | None, Some stdout ->
        let writeOp = startWrite postEvent stdout msg

        { state with
            PendingWrite = Some writeOp }
    | _, _ ->
        match writeQueue with
        | NormalWriteQueue ->
            { state with
                NormalWriteQueue = state.NormalWriteQueue @ [ msg ] }
        | LowPriorityWriteQueue ->
            { state with
                LowPriorityWriteQueue = state.LowPriorityWriteQueue @ [ msg ] }

let makeError (code: int) (message: string) =
    JObject(JProperty("code", code), JProperty("message", message))

let makeErrorResponse (id: JToken) (error: JToken) =
    JObject(JProperty("jsonrpc", "2.0"), JProperty("id", id), JProperty("error", error))

/// Start an inbound request handler asynchronously with a CancellationTokenSource.
/// The handler runs on the thread pool; on completion/failure/cancellation, an event
/// is posted back to the actor.
let startCallHandler postEvent writeQueuePrio (handler: JsonRpcCallHandler) (context: JsonRpcRequestContext) =
    let wireIdKey = string context.WireId.Value
    let cts = context.CancellationTokenSource

    let handlerAsync = async {
        let! response = handler context
        postEvent (HandlerCompleted(wireIdKey, writeQueuePrio, response))
    }

    Async.StartWithContinuations(
        handlerAsync,
        (fun () -> ()), // success: already posted HandlerCompleted inside the async
        (fun ex ->
            match ex with
            | :? OperationCanceledException -> postEvent (HandlerCancelled (wireIdKey, writeQueuePrio))
            | _ -> postEvent (HandlerFailed(wireIdKey, writeQueuePrio, ex))),
        (fun _cancelEx -> postEvent (HandlerCancelled (wireIdKey, writeQueuePrio))),
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
            | Some (writeQueuePrio, handler) ->
                let cts = new CancellationTokenSource()

                let context =
                    { MethodName = m
                      RequestOrdinal = state.NextInboundRequestOrdinal
                      WireId = Some requestId
                      Params = msg.SelectToken("params") |> Option.ofObj
                      CancellationTokenSource = Some cts }

                do startCallHandler postEvent writeQueuePrio handler context

                { state with
                    NextInboundRequestOrdinal = state.NextInboundRequestOrdinal + (int64 1)
                    RunningInboundRequests = state.RunningInboundRequests |> Map.add wireIdKey context }
            | None ->
                let errorResponse =
                    makeErrorResponse requestId (makeError -32601 (sprintf "Method not found: '%s'" m))

                enqueueWrite postEvent state NormalWriteQueue errorResponse
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

    | WriteComplete ->
        match state.NormalWriteQueue, state.LowPriorityWriteQueue, state.StdOut with
        | nextMsg :: rest, _, Some stdout ->
            let writeOp = startWrite postEvent stdout nextMsg

            { state with
                PendingWrite = Some writeOp
                NormalWriteQueue = rest }
        | [], nextMsg :: rest, Some stdout ->
            let writeOp = startWrite postEvent stdout nextMsg

            { state with
                PendingWrite = Some writeOp
                LowPriorityWriteQueue = rest }
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
                state.ShutdownWaiters |> List.iter (fun rc -> rc.Reply())

                { state with
                    StdIn = None
                    PendingRead = None
                    ShutdownWaiters = [] }
        | Error ex ->
            let pendingRead = state.StdIn |> Option.map (startRead postEvent)
            { state with PendingRead = pendingRead }

    | Shutdown ->
        state.ShutdownWaiters |> List.iter (fun rc -> rc.Reply())

        { state with
            StdIn = None
            PendingWrite = None
            PendingRead = None
            ShutdownWaiters = [] }

    | SendNotification(method, methodParams, priority) ->
        let msg =
            JObject(JProperty("jsonrpc", "2.0"), JProperty("method", method), JProperty("params", methodParams))

        enqueueWrite postEvent state priority msg

    | SendCall(method, methodParams, writeQueuePrio, replyChannel) ->
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

        enqueueWrite postEvent newState writeQueuePrio msg

    | HandlerCompleted(wireIdKey, writeQueuePrio, result) ->
        let ctx = state.RunningInboundRequests |> Map.find wireIdKey
        do ctx.CancellationTokenSource |> Option.iter _.Dispose()

        let newState =
            { state with
                RunningInboundRequests = state.RunningInboundRequests |> Map.remove wireIdKey }

        match result with
        | Ok returnValue ->
            let response =
                JObject(
                    JProperty("jsonrpc", "2.0"),
                    JProperty("id", ctx.WireId.Value),
                    JProperty("result", returnValue)
                )

            enqueueWrite postEvent newState writeQueuePrio response
        | Error error ->
            let errorResponse = makeErrorResponse ctx.WireId.Value error

            enqueueWrite postEvent newState writeQueuePrio errorResponse

    | HandlerFailed(wireIdKey, writeQueue, ex) ->
        postEvent (
            WriteRpcLogEntry(RpcError(sprintf "processEvent: handler failed for request %s: %s" wireIdKey (string ex)))
        )

        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        let cts = ctx |> Option.bind _.CancellationTokenSource
        do cts |> Option.iter _.Dispose()

        let newState =
            { state with
                RunningInboundRequests = state.RunningInboundRequests |> Map.remove wireIdKey }

        match ctx with
        | Some ctx ->
            let errorResponse =
                makeErrorResponse ctx.WireId.Value (makeError -32603 (sprintf "Internal error: %s" (ex.GetType().Name)))

            enqueueWrite postEvent newState writeQueue errorResponse
        | None -> newState

    | HandlerCancelled (wireIdKey, writeQueue) ->
        let ctx = state.RunningInboundRequests |> Map.tryFind wireIdKey

        ctx |> Option.bind _.CancellationTokenSource |> Option.iter _.Dispose()

        let newState =
            { state with
                RunningInboundRequests = state.RunningInboundRequests |> Map.remove wireIdKey }

        match ctx with
        | Some ctx ->
            let errorResponse =
                makeErrorResponse ctx.WireId.Value (makeError -32800 "Request cancelled")

            enqueueWrite postEvent newState writeQueue errorResponse
        | None -> newState

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

let sendJsonRpcNotification
    (transport: MailboxProcessor<JsonRpcTransportEvent>)
    (method: string)
    (methodParams: JToken)
    =
    transport.Post(SendNotification(method, methodParams, NormalWriteQueue))

let sendJsonRpcNotificationWithLowPriority
    (transport: MailboxProcessor<JsonRpcTransportEvent>)
    (method: string)
    (methodParams: JToken)
    =
    transport.Post(SendNotification(method, methodParams, LowPriorityWriteQueue))

let sendJsonRpcCall (transport: MailboxProcessor<JsonRpcTransportEvent>) (method: string) (methodParams: JToken) =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, NormalWriteQueue, rc))

let sendJsonRpcCallWithLowPriority
    (transport: MailboxProcessor<JsonRpcTransportEvent>)
    (method: string)
    (methodParams: JToken)
    =
    transport.PostAndAsyncReply(fun rc -> SendCall(method, methodParams, LowPriorityWriteQueue, rc))

let awaitJsonRpcTransportShutdown (transport: MailboxProcessor<JsonRpcTransportEvent>) =
    transport.PostAndAsyncReply(AwaitShutdown)
