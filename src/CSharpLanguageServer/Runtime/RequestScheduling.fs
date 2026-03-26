module CSharpLanguageServer.Runtime.RequestScheduling

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "Runtime.RequestScheduling"

type RequestQueueMode =
    | Dispatching
    | DrainingUpTo of int64

type RequestMetrics =
    { Count: int
      TotalDuration: TimeSpan
      MaxDuration: TimeSpan }

    static member Zero =
        { Count = 0
          TotalDuration = TimeSpan.Zero
          MaxDuration = TimeSpan.Zero }

/// Manages the scheduling and lifecycle of LSP requests.
///
/// Request lifecycle:
/// - A request enters the queue in `Pending` state, tracked by its RPC ordinal.
/// - It is activated by sending a `ServerRequestContext` (produced by
///   `makeServerRequestContext`) through its `ActivationRC` reply channel,
///   transitioning it to `Running`.
/// - When the handler completes (or fails), the request moves to `Finished`
///   and its buffered events are stored on the `ServerRequest` record.
/// - Finished requests are retired one at a time in ordinal order (oldest
///   first). Retiring replays the request's buffered `ServerEvent`s into the
///   state loop so they are applied to the current server state.
///
/// Event buffering:
/// - Handlers call `ServerRequestContext.Emit` to record state-changing events.
///   These are buffered in-memory on the context (not sent to the state actor
///   immediately) and only replayed when the request is retired, preserving
///   the serial mutation invariant.
///
/// Background read-only requests:
/// - Long-running read-only requests (e.g. `workspace/diagnostics`) that never
///   emit state changes are assigned `ReadOnlyBackground` mode.
/// - `ReadOnlyBackground` requests do not block retirement of later requests
///   and may be retired out of order (they are simply skipped over during the
///   ordinal walk).
/// - Calling `ServerRequestContext.Emit` on a `ReadOnlyBackground` request
///   throws `InvalidOperationException`.
///
/// Concurrency rules:
/// - Write requests start and retire serially; a write request may only start
///   once all previously registered requests (read-only or read/write) have
///   been retired.
/// - Multiple read-only requests may share the same server state and run
///   concurrently. "Read-only" means the solution is not mutated, but the
///   handler may still emit events (e.g. `WorkspaceFolderChange`) that are
///   applied at retirement time.
/// - Read-only requests may start at any time, but not past the next pending
///   read/write request.
///
/// Queue modes and workspace reload:
/// - `Dispatching` — normal operation; pending requests are activated according
///   to the concurrency rules above.
/// - `DrainingUpTo(ordinal)` — the queue is draining in preparation for a
///   workspace reload. Only requests with an ordinal ≤ the drain ordinal are
///   eligible for activation. Once all such requests have been retired,
///   `processRequestQueue` returns `Drained`, the state loop posts
///   `RequestQueueDrained`, and the workspace is reloaded. The mode then
///   resets to `Dispatching`.
type RequestQueue =
    {
        Mode: RequestQueueMode
        /// The highest ordinal N such that all ordinals 1..N have been registered
        /// (whether still in the map or already retired). Requests with ordinals
        /// above the watermark must not be activated because the missing ordinals
        /// could turn out to be ReadWrite and would need to run first.
        WatermarkRpcOrdinal: int64
        Requests: Map<int64, ServerRequest>
        Stats: Map<string, RequestMetrics>
        LastStatsDumpTime: DateTime
    }

    static member Empty =
        { Mode = Dispatching
          WatermarkRpcOrdinal = 0L
          Requests = Map.empty
          Stats = Map.empty
          LastStatsDumpTime = DateTime.MinValue }

let updateRequestStats requestQueue (request: ServerRequest) (stats: RequestMetrics option) : RequestMetrics option =
    let requestExecutionDuration: TimeSpan =
        match request.RunningSince with
        | Some startTime -> DateTime.Now - startTime
        | None -> DateTime.Now - request.Registered

    match stats with
    | None ->
        { RequestMetrics.Zero with
            Count = 1
            TotalDuration = requestExecutionDuration
            MaxDuration = requestExecutionDuration }
        |> Some
    | Some s ->
        Some
            { s with
                Count = s.Count + 1
                TotalDuration = s.TotalDuration + requestExecutionDuration
                MaxDuration = max s.MaxDuration requestExecutionDuration }

let formatRequestQueueStats requestQueue =
    let calculateRequestStatsMetrics (name, metrics) =
        let avgDurationMs =
            if metrics.Count > 0 then
                metrics.TotalDuration.TotalMilliseconds / float metrics.Count
            else
                0.0

        (name, metrics, avgDurationMs)

    let sortedStats =
        requestQueue.Stats
        |> Map.toList
        |> List.map calculateRequestStatsMetrics
        |> List.sortByDescending (fun (_, _, avgDuration) -> avgDuration)

    let formatStatsRowWithImpact (name, metrics, avgDurationMs: float) =
        [ $"\"{name}\""
          metrics.Count |> string
          avgDurationMs.ToString("F2")
          metrics.MaxDuration.TotalMilliseconds.ToString("F2") ]

    let headerRow = [ "Name"; "Count"; "AvgDuration (ms)"; "MaxDuration (ms)" ]

    let dataRows = sortedStats |> List.map formatStatsRowWithImpact

    formatInColumns (headerRow :: dataRows)

let formatCurrentRequests (requestQueue: RequestQueue) =
    let sortedRequests = requestQueue.Requests |> Map.toList |> List.sortBy fst

    let formatState =
        function
        | Pending -> "Pending"
        | Running -> "Running"
        | Finished -> "Finished"

    let formatMode =
        function
        | ReadOnly -> "RO"
        | ReadWrite -> "RW"
        | ReadOnlyBackground -> "ROBg"

    let formatDuration (request: ServerRequest) =
        let elapsed =
            match request.RunningSince with
            | Some start -> DateTime.Now - start
            | None -> DateTime.Now - request.Registered

        elapsed.TotalMilliseconds.ToString("F0")

    let headerRow = [ "Ordinal"; "Name"; "Mode"; "State"; "Duration (ms)"; "Events" ]

    let dataRows =
        sortedRequests
        |> List.map (fun (ordinal, r) ->
            [ string ordinal
              $"\"{r.Name}\""
              formatMode r.Mode
              formatState r.State
              formatDuration r
              string r.BufferedEvents.Length ])

    formatInColumns (headerRow :: dataRows)

let dumpAndResetRequestStats (debugMode: bool) (requestQueue: RequestQueue) : RequestQueue =
    let statsDumpDeadline = requestQueue.LastStatsDumpTime + TimeSpan.FromMinutes(1.0)

    if debugMode && statsDumpDeadline < DateTime.Now then
        let modeLabel =
            match requestQueue.Mode with
            | Dispatching -> "Dispatching"
            | DrainingUpTo ord -> $"DrainingUpTo({ord})"

        if not (Map.isEmpty requestQueue.Requests) then
            logger.LogDebug("------ Current Requests ({mode}) ------", modeLabel)
            logger.LogDebug("{requests}", (requestQueue |> formatCurrentRequests))
            logger.LogDebug("---------------------------------------")

        if not (Map.isEmpty requestQueue.Stats) then
            logger.LogDebug("--------- Request Stats ---------")
            logger.LogDebug("{stats}", (requestQueue |> formatRequestQueueStats))
            logger.LogDebug("---------------------------------")
        else
            logger.LogDebug("------- No request stats  -------")

        { requestQueue with
            Stats = Map.empty
            LastStatsDumpTime = DateTime.Now }
    else
        requestQueue

let registerRequest requestRpcOrdinal requestName requestMode activationReplyChannel (requestQueue: RequestQueue) =
    let newRequest =
        { State = Pending
          Name = requestName
          RpcOrdinal = requestRpcOrdinal
          Mode = requestMode
          Registered = DateTime.Now
          ActivationRC = activationReplyChannel
          RunningSince = None
          BufferedEvents = [] }

    let newRequests = requestQueue.Requests |> Map.add requestRpcOrdinal newRequest

    // Advance the watermark past any now-contiguous ordinals.
    let rec advanceWatermark watermark =
        if newRequests |> Map.containsKey (watermark + 1L) then
            advanceWatermark (watermark + 1L)
        else
            watermark

    { requestQueue with
        Requests = newRequests
        WatermarkRpcOrdinal = advanceWatermark requestQueue.WatermarkRpcOrdinal }

/// Transitions a running request to Finished and stores its buffered events.
let finishRequest requestRpcOrdinal (bufferedEvents: ServerEvent list) (requestQueue: RequestQueue) =
    let request = requestQueue.Requests |> Map.find requestRpcOrdinal

    let finishedRequest =
        { request with
            State = Finished
            BufferedEvents = bufferedEvents }

    { requestQueue with
        Requests = requestQueue.Requests |> Map.add requestRpcOrdinal finishedRequest }

/// Tries to retire the next eligible finished request from the queue.
///
/// Walks requests from lowest ordinal. A finished `ReadOnlyBackground` request
/// is retired immediately (it has no buffered events). A finished non-background
/// request is retired and removed. A still-running `ReadOnlyBackground` request
/// is skipped over (it never blocks retirement). Any other non-finished request
/// stops the walk.
///
/// Returns `(updatedQueue, Some retiredRequest)` if a request was retired, or
/// `(requestQueue, None)` if nothing is eligible.
let retireNextFinishedRequest
    (config: CSharpConfiguration)
    (requestQueue: RequestQueue)
    : ServerRequest option * RequestQueue =

    let sortedRequests = requestQueue.Requests |> Map.toList |> List.sortBy fst

    let rec findRetirable remaining =
        match remaining with
        | [] -> None
        | (ordinal, request) :: rest ->
            match request.State, request.Mode with
            | Finished, _ -> Some(ordinal, request)
            | _, ReadOnlyBackground -> findRetirable rest
            | _ -> None

    let debugMode = config.debug |> Option.bind _.debugMode |> Option.defaultValue false

    match findRetirable sortedRequests with
    | None -> (None, requestQueue)
    | Some(ordinal, request) ->
        let newStats =
            if debugMode then
                requestQueue.Stats
                |> Map.change request.Name (updateRequestStats requestQueue request)
            else
                requestQueue.Stats

        let updatedQueue =
            { requestQueue with
                Requests = requestQueue.Requests |> Map.remove ordinal
                Stats = newStats }

        (Some request, updatedQueue)

/// Enters `DrainingUpTo` mode if not already draining. Returns `Some` with
/// the updated queue, or `None` if already in draining mode.
let enterDrainingMode (requestQueue: RequestQueue) : RequestQueue option =
    match requestQueue.Mode with
    | DrainingUpTo _ -> None
    | Dispatching ->
        let maxOrd =
            if Map.isEmpty requestQueue.Requests then
                0L
            else
                requestQueue.Requests |> Map.keys |> Seq.max

        Some
            { requestQueue with
                Mode = DrainingUpTo maxOrd }

/// Resets the queue to `Dispatching` mode.
let enterDispatchingMode (requestQueue: RequestQueue) : RequestQueue =
    { requestQueue with Mode = Dispatching }

/// Activates a request: replies on its channel, marks it as `Running`, and
/// returns the updated queue.
let activateRequest
    (makeServerRequestContext: ServerRequestMode -> ServerRequestContext)
    (ordinal: int64)
    (request: ServerRequest)
    (requestQueue: RequestQueue)
    : ServerRequest * RequestQueue =

    let serverRequestCtx = makeServerRequestContext request.Mode
    request.ActivationRC.Reply(serverRequestCtx :> obj)

    let activatedRequest =
        { request with
            State = Running
            RunningSince = Some DateTime.Now
            BufferedEvents = [] }

    activatedRequest,
    { requestQueue with
        Requests = requestQueue.Requests |> Map.add ordinal activatedRequest }

/// Returns `true` when all non-background requests up to the drain ordinal
/// have been retired. `ReadOnlyBackground` requests are excluded because they
/// never emit state-changing events and must not block workspace reloads.
let isDrained (requestQueue: RequestQueue) : bool =
    match requestQueue.Mode with
    | Dispatching -> false
    | DrainingUpTo maxOrd ->
        requestQueue.Requests
        |> Map.exists (fun ord r -> ord <= maxOrd && r.Mode <> ReadOnlyBackground)
        |> not

/// Returns the sorted list of pending requests eligible for activation,
/// respecting the drain ordinal when in `DrainingUpTo` mode.
let eligiblePendingRequests (requestQueue: RequestQueue) : (int64 * ServerRequest) list =
    let allPending =
        requestQueue.Requests
        |> Map.toList
        |> List.filter (fun (_, r) -> r.State = Pending)
        |> List.sortBy fst

    match requestQueue.Mode with
    | DrainingUpTo maxOrd -> allPending |> List.filter (fun (ord, _) -> ord <= maxOrd)
    | Dispatching -> allPending

/// Determines whether a pending request can be activated given the current
/// queue state.
let canActivateRequest
    (requestQueue: RequestQueue)
    (pendingRequests: (int64 * ServerRequest) list)
    (requestRpcOrdinal: int64, request: ServerRequest)
    : bool =
    // Never activate a request past the watermark — there are gaps (missing
    // ordinals) before it that could later be filled by ReadWrite requests.
    if requestRpcOrdinal > requestQueue.WatermarkRpcOrdinal then
        false
    else

        match request.Mode with
        | ReadWrite ->
            // A write request can only start when there are no other non-background
            // running requests. ReadOnlyBackground requests never emit events and
            // must not block write requests (or workspace reloads).
            requestQueue.Requests
            |> Map.exists (fun _ r -> r.State = Running && r.Mode <> ReadOnlyBackground)
            |> not

        | ReadOnly
        | ReadOnlyBackground ->
            // A read-only request can start concurrently with other read-only
            // requests, but:
            //   - not while a write request is running
            //   - not past the next pending write request
            let hasRunningWrite =
                requestQueue.Requests
                |> Map.exists (fun _ r -> r.State = Running && r.Mode = ReadWrite)

            if hasRunningWrite then
                false
            else
                let firstPendingWriteOrdinal =
                    pendingRequests
                    |> List.tryFind (fun (_, r) -> r.Mode = ReadWrite)
                    |> Option.map fst

                match firstPendingWriteOrdinal with
                | Some writeOrd -> requestRpcOrdinal < writeOrd
                | None -> true

type ProcessRequestQueueResult =
    | Waiting
    | Activated of ServerRequest * RequestQueue
    | Drained

let processRequestQueue
    (makeServerRequestContext: ServerRequestMode -> ServerRequestContext)
    (requestQueue: RequestQueue)
    : Async<ProcessRequestQueueResult> =
    async {
        if isDrained requestQueue then
            return Drained
        else
            let pendingRequests = eligiblePendingRequests requestQueue

            let pendingRequestToActivate =
                pendingRequests
                |> List.tryFind (canActivateRequest requestQueue pendingRequests)

            match pendingRequestToActivate with
            | None -> return Waiting
            | Some(ordinal, request) ->
                let activatedRequest, updatedQueue =
                    activateRequest makeServerRequestContext ordinal request requestQueue

                return Activated(activatedRequest, updatedQueue)
    }
