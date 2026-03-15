module CSharpLanguageServer.Runtime.RequestScheduling

open System

open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "Runtime.RequestScheduling"

type RequestQueueMode = | Dispatching

type RequestMetrics =
    { Count: int
      TotalDuration: TimeSpan
      MaxDuration: TimeSpan
      ImpactedRequestsCount: int
      TotalImpactedWaitingTime: TimeSpan }

    static member Zero =
        { Count = 0
          TotalDuration = TimeSpan.Zero
          MaxDuration = TimeSpan.Zero
          ImpactedRequestsCount = 0
          TotalImpactedWaitingTime = TimeSpan.Zero }

type RequestQueue =
    { Mode: RequestQueueMode
      LastRequestId: int
      PendingRequests: ServerRequest list
      RunningRequests: Map<int, ServerRequest>
      Stats: Map<string, RequestMetrics>
      LastStatsDumpTime: DateTime }

    static member Empty =
        { Mode = Dispatching
          LastRequestId = 0
          PendingRequests = []
          RunningRequests = Map.empty
          Stats = Map.empty
          LastStatsDumpTime = DateTime.MinValue }

let updateRequestStats requestQueue request (stats: RequestMetrics option) : RequestMetrics option =
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
        let impactedCount, totalImpactedWait =
            if request.Mode.Value = ReadWrite then
                let blockingStartTime =
                    request.RunningSince |> Option.defaultValue request.Registered

                let aggregateBlockedRequestStats (count, totalWait) pendingRequest =
                    let waitStartTime = max blockingStartTime pendingRequest.Registered
                    let waitDurationForPending = DateTime.Now - waitStartTime

                    (count + 1, totalWait + waitDurationForPending)

                requestQueue.PendingRequests
                |> List.fold aggregateBlockedRequestStats (0, TimeSpan.Zero)
            else
                (0, TimeSpan.Zero)

        Some
            { s with
                Count = s.Count + 1
                TotalDuration = s.TotalDuration + requestExecutionDuration
                MaxDuration = max s.MaxDuration requestExecutionDuration
                ImpactedRequestsCount = s.ImpactedRequestsCount + impactedCount
                TotalImpactedWaitingTime = s.TotalImpactedWaitingTime + totalImpactedWait }

let formatRequestQueueStats requestQueue =
    let calculateRequestStatsMetrics (name, metrics) =
        let avgDurationMs =
            if metrics.Count > 0 then
                metrics.TotalDuration.TotalMilliseconds / float metrics.Count
            else
                0.0

        let avgImpactedWaitMs =
            if metrics.ImpactedRequestsCount > 0 then
                metrics.TotalImpactedWaitingTime.TotalMilliseconds
                / float metrics.ImpactedRequestsCount
            else
                0.0

        (name, metrics, avgImpactedWaitMs, avgDurationMs)

    let sortedStats =
        requestQueue.Stats
        |> Map.toList
        |> List.map calculateRequestStatsMetrics
        |> List.sortByDescending (fun (_, _, _, avgDuration) -> avgDuration)

    let formatStatsRowWithImpact (name, metrics, avgImpactedWaitMs: float, avgDurationMs: float) =
        [ $"\"{name}\""
          metrics.Count |> string
          avgDurationMs.ToString("F2")
          metrics.MaxDuration.TotalMilliseconds.ToString("F2")
          sprintf "%d (%s ms on avg)" metrics.ImpactedRequestsCount (avgImpactedWaitMs.ToString("F2")) ]

    let headerRow =
        [ "Name"; "Count"; "AvgDuration (ms)"; "MaxDuration (ms)"; "ImpactedRequests" ]

    let dataRows = sortedStats |> List.map formatStatsRowWithImpact

    formatInColumns (headerRow :: dataRows)

let dumpAndResetRequestStats (debugMode: bool) (requestQueue: RequestQueue) : RequestQueue =
    let statsDumpDeadline = requestQueue.LastStatsDumpTime + TimeSpan.FromMinutes(1.0)

    if debugMode && statsDumpDeadline < DateTime.Now then
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

let pullPendingRequestToActivateFromRequestQueue
    (workspaceReloadIsPending: bool)
    (requestQueue: RequestQueue)
    : option<ServerRequest * RequestQueue> =
    let noRWRequestRunning =
        requestQueue.RunningRequests
        |> Seq.tryFind (fun r -> r.Value.Mode = Some ReadWrite)
        |> Option.isNone

    let canPullNextRequest = noRWRequestRunning && (not workspaceReloadIsPending)

    match canPullNextRequest, requestQueue.PendingRequests with
    | false, _ -> None
    | true, [] -> None

    | true, nonEmptyRequestQueue ->
        // here we will try to take non-interrupted r/o request sequence at the front,
        // order it by priority and run the most prioritized one first
        let nextRoRequestByPriorityMaybe =
            nonEmptyRequestQueue
            |> Seq.takeWhile (fun r -> r.Mode = Some ReadOnly)
            |> Seq.tryHead

        // otherwise, if no r/o request by priority was found then we should just take the first request
        let nextRequest =
            nextRoRequestByPriorityMaybe
            |> Option.defaultValue (nonEmptyRequestQueue |> Seq.head)

        let newPendingRequests = nonEmptyRequestQueue |> List.except [ nextRequest ]

        let nextRunnableRequest =
            { nextRequest with
                RunningSince = Some DateTime.Now }

        let newRunningRequests =
            requestQueue.RunningRequests
            |> Map.add nextRunnableRequest.Id nextRunnableRequest

        let newRequestQueue =
            { requestQueue with
                RunningRequests = newRunningRequests
                PendingRequests = newPendingRequests }

        Some(nextRunnableRequest, newRequestQueue)

let registerRequestToRequestQueue requestName requestQueue =
    let newRequest =
        { State = Enqueued
          Name = requestName
          Id = requestQueue.LastRequestId + 1
          ActivationListeners = []
          Mode = None
          Registered = DateTime.Now
          RunningSince = None }

    let updatedRequestQueue =
        { requestQueue with
            LastRequestId = newRequest.Id
            PendingRequests = requestQueue.PendingRequests @ [ newRequest ] }

    newRequest, updatedRequestQueue

let addActivationListenerForRequestOnRequestQueue requestId activationReplyChannel requestQueue =
    let pendingRequest =
        requestQueue.PendingRequests |> List.tryFind (fun r -> r.Id = requestId)

    match pendingRequest with
    | Some request ->
        // add replyChannel to ActivationListeners for the request
        let updatedPendingRequests =
            requestQueue.PendingRequests
            |> List.map (fun r ->
                if r.Id = requestId then
                    { r with
                        ActivationListeners = activationReplyChannel :: r.ActivationListeners }
                else
                    r)

        let updatedRequestQueue =
            { requestQueue with
                PendingRequests = updatedPendingRequests }

        Some(request, updatedRequestQueue)

    | None -> None

let retireRequestFromRequestQueue serverSettings requestId requestQueue =
    let request = requestQueue.RunningRequests |> Map.tryFind requestId

    match request with
    | None -> None
    | Some request ->
        let newRequestStats =
            match serverSettings.DebugMode with
            | true ->
                requestQueue.Stats
                |> Map.change request.Name (updateRequestStats requestQueue request)
            | false -> requestQueue.Stats

        let newRunningRequests = requestQueue.RunningRequests |> Map.remove request.Id

        let newRequestQueue =
            { requestQueue with
                RunningRequests = newRunningRequests
                Stats = newRequestStats }

        Some newRequestQueue

let cleanupDeadlockedRequestsOnRequestQueue requestQueue =
    // the combination of StreamJsonRpc+Ionide.LanguageServerProtocol
    // does not allow us to catch request cancellation properly
    // when request is cancelled *before* async part of the handler
    // runs -- here we just go after old (100ms+) requests in Enqueued state
    // and upgrade them to Cancelled state directly
    let deadlockThreshold = TimeSpan.FromMilliseconds(100.0)

    let isDeadlockedROEnqueuedRequest (r: ServerRequest) =
        r.Mode <> Some ReadWrite
        && r.State = Enqueued
        && (DateTime.Now - r.Registered) > deadlockThreshold

    let haveDeadlockedRequests =
        requestQueue.PendingRequests |> List.exists isDeadlockedROEnqueuedRequest

    match haveDeadlockedRequests with
    | false -> None
    | true ->
        let newPendingRequests =
            requestQueue.PendingRequests
            |> List.collect (fun r -> if isDeadlockedROEnqueuedRequest r then [] else [ r ])

        let updatedRequestQueue =
            { requestQueue with
                PendingRequests = newPendingRequests }

        Some updatedRequestQueue

let processRequestQueue
    (workspaceReloadIsPending: bool)
    (serverState: obj)
    (workspace: LspWorkspace)
    (settings: ServerSettings)
    requestQueue
    : Async<RequestQueue option> =
    async {
        match pullPendingRequestToActivateFromRequestQueue workspaceReloadIsPending requestQueue with
        | None -> return None
        | Some(requestToRun, updatedRequestQueue) ->
            // notify all activation listeners with the current state
            requestToRun.ActivationListeners |> List.iter (fun rc -> rc.Reply(serverState))
            return Some updatedRequestQueue
    }
