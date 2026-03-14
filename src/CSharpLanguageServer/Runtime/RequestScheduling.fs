module CSharpLanguageServer.Runtime.RequestScheduling

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "Runtime.RequestScheduling"

type RequestQueueMode =
    | Dispatching
    | Flushing of int
    | Terminating

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
      Requests: ServerRequest list
      Stats: Map<string, RequestMetrics>
      LastStatsDumpTime: DateTime }

    static member Empty =
        { Mode = Dispatching
          LastRequestId = 0
          Requests = []
          Stats = Map.empty
          LastStatsDumpTime = DateTime.MinValue }

let updateRequestQueueStats queue request (stats: RequestMetrics option) : RequestMetrics option =
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
            if request.Mode = Some ReadWriteSequential then
                let blockingStartTime =
                    request.RunningSince |> Option.defaultValue request.Registered

                let aggregateBlockedRequestStats (count, totalWait) pendingRequest =
                    let waitStartTime = max blockingStartTime pendingRequest.Registered
                    let waitDurationForPending = DateTime.Now - waitStartTime

                    (count + 1, totalWait + waitDurationForPending)

                queue.Requests
                |> Seq.filter _.State.IsPending
                |> Seq.fold aggregateBlockedRequestStats (0, TimeSpan.Zero)
            else
                (0, TimeSpan.Zero)

        Some
            { s with
                Count = s.Count + 1
                TotalDuration = s.TotalDuration + requestExecutionDuration
                MaxDuration = max s.MaxDuration requestExecutionDuration
                ImpactedRequestsCount = s.ImpactedRequestsCount + impactedCount
                TotalImpactedWaitingTime = s.TotalImpactedWaitingTime + totalImpactedWait }

let requestStateName (state: ServerRequestState) =
    match state with
    | Enqueued -> "Enqueued"
    | Pending _ -> "Pending"
    | Running -> "Running"
    | Terminated _ -> "Terminated"

let formatRequestList (requests: ServerRequest list) : string =
    let header = sprintf "Request queue (%d)" requests.Length

    let rows =
        requests
        |> List.map (fun req ->
            sprintf " - #%d %s %s \"%s\"" req.Id (requestStateName req.State) (string req.Mode) (string req.Name))

    header :: rows |> String.concat "\n"

let formatRequestQueueStats requestQueue : string =
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

let registerRequestToRequestQueue requestName queue =
    let newRequest =
        { State = Enqueued
          Name = requestName
          Id = queue.LastRequestId + 1
          Registered = DateTime.Now
          RunningSince = None
          Mode = None
          TargetUri = None }

    let updatedQueue =
        { queue with
            LastRequestId = newRequest.Id
            Requests = queue.Requests @ [ newRequest ] }

    newRequest, updatedQueue

let activateRequestOnRequestQueue requestId mode targetUri replyChannel queue =
    let matchingRequest r = r.State = Enqueued && r.Id = requestId
    let request = queue.Requests |> List.tryFind matchingRequest

    match request with
    | Some request ->
        let requestAsPending =
            { request with
                State = Pending replyChannel
                Mode = Some mode
                TargetUri = targetUri }

        let newRequests =
            queue.Requests
            |> List.map (fun r -> if matchingRequest r then requestAsPending else r)

        let newQueue = { queue with Requests = newRequests }

        newQueue, Some request

    | None -> queue, None

let terminateRequestOnRequestQueue requestId outcome (emittedEvents: list<ServerEvent>) (queue: RequestQueue) =
    let matchingRequest (r: ServerRequest) = r.Id = requestId
    let request = queue.Requests |> List.tryFind matchingRequest

    match request with
    | Some request ->
        let terminatedRequest =
            let emittedEventsAsObj = emittedEvents |> List.map (fun x -> x :> obj)

            { request with
                State = Terminated(outcome, emittedEventsAsObj) }

        let newRequests =
            queue.Requests
            |> List.map (fun r -> if matchingRequest r then terminatedRequest else r)

        let newQueue = { queue with Requests = newRequests }
        newQueue, Some request

    | None -> queue, None

let cleanupDeadlockedRequestsOnRequestQueue queue =
    // the combination of StreamJsonRpc+Ionide.LanguageServerProtocol
    // does not allow us to catch request cancellation properly
    // when request is cancelled *before* async part of the handler
    // runs -- here we just go after old (100ms+) requests in Enqueued state
    // and upgrade them to Cancelled state directly
    let deadlockThreshold = TimeSpan.FromMilliseconds(100.0)

    let isDeadlockedROEnqueuedRequest (r: ServerRequest) =
        r.Mode <> Some ReadWriteSequential
        && r.State = Enqueued
        && (DateTime.Now - r.Registered) > deadlockThreshold

    let haveDeadlockedRequests =
        queue.Requests |> List.exists isDeadlockedROEnqueuedRequest

    match haveDeadlockedRequests with
    | false -> None
    | true ->
        let translateDeadlockedEnqueuedToTerminatedState r =
            if isDeadlockedROEnqueuedRequest r then
                { r with
                    State = Terminated(Cancelled, []) }
            else
                r

        let newRequests =
            queue.Requests |> List.map translateDeadlockedEnqueuedToTerminatedState

        let updatedQueue = { queue with Requests = newRequests }
        Some updatedQueue

let pullCompletedRequestToRetire (requests: ServerRequest list) : option<ServerRequest * ServerRequest list> =
    let rec findRequestToRetire list =
        match list with
        | [] -> None
        | request :: rest ->
            // Generally, requests are retired in-sequence
            match request.State with
            | Terminated _ -> Some request
            | _ ->
                // BUT we will ignore ReadOnlyBackground requests in
                // a non-Completed state as those are not supposed to be retired
                // in-sequence
                match request.Mode with
                | Some ReadOnlyBackground -> findRequestToRetire rest
                | _ -> None

    requests
    |> findRequestToRetire
    |> Option.map (fun r -> (r, requests |> List.filter (fun i -> i.Id <> r.Id)))

/// Activation logic is bound to these rules to ensure sequential state transitions
/// but still allow for "Many ReadOnly, One ReadWrite" request concurrency:
///  (1) only 1 ReadWrite request can be active (running) at a time
///  (2) no requests can be activated past the currently running ReadWrite request
///  (3) no requests can be activated while the server is retiring requests up to a certain id
///  (4) no requests can be activated while solutions are not loaded, except ReadWriteSequentialImmediate

type RequestOrWorkspaceFolderActivation =
    | RequestSelectedForActivation of ServerRequest
    | WorkspaceFolderSelectedForActivation of LspWorkspaceFolder

let rec selectPendingRequestToActivate
    (workspace: LspWorkspace)
    (retireUpToId: int option)
    (requests: list<ServerRequest>)
    : option<RequestOrWorkspaceFolderActivation> =
    match requests with
    | [] -> None
    | request :: remainder ->
        match request.State with
        | Enqueued -> None // undetermined request mode at this point,--terminate the search!

        | Pending _ ->
            match retireUpToId with
            | Some id when request.Id > id -> None // request is past the retire-up-to boundary,--don't activate
            | _ ->
                match request.Mode with
                | Some ReadWriteSequentialImmediate ->
                    // ReadWriteSequentialImmediate requests are not blocked on solution status
                    Some(RequestSelectedForActivation request)
                | _ ->
                    match workspace.Initialized with
                    | false ->
                        // workspace is still being initialized, skip this request
                        selectPendingRequestToActivate workspace retireUpToId remainder
                    | true ->
                        let unactivatedWf =
                            workspace.Folders
                            |> List.tryFind (fun wf -> not wf.Solution.IsLoaded && not wf.Solution.IsLoadFailure)

                        logger.LogError(
                            "selectPendingRequestToActivate, wfs.Len={0}, unactivatedWf={1}",
                            workspace.Folders.Length,
                            unactivatedWf
                        )

                        match unactivatedWf with
                        | None ->
                            // ok, all solutions are loaded/failed-to-load and this request is not blocked for activation
                            // and can be activated
                            Some(RequestSelectedForActivation request)

                        | Some wfWithUnloadedSln ->
                            // here we need to initiate solution load for the wf if solution is NotLoaded (activate it)
                            // if not already
                            match wfWithUnloadedSln.Solution with
                            | NotLoaded -> Some(WorkspaceFolderSelectedForActivation wfWithUnloadedSln)
                            | Loading ->
                                // solution(s) are still being loaded, skip this request
                                selectPendingRequestToActivate workspace retireUpToId remainder
                            | _ -> failwithf "unspected .Solution state for %s" (string wfWithUnloadedSln)

        | Running
        | Terminated _ ->
            match request.Mode with
            | Some ReadWriteSequential
            | Some ReadWriteSequentialImmediate ->
                // we have a blocking ReadWrite request that has not been retired yet,--terminate the search!
                None

            | Some ReadOnlyBackground
            | Some ReadOnlySequential
            | None -> selectPendingRequestToActivate workspace retireUpToId remainder

let processRequestQueue
    postServerEvent
    (state: obj)
    (workspace: LspWorkspace)
    (settings: ServerSettings)
    queue
    : Async<RequestQueue> =
    async {
        // first, see if there is any request to retire, and do that first
        match pullCompletedRequestToRetire queue.Requests with
        | Some(retiredReq, updatedRequests) ->
            let newRequestStats =
                match retiredReq.State with
                | Terminated(_, evs) ->
                    for ev in evs do
                        do postServerEvent ev

                    match settings.DebugMode with
                    | true ->
                        queue.Stats
                        |> Map.change retiredReq.Name (updateRequestQueueStats queue retiredReq)
                    | false -> queue.Stats
                | _ ->
                    failwithf
                        "retired request '%s' is in unexpected state '%s'"
                        (string retiredReq)
                        (requestStateName retiredReq.State)

            let updatedQueue =
                { queue with
                    Requests = updatedRequests
                    Stats = newRequestStats }

            do postServerEvent ProcessRequestQueue
            return updatedQueue

        | None ->
            let retireRequestsUpToId = None // state.PendingWorkspaceConfigurationChange |> Option.map _.RetireRequestsUpToId

            let activation =
                selectPendingRequestToActivate workspace retireRequestsUpToId queue.Requests

            match activation with
            | Some(RequestSelectedForActivation pendingReq) ->
                let requestAsRunnable, replyChannel =
                    match pendingReq.State with
                    | Pending rc ->
                        let runningReq =
                            { pendingReq with
                                State = Running
                                RunningSince = Some DateTime.Now }

                        runningReq, rc

                    | _ -> failwithf "ProcessRequestQueue: request %s in unexpected State!" (string pendingReq)

                // replace pending request with a runnable version
                let newRequests =
                    queue.Requests
                    |> List.map (fun r -> if r.Id = pendingReq.Id then requestAsRunnable else r)

                let newQueue = { queue with Requests = newRequests }

                // actually activate the request by sending it the state
                do replyChannel.Reply state

                do postServerEvent ProcessRequestQueue
                return newQueue

            | Some(WorkspaceFolderSelectedForActivation wf) ->
                postServerEvent (WorkspaceFolderActivationRequest wf.Uri)
                return queue

            | None ->
                // no pending requests ready to run or workspace folder to activate;
                // check if all requests up to the retire-up-to boundary have been retired
                let retireRequestsUpToId = None // TODO state.PendingWorkspaceConfigurationChange |> Option.map _.RetireRequestsUpToId

                match retireRequestsUpToId with
                | Some retireUpToId when queue.Requests |> List.exists (fun r -> r.Id <= retireUpToId) |> not ->
                    do postServerEvent RetireRequestsUpToIdCompleted
                | _ -> ()

                return queue
    }
