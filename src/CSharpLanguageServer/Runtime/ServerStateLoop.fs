module CSharpLanguageServer.Runtime.ServerStateLoop

open System
open System.Globalization
open System.Threading

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Runtime.JsonRpc
open CSharpLanguageServer.Runtime.DebugInfo
open CSharpLanguageServer.Runtime.PushDiagnostics

let logger = Logging.getLoggerByName "Runtime.ServerStateLoop"

[<RequireQualifiedAccess>]
type PendingReconfiguration =
    | FolderReconfiguration of Ionide.LanguageServerProtocol.Types.WorkspaceFolder list
    | ConfigurationChange of CSharpConfiguration
    | WorkspaceReload of deadline: DateTime

type ServerEvent =
    | ServerStarted of ILspClient * (unit -> Async<JsonRpcStats>)
    | ClientInitialize
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | PushDiagnosticsBacklogUpdate
    | EnterRequestContext of
        int64 *
        string *
        RequestMode *
        System.Threading.CancellationTokenSource option *
        AsyncReplyChannel<RequestContext>
    | LoadWorkspaceFolder of DocumentUri * AsyncReplyChannel<LspWorkspaceFolder option>
    | GetWorkspaceFolderNameUriList of AsyncReplyChannel<(string * string) list>
    | GetDebugInfo of AsyncReplyChannel<DebugInfo option>
    | LeaveRequestContext of int64 * LspWorkspaceUpdate
    | PeriodicTimerTick
    | ApplyRequestWorkspaceUpdate of int64 * LspWorkspaceUpdate
    | ProcessRequestQueue
    | FinishRequest of int64
    | RequestQueueDrained
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | ConfigurationChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceSolutionLoadCompleted of WorkspaceFolderSolutionLoadResult
    | WorkspaceFolderUpdates of string * LspWorkspaceFolderUpdateFn list
    | WorkspaceReloadRequested of TimeSpan
    | ProcessSolutionAwaiters

type ServerState =
    { Config: CSharpConfiguration
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      TraceLevel: TraceValues
      Workspace: LspWorkspace
      RequestQueue: RequestQueue
      PushDiagnostics: PushDiagnosticsState
      PeriodicTickTimer: Threading.Timer option
      ShutdownReceived: bool
      LastDebugDumpTime: DateTime
      GetRpcStats: (unit -> Async<JsonRpcStats>) option
      PendingReconfigurations: PendingReconfiguration list }

    static member Empty =
        { Config = CSharpConfiguration.Default
          LspClient = None
          ClientCapabilities = emptyClientCapabilities
          TraceLevel = TraceValues.Off
          Workspace = LspWorkspace.Empty
          RequestQueue = RequestQueue.Empty
          PushDiagnostics = PushDiagnosticsState.Empty
          PeriodicTickTimer = None
          ShutdownReceived = false
          LastDebugDumpTime = DateTime.MinValue
          GetRpcStats = None
          PendingReconfigurations = [] }

let makeRequestContext (state: ServerState) (inbox: MailboxProcessor<ServerEvent>) (requestMode: RequestMode) =
    let loadWorkspaceFolder uri =
        inbox.PostAndAsyncReply(fun rc -> LoadWorkspaceFolder(uri, rc))

    let getWorkspaceFolderList () =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspaceFolderNameUriList rc)

    let lspClient =
        match state.LspClient with
        | Some c -> c
        | None ->
            // LspClient is None after ClientShutdown. Only OutOfBand requests
            // (e.g. $/csharp/debugInfo) may activate at that point; they never
            // use the client, so this is safe. Anything else is a bug.
            new DisconnectedLspClient()

    RequestContext(
        requestMode,
        lspClient,
        state.Config,
        getWorkspaceFolderList,
        loadWorkspaceFolder,
        state.ClientCapabilities,
        state.ShutdownReceived
    )

let processServerEvent state postServerEvent (inbox: MailboxProcessor<ServerEvent>) ev : Async<ServerState> = async {
    match ev with
    | ConfigurationChange newConfig ->
        if newConfig.locale <> state.Config.locale then
            let culture =
                match newConfig.locale with
                | Some l when not (String.IsNullOrWhiteSpace(l)) -> CultureInfo.GetCultureInfo(l)
                | Some _ -> CultureInfo.InvariantCulture
                | None -> CultureInfo.InvariantCulture

            CultureInfo.DefaultThreadCurrentCulture <- culture
            CultureInfo.DefaultThreadCurrentUICulture <- culture

        // When analyzersEnabled toggles, the resultId format changes (it encodes the flag),
        // so the client's cached resultIds are stale.  Ask the client to re-poll immediately
        // rather than waiting for its next scheduled workspace/diagnostic cycle.
        let analyzersEnabledChanged =
            newConfig.analyzersEnabled <> state.Config.analyzersEnabled

        let clientSupportsRefresh =
            state.ClientCapabilities.Workspace
            |> Option.bind _.Diagnostics
            |> Option.bind _.RefreshSupport
            |> Option.defaultValue false

        if analyzersEnabledChanged && clientSupportsRefresh then
            state.LspClient
            |> Option.iter (fun lspClient ->
                Async.Start(
                    async {
                        let! _ = lspClient.WorkspaceDiagnosticRefresh()
                        ()
                    }
                ))

        return { state with Config = newConfig }

    | TraceLevelChange newTraceLevel ->
        Logging.setLspTraceLevel newTraceLevel

        return
            { state with
                TraceLevel = newTraceLevel }

    | EnterRequestContext(requestRpcOrdinal, requestName, requestMode, cts, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue =
            state.RequestQueue
            |> registerRequest requestRpcOrdinal requestName requestMode cts replyChannel

        return
            { state with
                RequestQueue = newRequestQueue }

    | LoadWorkspaceFolder(uri, replyChannel) ->
        match state.Workspace |> workspaceFolder uri with
        | None ->
            replyChannel.Reply None
            return state

        | Some wf ->
            postServerEvent ProcessSolutionAwaiters

            let awaiter = (string uri, replyChannel)
            let newAwaiters = awaiter :: state.Workspace.ReadyAwaiters

            let newWorkspace =
                { state.Workspace with
                    ReadyAwaiters = newAwaiters }

            return { state with Workspace = newWorkspace }

    | GetWorkspaceFolderNameUriList replyChannel ->
        let wfNameUriList = state.Workspace.Folders |> List.map (fun wf -> wf.Name, wf.Uri)
        replyChannel.Reply(wfNameUriList)
        return state

    | GetDebugInfo replyChannel ->
        let! jsonRpcStats =
            match state.GetRpcStats with
            | Some f -> async {
                let! s = f ()
                return Some s
              }
            | None -> async { return None }

        replyChannel.Reply(assembleDebugInfo state.Config state.Workspace state.RequestQueue jsonRpcStats)
        return state

    | LeaveRequestContext(requestRpcOrdinal, wsUpdate) ->
        // The request stays Running until FinishRequest(N) fires after all
        // WorkspaceFolderUpdates for this wsUpdate have been applied.  That keeps
        // subsequent requests blocked by the normal Running-phase concurrency gate
        // until the workspace is up-to-date.  ProcessRequestQueue is posted here too
        // so the scheduler can re-evaluate immediately (e.g. for OutOfBand requests).
        postServerEvent (ApplyRequestWorkspaceUpdate(requestRpcOrdinal, wsUpdate))
        postServerEvent ProcessRequestQueue

        return state

    | FinishRequest(requestRpcOrdinal) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue = state.RequestQueue |> finishRequest requestRpcOrdinal

        return
            { state with
                RequestQueue = newRequestQueue }

    | ApplyRequestWorkspaceUpdate(requestRpcOrdinal, wsUpdate) ->
        if wsUpdate.ClientInitializeEmitted then
            do postServerEvent ClientInitialize

        if wsUpdate.ClientShutdownEmitted then
            do postServerEvent ClientShutdown

        wsUpdate.ClientCapabilityChange
        |> Option.iter (fun caps -> do postServerEvent (ClientCapabilityChange caps))

        wsUpdate.ConfigurationChange
        |> Option.iter (fun cfg -> do postServerEvent (ConfigurationChange cfg))

        wsUpdate.TraceLevelChange
        |> Option.iter (fun level -> do postServerEvent (TraceLevelChange level))

        wsUpdate.ReloadRequested
        |> List.iter (fun delay -> do postServerEvent (WorkspaceReloadRequested delay))

        wsUpdate.FolderUpdates
        |> Map.toSeq
        |> Seq.iter (fun (wfUri, wfUpdates) -> do postServerEvent (WorkspaceFolderUpdates(wfUri, wfUpdates)))

        let mutable pendingReconfigs = state.PendingReconfigurations
        let mutable requestQueue = state.RequestQueue
        let mutable workspace = state.Workspace

        // Accumulate reconfigurations.  While Uninitialized there is no loaded solution
        // to protect, so we never enter drain mode — we just queue the changes.
        // For all other phases the existing drain/reconfiguring machinery applies.
        let accumulate reconfig =
            pendingReconfigs <- pendingReconfigs @ [ reconfig ]

            if workspace.Phase <> LspWorkspacePhase.Uninitialized then
                match enterDrainingMode requestQueue with
                | Some updatedQueue ->
                    requestQueue <- updatedQueue
                    workspace <- workspaceSetPhaseReconfiguring workspace
                    postServerEvent ProcessRequestQueue
                | None -> ()

        wsUpdate.ConfigurationChange
        |> Option.iter (fun cfg ->
            accumulate (PendingReconfiguration.ConfigurationChange cfg)

            // If solutionPathOverride changed while a solution is already loaded,
            // synthesise a FolderReconfiguration so that RequestQueueDrained tears
            // down the old solution and starts a fresh load with the new path.
            // A ConfigurationChange alone only stamps the override onto the folders
            // but never triggers teardown+reload.
            // state.Config is still the old config here (ConfigurationChange is a
            // separate server event posted above and processed later), so the
            // comparison correctly detects a genuine change.
            // Skip this when Uninitialized: the InitializedGate path folds the
            // ConfigurationChange together with the already-queued FolderReconfiguration
            // from handleInitialize in one shot, so a second FolderReconfiguration
            // would cause workspaceFoldersReplaced to run twice.
            if
                cfg.solutionPathOverride <> state.Config.solutionPathOverride
                && workspace.Phase <> LspWorkspacePhase.Uninitialized
            then
                let currentFolders =
                    workspace.Folders |> List.map (fun wf -> { Name = wf.Name; Uri = wf.Uri })

                accumulate (PendingReconfiguration.FolderReconfiguration currentFolders))

        wsUpdate.FolderReconfiguration
        |> Option.iter (fun folders -> accumulate (PendingReconfiguration.FolderReconfiguration folders))

        // handleInitialized has completed: apply all accumulated reconfigurations
        // directly without a drain — nothing is reading an uninitialised workspace.
        // Use the latest config from the pending list rather than state.Config:
        // the ConfigurationChange event posted above is still queued behind this
        // message, so state.Config has not been updated yet.
        if wsUpdate.InitializedGateEmitted then
            let latestConfig =
                pendingReconfigs
                |> List.choose (fun r ->
                    match r with
                    | PendingReconfiguration.ConfigurationChange cfg -> Some cfg
                    | _ -> None)
                |> List.tryLast
                |> Option.defaultValue state.Config

            let applyOne (ws: LspWorkspace) reconfig =
                match reconfig with
                | PendingReconfiguration.FolderReconfiguration folders ->
                    ws
                    |> workspaceFoldersReplaced folders
                    |> workspaceSolutionPathOverride latestConfig
                | PendingReconfiguration.ConfigurationChange _ -> workspaceSolutionPathOverride latestConfig ws
                | PendingReconfiguration.WorkspaceReload _ -> workspaceTeardown ws

            workspace <- pendingReconfigs |> List.fold applyOne workspace
            pendingReconfigs <- []

        // Post FinishRequest(N) after all WorkspaceFolderUpdates events for this
        // request's wsUpdate.  Because the mailbox is FIFO, by the time FinishRequest(N)
        // is processed all WFU events above have already been applied to the workspace,
        // so it is safe to move request N from Running to Finished and let
        // ProcessRequestQueue activate the next request.
        do postServerEvent (FinishRequest requestRpcOrdinal)

        do postServerEvent PushDiagnosticsBacklogUpdate

        return
            { state with
                PendingReconfigurations = pendingReconfigs
                RequestQueue = requestQueue
                Workspace = workspace }

    | ProcessRequestQueue ->
        let result =
            state.RequestQueue
            |> processRequestQueue state.Config (makeRequestContext state inbox)

        match result with
        | Retired(_retiredRequest, newRequestQueue) ->
            // ApplyWorkspaceUpdate was already posted from LeaveRequestContext when
            // this request's handler completed; nothing to replay here.
            do postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = newRequestQueue }

        | Activated(_activatedRequest, newRequestQueue) ->
            postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = newRequestQueue }

        | Drained ->
            postServerEvent RequestQueueDrained

            return state

        | Waiting -> return state

    | ServerStarted(lspClient, getRpcStats) ->
        Logging.setLspTraceClient (Some lspClient)

        return
            { state with
                LspClient = Some lspClient
                GetRpcStats = Some getRpcStats }

    | ClientInitialize ->
        let timer =
            new Threading.Timer(
                Threading.TimerCallback(fun _ -> do postServerEvent PeriodicTimerTick),
                null,
                dueTime = 100,
                period = 250
            )

        return
            { state with
                PeriodicTickTimer = Some timer }

    | ClientShutdown ->
        Logging.setLspTraceClient None

        match state.PeriodicTickTimer with
        | Some timer -> timer.Dispose()
        | None -> ()

        let updatedWorkspace = workspaceTeardown state.Workspace

        return
            { state with
                LspClient = None
                Workspace = updatedWorkspace
                PeriodicTickTimer = None
                ShutdownReceived = true }

    | ClientCapabilityChange cc ->
        let experimentalCapsBoolValue boolPropName =
            cc.Experimental
            |> Option.map _.SelectToken(boolPropName)
            |> Option.bind Option.ofObj
            |> Option.map (fun t ->
                let v = t :?> JValue
                v.Value :?> bool)

        let newConfig =
            { state.Config with
                useMetadataUris =
                    experimentalCapsBoolValue "csharp.metadataUris"
                    |> Option.orElse state.Config.useMetadataUris }

        return
            { state with
                ClientCapabilities = cc
                Config = newConfig }

    | WorkspaceSolutionLoadCompleted changes ->
        let updatedWorkspace =
            state.Workspace
            |> workspaceSolutionLoadCompleted changes
            |> workspaceReadyAwaitersProcessed

        return
            { state with
                Workspace = updatedWorkspace }

    | WorkspaceFolderUpdates(wfUri, wfUpdates) ->
        let wf = state.Workspace |> workspaceFolder wfUri

        let newWorkspace =
            match wf with
            | None -> state.Workspace
            | Some wf ->
                let updatedWf = wfUpdates |> List.fold (|>) wf
                state.Workspace |> workspaceFolderUpdated updatedWf

        return { state with Workspace = newWorkspace }

    | PushDiagnosticsBacklogUpdate ->
        let newWS, pdBacklogUpdatePending =
            state.Workspace |> workspacePDBacklogUpdatePendingReset

        match pdBacklogUpdatePending with
        | false -> return { state with Workspace = newWS }
        | true ->
            let newPD = state.PushDiagnostics |> pushDiagnosticsBacklogUpdate state.Workspace

            return
                { state with
                    Workspace = newWS
                    PushDiagnostics = newPD }

    | WorkspaceReloadRequested quietPeriod ->
        // Sliding-window debounce: each new event resets the deadline to now+delay,
        // so a burst of changes (e.g. `dotnet add package` touching multiple files
        // over several seconds) fully settles before the reload begins.
        let newDeadline = DateTime.UtcNow + quietPeriod

        let newWorkspace =
            { state.Workspace with
                ReloadPending = newDeadline |> Some }

        return { state with Workspace = newWorkspace }

    | PushDiagnosticsProcessPendingDocuments ->
        let postResolution = PushDiagnosticsDocumentDiagnosticsResolution >> postServerEvent

        let! newPD =
            state.PushDiagnostics
            |> processPendingPushDiagnostics state.Workspace state.ClientCapabilities state.Config postResolution

        return { state with PushDiagnostics = newPD }

    | PushDiagnosticsDocumentDiagnosticsResolution result ->
        postServerEvent PushDiagnosticsProcessPendingDocuments

        let! newPD =
            state.PushDiagnostics
            |> handleDocumentDiagnosticsResolution state.LspClient result

        return { state with PushDiagnostics = newPD }

    | RequestQueueDrained ->
        let applyOne (ws: LspWorkspace) reconfig =
            match reconfig with
            | PendingReconfiguration.FolderReconfiguration folders ->
                ws
                |> workspaceTeardown
                |> workspaceFoldersReplaced folders
                |> workspaceSolutionPathOverride state.Config
            | PendingReconfiguration.ConfigurationChange _ ->
                // state.Config was already updated by the ConfigurationChange event
                // posted from ApplyWorkspaceUpdate; re-stamp the path override now.
                workspaceSolutionPathOverride state.Config ws
            | PendingReconfiguration.WorkspaceReload _ -> workspaceTeardown ws

        let finalWorkspace =
            state.PendingReconfigurations |> List.fold applyOne state.Workspace

        postServerEvent ProcessRequestQueue

        return
            { state with
                Workspace = finalWorkspace
                PendingReconfigurations = []
                RequestQueue = state.RequestQueue |> enterDispatchingMode }

    | PeriodicTimerTick ->
        postServerEvent PushDiagnosticsProcessPendingDocuments

        let! jsonRpcStats =
            match state.GetRpcStats with
            | Some f -> async {
                let! s = f ()
                return Some s
              }
            | None -> async { return None }

        let debugInfo =
            assembleDebugInfo state.Config state.Workspace state.RequestQueue jsonRpcStats

        let debugDumpDeadline = state.LastDebugDumpTime + TimeSpan.FromMinutes(1.0)

        let updatedRequestQueue, updatedLastDebugDumpTime =
            match debugInfo with
            | Some info when debugDumpDeadline < DateTime.Now ->
                dumpDebugInfo info

                { state.RequestQueue with
                    Stats = Map.empty },
                DateTime.Now
            | _ -> state.RequestQueue, state.LastDebugDumpTime

        let state =
            { state with
                RequestQueue = updatedRequestQueue
                LastDebugDumpTime = updatedLastDebugDumpTime }

        let solutionReloadDeadline =
            state.Workspace.ReloadPending |> Option.defaultValue (DateTime.UtcNow.AddDays 1)

        match solutionReloadDeadline < DateTime.UtcNow with
        | true ->
            match enterDrainingMode state.RequestQueue with
            | Some updatedRequestQueue ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        Workspace = workspaceSetPhaseReconfiguring state.Workspace
                        PendingReconfigurations =
                            state.PendingReconfigurations
                            @ [ PendingReconfiguration.WorkspaceReload(DateTime.UtcNow) ]
                        RequestQueue = updatedRequestQueue }
            | None -> return state

        | false -> return state

    | ProcessSolutionAwaiters ->
        let updatedWorkspace =
            state.Workspace
            |> workspaceLoadingStarted state.LspClient.Value state.ClientCapabilities state.Config (fun changes ->
                postServerEvent (WorkspaceSolutionLoadCompleted changes))
            |> workspaceReadyAwaitersProcessed

        return
            { state with
                Workspace = updatedWorkspace }
}

let serverEventLoop initialState (inbox: MailboxProcessor<ServerEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent state inbox.Post inbox
            return! loop newState
        with ex ->
            logger.LogError(ex, "serverEventLoop: crashed with {exception}", string ex)
            raise ex
    }

    loop initialState
