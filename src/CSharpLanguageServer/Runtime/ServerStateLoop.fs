module CSharpLanguageServer.Runtime.ServerStateLoop

open System
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
open CSharpLanguageServer.Runtime.PushDiagnostics

let logger = Logging.getLoggerByName "Runtime.ServerStateLoop"

type ServerEvent =
    | ServerStarted of ILspClient
    | ClientInitialize
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | DocumentClosed of string
    | DocumentOpened of string * int * DateTime
    | DocumentTouched of string * DateTime
    | EnterRequestContext of int64 * string * RequestMode * AsyncReplyChannel<RequestContext>
    | LeaveRequestContext of int64 * ServerEvent list
    | PeriodicTimerTick
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentBacklogUpdate
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | SettingsChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceConfigurationChanged of WorkspaceFolder list
    | WorkspaceFolderChange of LspWorkspaceFolder
    | WorkspaceReloadRequested of TimeSpan

type ServerState =
    { Config: CSharpConfiguration
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      TraceLevel: TraceValues
      Workspace: LspWorkspace
      RequestQueue: RequestQueue
      WorkspaceReloadPending: DateTime option
      PushDiagnostics: PushDiagnosticsState
      PeriodicTickTimer: Threading.Timer option
      ShutdownReceived: bool }

    static member Empty =
        { Config = CSharpConfiguration.Default
          LspClient = None
          ClientCapabilities = emptyClientCapabilities
          TraceLevel = TraceValues.Off
          Workspace = LspWorkspace.Empty
          RequestQueue = RequestQueue.Empty
          WorkspaceReloadPending = None
          PushDiagnostics = PushDiagnosticsState.Empty
          PeriodicTickTimer = None
          ShutdownReceived = false }

let processServerEvent state postServerEvent ev : Async<ServerState> = async {
    match ev with
    | SettingsChange newConfig -> return { state with Config = newConfig }

    | TraceLevelChange newTraceLevel ->
        Logging.setLspTraceLevel newTraceLevel

        return
            { state with
                TraceLevel = newTraceLevel }

    | EnterRequestContext(requestRpcOrdinal, requestName, requestMode, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue =
            state.RequestQueue
            |> registerRequest requestRpcOrdinal requestName requestMode replyChannel

        let newState =
            { state with
                RequestQueue = newRequestQueue }

        return newState

    | LeaveRequestContext(requestRpcOrdinal, bufferedServerEvents) ->
        let bufferedServerEvents = bufferedServerEvents |> List.map (fun e -> e :> obj)

        let newRequestQueue =
            state.RequestQueue |> finishRequest requestRpcOrdinal bufferedServerEvents

        let newState =
            { state with
                RequestQueue = newRequestQueue }

        postServerEvent ProcessRequestQueue
        return newState

    | ProcessRequestQueue ->
        let (retiredRequest, requestQueue) =
            state.RequestQueue |> retireNextFinishedRequest state.Config

        match retiredRequest with
        | Some retiredRequest ->
            // Replay the retired request's buffered events, then continue
            // in the next round so they are processed before further work.
            retiredRequest.BufferedServerEvents
            |> List.iter (fun ev -> postServerEvent (ev :?> ServerEvent))

            postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = requestQueue }
        | None ->
            let makeRequestContext (requestMode: RequestMode) =
                RequestContext(
                    requestMode,
                    state.LspClient.Value,
                    state.Config,
                    state.Workspace,
                    state.ClientCapabilities,
                    state.ShutdownReceived
                )

            let! result = processRequestQueue makeRequestContext requestQueue

            match result with
            | Activated(_activatedRequest, updatedRequestQueue) ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        RequestQueue = updatedRequestQueue }
            | Drained ->
                postServerEvent RequestQueueDrained

                return
                    { state with
                        RequestQueue = requestQueue }
            | Waiting ->
                return
                    { state with
                        RequestQueue = requestQueue }

    | WorkspaceConfigurationChanged workspaceFolders ->
        let newWorkspace =
            workspaceFrom workspaceFolders
            |> workspaceWithSolutionPathOverridesFrom state.Workspace

        return { state with Workspace = newWorkspace }

    | ServerStarted lspClient ->
        Logging.setLspTraceClient (Some lspClient)

        return
            { state with
                LspClient = Some lspClient }

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

        return
            { state with
                LspClient = None
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

    | WorkspaceFolderChange updatedWf ->
        let updatedWorkspaceFolderList =
            state.Workspace.Folders
            |> List.map (fun wf -> if wf.Uri = updatedWf.Uri then updatedWf else wf)

        // request queue may have been blocked due to workspace folder(s)
        // not having solution loaded yet
        postServerEvent ProcessRequestQueue

        let newWorkspace =
            { state.Workspace with
                Folders = updatedWorkspaceFolderList }

        let newState = { state with Workspace = newWorkspace }

        return newState

    | DocumentOpened(uri, ver, timestamp) ->
        postServerEvent PushDiagnosticsDocumentBacklogUpdate

        return
            { state with
                Workspace = state.Workspace |> workspaceWithDocOpened uri ver timestamp }

    | DocumentClosed uri ->
        postServerEvent PushDiagnosticsDocumentBacklogUpdate

        return
            { state with
                Workspace = state.Workspace |> workspaceWithDocClosed uri }

    | DocumentTouched(uri, timestamp) ->
        postServerEvent PushDiagnosticsDocumentBacklogUpdate

        match state.Workspace |> workspaceWithDocTouched uri timestamp with
        | None -> return state
        | Some newWorkspace -> return { state with Workspace = newWorkspace }

    | WorkspaceReloadRequested reloadNoLaterThanIn ->
        // we need to wait a bit before starting this so we
        // can buffer many incoming requests at once
        let newSolutionReloadDeadline =
            let suggestedDeadline = DateTime.Now + reloadNoLaterThanIn

            match state.WorkspaceReloadPending with
            | Some currentDeadline ->
                if suggestedDeadline < currentDeadline then
                    suggestedDeadline
                else
                    currentDeadline
            | None -> suggestedDeadline

        return
            { state with
                WorkspaceReloadPending = newSolutionReloadDeadline |> Some }

    | PushDiagnosticsDocumentBacklogUpdate ->
        let newPD =
            PushDiagnostics.handleBacklogUpdate state.Workspace state.PushDiagnostics

        return { state with PushDiagnostics = newPD }

    | PushDiagnosticsProcessPendingDocuments ->
        let postResolution = PushDiagnosticsDocumentDiagnosticsResolution >> postServerEvent

        let! newPD =
            PushDiagnostics.handleProcessPending
                state.Workspace
                state.ClientCapabilities
                postResolution
                state.PushDiagnostics

        return { state with PushDiagnostics = newPD }

    | PushDiagnosticsDocumentDiagnosticsResolution result ->
        let postProcessPending () =
            postServerEvent PushDiagnosticsProcessPendingDocuments

        let! newPD = PushDiagnostics.handleResolution state.LspClient postProcessPending result state.PushDiagnostics
        return { state with PushDiagnostics = newPD }

    | RequestQueueDrained ->
        let solutionLoadDelay = state.Config.debug |> Option.bind _.solutionLoadDelay

        match solutionLoadDelay with
        | Some ms when ms > 0 ->
            logger.LogInformation("SolutionLoadDelay is set to {ms}ms, waiting before loading solution..", ms)
            do! Async.Sleep ms
        | _ -> ()

        let! updatedWorkspace =
            workspaceWithSolutionsLoaded state.LspClient.Value state.ClientCapabilities state.Workspace

        postServerEvent ProcessRequestQueue

        return
            { state with
                Workspace = updatedWorkspace
                WorkspaceReloadPending = None
                RequestQueue = state.RequestQueue |> enterDispatchingMode }

    | PeriodicTimerTick ->
        postServerEvent PushDiagnosticsProcessPendingDocuments

        let debugMode =
            state.Config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        let updatedRequestQueue = dumpAndResetRequestStats debugMode state.RequestQueue

        let state =
            { state with
                RequestQueue = updatedRequestQueue }

        let solutionReloadDeadline =
            state.WorkspaceReloadPending |> Option.defaultValue (DateTime.Now.AddDays 1)

        match solutionReloadDeadline < DateTime.Now with
        | true ->
            match enterDrainingMode state.RequestQueue with
            | Some updatedRequestQueue ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        RequestQueue = updatedRequestQueue }
            | None -> return state

        | false -> return state

}

let serverEventLoop initialState (inbox: MailboxProcessor<ServerEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent state inbox.Post
            return! loop newState
        with ex ->
            logger.LogError(ex, "serverEventLoop: crashed with {exception}", string ex)
            raise ex
    }

    loop initialState
