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
open CSharpLanguageServer.Runtime.PushDiagnostics

let logger = Logging.getLoggerByName "Runtime.ServerStateLoop"

type ServerEvent =
    | ServerStarted of ILspClient
    | ClientInitialize
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | PushDiagnosticsBacklogUpdate
    | EnterRequestContext of int64 * string * RequestMode * AsyncReplyChannel<RequestContext>
    | GetWorkspaceFolder of DocumentUri * withSolutionReady: bool * AsyncReplyChannel<LspWorkspaceFolder option>
    | GetWorkspaceFolderUriList of AsyncReplyChannel<string list>
    | LeaveRequestContext of int64 * LspWorkspaceUpdate
    | PeriodicTimerTick
    | ApplyWorkspaceUpdate of LspWorkspaceUpdate
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | SettingsChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceConfigurationChanged of WorkspaceFolder list
    | WorkspaceFolderSolutionChange of uri: string * generation: Guid * LspWorkspaceFolderSolution
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
      SolutionReadyAwaiters: list<string * AsyncReplyChannel<LspWorkspaceFolder option>> }

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
          SolutionReadyAwaiters = [] }

let makeRequestContext (state: ServerState) (inbox: MailboxProcessor<ServerEvent>) (requestMode: RequestMode) =
    let getWorkspaceFolder uri withSolutionReady =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspaceFolder(uri, withSolutionReady, rc))

    let getWorkspaceFolderList () =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspaceFolderUriList rc)

    RequestContext(
        requestMode,
        state.LspClient.Value,
        state.Config,
        getWorkspaceFolder,
        getWorkspaceFolderList,
        state.ClientCapabilities,
        state.ShutdownReceived
    )

let processServerEvent state postServerEvent (inbox: MailboxProcessor<ServerEvent>) ev : Async<ServerState> = async {
    match ev with
    | SettingsChange newConfig ->
        if newConfig.locale <> state.Config.locale then
            let culture =
                match newConfig.locale with
                | Some l when not (String.IsNullOrWhiteSpace(l)) -> CultureInfo.GetCultureInfo(l)
                | Some _ -> CultureInfo.InvariantCulture
                | None -> CultureInfo.InvariantCulture

            CultureInfo.DefaultThreadCurrentCulture <- culture
            CultureInfo.DefaultThreadCurrentUICulture <- culture

        return { state with Config = newConfig }

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

        return
            { state with
                RequestQueue = newRequestQueue }

    | GetWorkspaceFolder(uri, withSolutionReady, replyChannel) ->
        match state.Workspace |> workspaceFolder uri with
        | None ->
            replyChannel.Reply None
            return state

        | Some wf ->
            match withSolutionReady with
            | false ->
                replyChannel.Reply(Some wf)
                return state

            | true ->
                postServerEvent ProcessSolutionAwaiters

                let awaiter = (string uri, replyChannel)
                let newAwaiters = awaiter :: state.SolutionReadyAwaiters

                return
                    { state with
                        SolutionReadyAwaiters = newAwaiters }

    | GetWorkspaceFolderUriList replyChannel ->
        replyChannel.Reply(state.Workspace.Folders |> List.map _.Uri)
        return state

    | LeaveRequestContext(requestRpcOrdinal, wsUpdate) ->
        let newRequestQueue = state.RequestQueue |> finishRequest requestRpcOrdinal wsUpdate

        postServerEvent ProcessRequestQueue

        return
            { state with
                RequestQueue = newRequestQueue }

    | ApplyWorkspaceUpdate wsUpdate ->

        if wsUpdate.ClientInitializeEmitted then
            do postServerEvent ClientInitialize

        if wsUpdate.ClientShutdownEmitted then
            do postServerEvent ClientShutdown

        wsUpdate.ClientCapabilityChange
        |> Option.iter (fun caps -> do postServerEvent (ClientCapabilityChange caps))

        wsUpdate.SettingsChange
        |> Option.iter (fun cfg -> do postServerEvent (SettingsChange cfg))

        wsUpdate.TraceLevelChange
        |> Option.iter (fun level -> do postServerEvent (TraceLevelChange level))

        wsUpdate.WorkspaceConfigurationChanged
        |> Option.iter (fun folders -> do postServerEvent (WorkspaceConfigurationChanged folders))

        wsUpdate.WorkspaceReloadRequested
        |> List.iter (fun delay -> do postServerEvent (WorkspaceReloadRequested delay))

        wsUpdate.FolderUpdates
        |> Map.toSeq
        |> Seq.iter (fun (wfUri, wfUpdates) -> do postServerEvent (WorkspaceFolderUpdates(wfUri, wfUpdates)))

        do postServerEvent PushDiagnosticsBacklogUpdate

        return state

    | ProcessRequestQueue ->
        let result =
            state.RequestQueue
            |> processRequestQueue state.Config (makeRequestContext state inbox)

        match result with
        | Retired(retiredRequest, newRequestQueue) ->
            do postServerEvent (ApplyWorkspaceUpdate retiredRequest.WorkspaceUpdate)
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

    | WorkspaceConfigurationChanged workspaceFolders ->
        for (_, rc) in state.SolutionReadyAwaiters do
            rc.Reply(None)

        let _ = workspaceTeardown state.Workspace

        let newWorkspace =
            workspaceFrom workspaceFolders |> workspaceWithSolutionPathOverride state.Config

        return
            { state with
                Workspace = newWorkspace
                SolutionReadyAwaiters = [] }

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

        let _ = workspaceTeardown state.Workspace

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

    | WorkspaceFolderSolutionChange(uri, generation, newSolution) ->
        let wf = state.Workspace |> workspaceFolder uri

        let newWorkspace =
            match wf with
            | None -> state.Workspace
            | Some wf ->
                if wf.Generation <> generation then
                    // Stale event from a cancelled/superseded load — discard silently.
                    state.Workspace
                else
                    state.Workspace |> workspaceWithFolderUpdated { wf with Solution = newSolution }

        do postServerEvent ProcessSolutionAwaiters
        return { state with Workspace = newWorkspace }

    | WorkspaceFolderUpdates(wfUri, wfUpdates) ->
        let wf = state.Workspace |> workspaceFolder wfUri

        let newWorkspace =
            match wf with
            | None -> state.Workspace
            | Some wf ->
                let updatedWf = wfUpdates |> List.fold (|>) wf
                state.Workspace |> workspaceWithFolderUpdated updatedWf

        return { state with Workspace = newWorkspace }

    | PushDiagnosticsBacklogUpdate ->
        let newWS, pdBacklogUpdatePending =
            state.Workspace |> workspaceWithPDBacklogUpdatePendingReset

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
        for (_, rc) in state.SolutionReadyAwaiters do
            rc.Reply(None)

        let tornDownWorkspace = workspaceTeardown state.Workspace

        postServerEvent ProcessRequestQueue

        return
            { state with
                Workspace = tornDownWorkspace
                SolutionReadyAwaiters = []
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
            state.Workspace.ReloadPending |> Option.defaultValue (DateTime.UtcNow.AddDays 1)

        match solutionReloadDeadline < DateTime.UtcNow with
        | true ->
            match enterDrainingMode state.RequestQueue with
            | Some updatedRequestQueue ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        RequestQueue = updatedRequestQueue }
            | None -> return state

        | false -> return state

    | ProcessSolutionAwaiters ->
        let mutable newState = state

        //
        // initiate solution load for wfs where wf.Solution is Pending
        //
        let wfsWithUninitializedSolution =
            state.SolutionReadyAwaiters
            |> Seq.map fst
            |> Seq.distinct
            |> Seq.map (fun uri -> workspaceFolder uri state.Workspace)
            |> Seq.collect Option.toList
            |> Seq.filter _.Solution.IsUninitialized
            |> List.ofSeq

        for wf in wfsWithUninitializedSolution do
            let onSolutionInitCompletion newSolution =
                postServerEvent (WorkspaceFolderSolutionChange(wf.Uri, wf.Generation, newSolution))

            let updatedWf =
                wf
                |> workspaceFolderWithSolutionInitialized
                    state.LspClient.Value
                    state.ClientCapabilities
                    onSolutionInitCompletion

            let newWorkspace = state.Workspace |> workspaceWithFolderUpdated updatedWf

            newState <-
                { newState with
                    Workspace = newWorkspace }

        //
        // satisfy and release awaiters immediately where uri resolves to wf.Solution that is Ready or Defunct
        //
        let mutable awaitersToKeep = []

        for awaiter in newState.SolutionReadyAwaiters do
            let (awaiterWfUri, awaiterRC) = awaiter

            let wf = newState.Workspace |> workspaceFolder awaiterWfUri

            match wf with
            | None -> awaiterRC.Reply(None)

            | Some wf ->
                match wf.Solution with
                | Ready _ -> awaiterRC.Reply(Some wf)
                | Defunct _ -> awaiterRC.Reply(None)
                | Uninitialized
                | Loading _ -> awaitersToKeep <- awaiter :: awaitersToKeep

        newState <-
            { newState with
                SolutionReadyAwaiters = awaitersToKeep }

        return newState
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
