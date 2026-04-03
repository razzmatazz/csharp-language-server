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
    | GetWorkspaceFolder of DocumentUri * withSolutionReady: bool * AsyncReplyChannel<LspWorkspaceFolder option>
    | GetWorkspaceFolderUriList of AsyncReplyChannel<string list>
    | LeaveRequestContext of int64 * ServerEvent list
    | PeriodicTimerTick
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | SettingsChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceConfigurationChanged of WorkspaceFolder list
    | WorkspaceFolderSolutionChange of string * LspWorkspaceFolderSolution
    | WorkspaceFolderChange of LspWorkspaceFolder
    | WorkspaceReloadRequested of TimeSpan
    | ProcessSolutionAwaiters

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
      ShutdownReceived: bool
      SolutionReadyAwaiters: list<string * AsyncReplyChannel<LspWorkspaceFolder option>> }

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

    | LeaveRequestContext(requestRpcOrdinal, bufferedServerEvents) ->
        postServerEvent ProcessRequestQueue

        let bufferedServerEvents = bufferedServerEvents |> List.map (fun e -> e :> obj)

        let newRequestQueue =
            state.RequestQueue |> finishRequest requestRpcOrdinal bufferedServerEvents

        return
            { state with
                RequestQueue = newRequestQueue }

    | ProcessRequestQueue ->
        let! result = processRequestQueue state.Config (makeRequestContext state inbox) state.RequestQueue

        match result with
        | Retired(retiredRequest, updatedRequestQueue) ->
            // Replay the retired request's buffered events, then continue
            // in the next round so they are processed before further work.
            retiredRequest.BufferedServerEvents
            |> List.iter (fun ev -> postServerEvent (ev :?> ServerEvent))

            postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = updatedRequestQueue }

        | Activated(_activatedRequest, updatedRequestQueue) ->
            postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = updatedRequestQueue }

        | Drained ->
            postServerEvent RequestQueueDrained

            return
                { state with
                    RequestQueue = state.RequestQueue }

        | Waiting ->
            return
                { state with
                    RequestQueue = state.RequestQueue }

    | WorkspaceConfigurationChanged workspaceFolders ->
        // TODO: we may want to kill existing SolutionReadyAwaiters!

        let _ = workspaceTeardown state.Workspace

        let newWorkspace =
            workspaceFrom workspaceFolders |> workspaceWithSolutionPathOverride state.Config

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

    | WorkspaceFolderSolutionChange(uri, newSolution) ->
        let wf = state.Workspace |> workspaceFolder uri

        let newWorkspace =
            match wf with
            | None -> state.Workspace
            | Some wf ->
                let updatedWf = { wf with Solution = newSolution }
                state.Workspace |> workspaceWithFolderUpdated updatedWf

        do postServerEvent ProcessSolutionAwaiters

        return { state with Workspace = newWorkspace }

    | WorkspaceFolderChange updatedWf ->
        let newWorkspace = state.Workspace |> workspaceWithFolderUpdated updatedWf

        return { state with Workspace = newWorkspace }

    | DocumentOpened(uri, ver, timestamp) ->
        let newWorkspace =
            match workspaceFolder uri state.Workspace with
            | None -> state.Workspace
            | Some wf ->
                let updatedWf = workspaceFolderWithDocOpened uri ver timestamp wf
                state.Workspace |> workspaceWithFolderUpdated updatedWf

        let newPD = state.PushDiagnostics |> pushDiagnosticsBacklogUpdate newWorkspace

        return
            { state with
                Workspace = newWorkspace
                PushDiagnostics = newPD }

    | DocumentClosed uri ->
        let newWorkspace =
            match workspaceFolder uri state.Workspace with
            | None -> state.Workspace
            | Some wf ->
                let updatedWf = workspaceFolderWithDocClosed uri wf
                workspaceWithFolderUpdated updatedWf state.Workspace

        let newPD = state.PushDiagnostics |> pushDiagnosticsBacklogUpdate newWorkspace

        return
            { state with
                Workspace = newWorkspace
                PushDiagnostics = newPD }

    | DocumentTouched(uri, timestamp) ->
        let newWorkspace =
            workspaceFolder uri state.Workspace
            |> Option.bind (workspaceFolderWithDocTouched uri timestamp)
            |> Option.map (fun wf -> workspaceWithFolderUpdated wf state.Workspace)
            |> Option.defaultValue state.Workspace

        let newPD = state.PushDiagnostics |> pushDiagnosticsBacklogUpdate newWorkspace

        return
            { state with
                Workspace = newWorkspace
                PushDiagnostics = newPD }

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

    | PushDiagnosticsProcessPendingDocuments ->
        let postResolution = PushDiagnosticsDocumentDiagnosticsResolution >> postServerEvent

        let! newPD =
            state.PushDiagnostics
            |> processPendingPushDiagnostics state.Workspace state.ClientCapabilities postResolution

        return { state with PushDiagnostics = newPD }

    | PushDiagnosticsDocumentDiagnosticsResolution result ->
        postServerEvent PushDiagnosticsProcessPendingDocuments

        let! newPD =
            state.PushDiagnostics
            |> handleDocumentDiagnosticsResolution state.LspClient result

        return { state with PushDiagnostics = newPD }

    | RequestQueueDrained ->
        let tornDownWorkspace = workspaceTeardown state.Workspace

        postServerEvent ProcessRequestQueue

        return
            { state with
                Workspace = tornDownWorkspace
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
            |> Seq.collect (fun wf -> if wf.IsSome then [ wf.Value ] else [])
            |> Seq.filter _.Solution.IsUninitialized
            |> List.ofSeq

        for wf in wfsWithUninitializedSolution do
            let onSolutionInitCompletion newSolution =
                postServerEvent (WorkspaceFolderSolutionChange(wf.Uri, newSolution))

            let updatedWf =
                wf
                |> workspaceFolderWithSolutionInitialized state.LspClient.Value onSolutionInitCompletion

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
