module CSharpLanguageServer.Runtime.ServerStateLoop

open System
open System.Threading
open System.Threading.Tasks

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Runtime.RequestScheduling

let logger = Logging.getLoggerByName "Runtime.ServerStateLoop"

type ServerLspWorkspace =
    | Empty
    | Uninitialized of LspWorkspace
    | Loading of LspWorkspace
    | Ready of LspWorkspace

let withReadyWorkspaceOrFail (ws: ServerLspWorkspace) (op: LspWorkspace -> LspWorkspace) =
    match ws with
    | Ready ws -> ws |> op |> Ready
    | _ -> failwithf "applyOnReadyWorkspaceOrFail: expected a 'Ready' LspWorkspace but have '%s'" (string ws)

type ServerState =
    { Config: CSharpConfiguration
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      TraceLevel: TraceValues
      Workspace: ServerLspWorkspace
      PendingWorkspaceReconfiguration: LspWorkspace option
      RequestQueue: RequestQueue
      PushDiagnosticsDocumentBacklog: string list
      PushDiagnosticsCurrentDocTask: (string * Task) option
      PeriodicTickTimer: Threading.Timer option
      ShutdownReceived: bool }

    static member Empty =
        { Config = CSharpConfiguration.Default
          LspClient = None
          ClientCapabilities = emptyClientCapabilities
          TraceLevel = TraceValues.Off
          Workspace = Empty
          PendingWorkspaceReconfiguration = None
          WorkspaceReloadPending = None
          RequestQueue = RequestQueue.Empty
          PushDiagnosticsDocumentBacklog = []
          PushDiagnosticsCurrentDocTask = None
          PeriodicTickTimer = None
          ShutdownReceived = false }

let processServerEvent state postServerEvent ev : Async<ServerState> = async {
    match ev with
    | ServerReconfigured newConfig -> return { state with Config = newConfig }

    | TraceLevelChange newTraceLevel ->
        Logging.setLspTraceLevel newTraceLevel

        return
            { state with
                TraceLevel = newTraceLevel }

    | EnterRequestContext(requestRpcOrdinal, requestName, requestMode, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue =
            state.RequestQueue
            |> registerRequest requestRpcOrdinal requestName (requestMode :?> ServerRequestMode) replyChannel

        let newState =
            { state with
                RequestQueue = newRequestQueue }

        return newState

    | LeaveRequestContext(requestRpcOrdinal, bufferedEvents) ->
        let newRequestQueue =
            state.RequestQueue |> finishRequest requestRpcOrdinal bufferedEvents

        let newState =
            { state with
                RequestQueue = newRequestQueue }

        postServerEvent ProcessRequestQueue
        return newState

    | ProcessRequestQueue ->
        // Check if there are any requests to retire and do so.
        let (retiredRequest, requestQueue) =
            state.RequestQueue |> retireNextFinishedRequest state.Config

        match retiredRequest with
        | Some retiredRequest ->
            // Replay the retired request's buffered events, then continue
            // in the next round so they are processed before further work.
            retiredRequest.BufferedEvents |> List.iter postServerEvent
            postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = requestQueue }
        | None ->
            // OK, there were no requests to retire, actually process
            // request queue.
            let makeRequestContext requestMode =
                let readyWorkspace =
                    match state.Workspace with
                    | Ready ws -> ws
                    | _ -> failwithf "Expected 'Ready' LspWorkspace but have '%s' for state.Workspace" (string state.Workspace)

                ServerRequestContext(
                    requestMode,
                    state.LspClient.Value,
                    state.Config,
                    readyWorkspace,
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

    | WorkspaceReconfigured workspaceFolders ->
        let newWorkspace = workspaceFrom workspaceFolders
        return { state with PendingWorkspaceReconfiguration = Some newWorkspace }

        (*
        let currentWs =
            match state.Workspace with
            | Empty -> None
            | Uninitialized ws -> Some ws
            | Loading ws -> Some ws
            | Ready ws -> Some ws

        let newWorkspace =
            match currentWs with
            | Some currentWs ->
                newWorkspace |> workspaceWithSolutionPathOverridesFrom currentWs
            | None ->
                newWorkspace

        return { state with Workspace = newWorkspace }
        *)


    | WorkspaceLoadResult result ->
        failwith "not implemented, WorkspaceLoadResult"
        return state

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
        let doWsFolderChange ws =
            let updatedWorkspaceFolderList =
                ws.Folders
                |> List.map (fun wf -> if wf.Uri = updatedWf.Uri then updatedWf else wf)

            // request queue may have been blocked due to workspace folder(s)
            // not having solution loaded yet
            postServerEvent ProcessRequestQueue

            { ws with
                Folders = updatedWorkspaceFolderList }

        let newWorkspace = withReadyWorkspaceOrFail state.Workspace doWsFolderChange
        return { state with Workspace = newWorkspace }

    | DocumentOpened(uri, ver, timestamp) ->
        let doDocumentOpenedProcessing ws =
            postServerEvent PushDiagnosticsDocumentBacklogUpdate

            let openDocInfo = { Version = ver; Touched = timestamp }
            let newOpenDocs = ws.OpenDocs |> Map.add uri openDocInfo

            { ws with
                OpenDocs = newOpenDocs }

        let newWorkspace = withReadyWorkspaceOrFail state.Workspace doDocumentOpenedProcessing
        return { state with Workspace = newWorkspace }

    | DocumentClosed uri ->
        let doDocumentClosedProcessing ws =
            postServerEvent PushDiagnosticsDocumentBacklogUpdate

            let newOpenDocVersions = ws.OpenDocs |> Map.remove uri

            { ws with
                OpenDocs = newOpenDocVersions }

        let newWorkspace = withReadyWorkspaceOrFail state.Workspace doDocumentClosedProcessing
        return { state with Workspace = newWorkspace }

    | DocumentTouched(uri, timestamp) ->
        let doDocumentTouchedProcessing ws =
            postServerEvent PushDiagnosticsDocumentBacklogUpdate

            let openDocInfo = ws.OpenDocs |> Map.tryFind uri

            match openDocInfo with
            | None -> ws
            | Some openDocInfo ->
                let updatedOpenDocInfo = { openDocInfo with Touched = timestamp }
                let newOpenDocVersions = ws.OpenDocs |> Map.add uri updatedOpenDocInfo

                { ws with
                    OpenDocs = newOpenDocVersions }

        let newWorkspace = withReadyWorkspaceOrFail state.Workspace doDocumentTouchedProcessing
        return { state with Workspace = newWorkspace }

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
        // here we build new backlog for background diagnostics processing
        // which will consider documents by their last modification date
        // for processing first
        let newBacklog =
            state.Workspace.OpenDocs
            |> Seq.sortByDescending (fun kv -> kv.Value.Touched)
            |> Seq.map (fun kv -> kv.Key)
            |> List.ofSeq

        return
            { state with
                PushDiagnosticsDocumentBacklog = newBacklog }

    | PushDiagnosticsProcessPendingDocuments ->
        match state.PushDiagnosticsCurrentDocTask with
        | Some _ ->
            // another document is still being processed, do nothing
            return state
        | None ->
            // try pull next doc from the backlog to process
            let nextDocUri, newBacklog =
                match state.PushDiagnosticsDocumentBacklog with
                | [] -> (None, [])
                | uri :: remainder -> (Some uri, remainder)

            // push diagnostic is enabled only if pull diagnostics is
            // not reported to be supported by the client
            let diagnosticPullSupported =
                state.ClientCapabilities.TextDocument
                |> Option.map _.Diagnostic
                |> Option.map _.IsSome
                |> Option.defaultValue false

            match diagnosticPullSupported, nextDocUri with
            | false, Some docUri ->
                let newState =
                    { state with
                        PushDiagnosticsDocumentBacklog = newBacklog }

                let wf, docForUri = docUri |> workspaceDocument state.Workspace AnyDocument
                let wfPathToUri = workspaceFolderPathToUri wf.Value

                match wf, docForUri with
                | Some wf, None ->
                    let cshtmlPath = workspaceFolderUriToPath wf docUri |> _.Value

                    match! solutionGetRazorDocumentForPath wf.Solution.Value cshtmlPath with
                    | Some(_, compilation, cshtmlTree) ->
                        let semanticModelMaybe = compilation.GetSemanticModel cshtmlTree |> Option.ofObj

                        match semanticModelMaybe with
                        | None ->
                            Error(Exception "could not GetSemanticModelAsync")
                            |> PushDiagnosticsDocumentDiagnosticsResolution
                            |> postServerEvent

                        | Some semanticModel ->
                            let diagnostics =
                                semanticModel.GetDiagnostics()
                                |> Seq.map (Diagnostic.fromRoslynDiagnostic (workspaceFolderPathToUri wf))
                                |> Seq.filter (fun (_, uri) -> uri = docUri)
                                |> Seq.map fst
                                |> Array.ofSeq

                            Ok(docUri, None, diagnostics)
                            |> PushDiagnosticsDocumentDiagnosticsResolution
                            |> postServerEvent

                    | None ->
                        // could not find document for this enqueued uri
                        logger.LogDebug(
                            "PushDiagnosticsProcessPendingDocuments: could not find document w/ uri \"{docUri}\"",
                            string docUri
                        )

                        ()

                    return newState

                | Some wf, Some doc ->
                    let resolveDocumentDiagnostics () : Task = task {
                        let! semanticModelMaybe = doc.GetSemanticModelAsync()

                        match semanticModelMaybe |> Option.ofObj with
                        | None ->
                            Error(Exception("could not GetSemanticModelAsync"))
                            |> PushDiagnosticsDocumentDiagnosticsResolution
                            |> postServerEvent

                        | Some semanticModel ->
                            let diagnostics =
                                semanticModel.GetDiagnostics()
                                |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                                |> Seq.map fst
                                |> Array.ofSeq

                            Ok(docUri, None, diagnostics)
                            |> PushDiagnosticsDocumentDiagnosticsResolution
                            |> postServerEvent
                    }

                    let newTask = Task.Run(resolveDocumentDiagnostics)

                    let newState =
                        { newState with
                            PushDiagnosticsCurrentDocTask = Some(docUri, newTask) }

                    return newState

                | _, _ -> return newState

            | _, _ ->
                // backlog is empty or pull diagnostics is enabled instead,--nothing to do
                return state

    | PushDiagnosticsDocumentDiagnosticsResolution result ->
        // enqueue processing for the next doc on the queue (if any)
        postServerEvent PushDiagnosticsProcessPendingDocuments

        let newState =
            { state with
                PushDiagnosticsCurrentDocTask = None }

        match result with
        | Error exn ->
            logger.LogDebug("PushDiagnosticsDocumentDiagnosticsResolution: {exn}", exn)
            return newState

        | Ok(docUri, version, diagnostics) ->
            match state.LspClient with
            | None -> return newState

            | Some lspClient ->
                let resolvedDocumentDiagnostics =
                    { Uri = docUri
                      Version = version
                      Diagnostics = diagnostics }

                do! lspClient.TextDocumentPublishDiagnostics(resolvedDocumentDiagnostics)
                return newState

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

        match state.PendingWorkspaceReconfiguration with
        | Some pendingWs ->
            match enterDrainingMode state.RequestQueue with
            | Some updatedRequestQueue ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        RequestQueue = updatedRequestQueue }
            | None -> return state

        | None -> return state

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
