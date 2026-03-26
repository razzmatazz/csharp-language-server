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

type ServerState =
    { Config: CSharpConfiguration
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      TraceLevel: TraceValues
      Workspace: LspWorkspace
      RequestQueue: RequestQueue
      WorkspaceReloadPending: DateTime option
      PushDiagnosticsDocumentBacklog: string list
      PushDiagnosticsCurrentDocTask: (string * Task) option
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
          PushDiagnosticsDocumentBacklog = []
          PushDiagnosticsCurrentDocTask = None
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
            let makeRequestContext requestMode =
                ServerRequestContext(
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

        let openDocInfo = { Version = ver; Touched = timestamp }
        let newOpenDocs = state.Workspace.OpenDocs |> Map.add uri openDocInfo

        return
            { state with
                Workspace =
                    { state.Workspace with
                        OpenDocs = newOpenDocs } }

    | DocumentClosed uri ->
        postServerEvent PushDiagnosticsDocumentBacklogUpdate

        let newOpenDocVersions = state.Workspace.OpenDocs |> Map.remove uri

        return
            { state with
                Workspace =
                    { state.Workspace with
                        OpenDocs = newOpenDocVersions } }

    | DocumentTouched(uri, timestamp) ->
        postServerEvent PushDiagnosticsDocumentBacklogUpdate

        let openDocInfo = state.Workspace.OpenDocs |> Map.tryFind uri

        match openDocInfo with
        | None -> return state
        | Some openDocInfo ->
            let updatedOpenDocInfo = { openDocInfo with Touched = timestamp }
            let newOpenDocVersions = state.Workspace.OpenDocs |> Map.add uri updatedOpenDocInfo

            return
                { state with
                    Workspace =
                        { state.Workspace with
                            OpenDocs = newOpenDocVersions } }

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
