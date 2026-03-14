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

type WorkspaceConfigurationChangeDetails =
    { Deadline: DateTime
      NewWorkspaceFolders: option<LspWorkspaceFolder list> }

type ServerState =
    { Settings: ServerSettings
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      Workspace: LspWorkspace
      RequestQueue: RequestQueue
      PendingWorkspaceConfigurationChange: option<WorkspaceConfigurationChangeDetails>
      PushDiagnosticsDocumentBacklog: string list
      PushDiagnosticsCurrentDocTask: (string * Task) option
      PeriodicTickTimer: Threading.Timer option }

    static member Empty =
        { Settings = ServerSettings.Default
          LspClient = None
          ClientCapabilities = emptyClientCapabilities
          Workspace = LspWorkspace.Uninitialized
          RequestQueue = RequestQueue.Empty
          PendingWorkspaceConfigurationChange = None
          PushDiagnosticsDocumentBacklog = []
          PushDiagnosticsCurrentDocTask = None
          PeriodicTickTimer = None }

let processServerEvent (logger: ILogger) state postServerEvent ev : Async<ServerState> = async {
    match ev with
    | SettingsChange newSettings ->
        let newState: ServerState = { state with Settings = newSettings }

        let solutionChanged =
            not (state.Settings.SolutionPath = newState.Settings.SolutionPath)

        if solutionChanged then
            // request workspace solution reload
            postServerEvent (WorkspaceConfigurationChange(TimeSpan.FromMilliseconds(int64 100), None))

        return newState

    | RegisterRequest(requestName, replyChannel) ->
        let newRequest, updatedRequestQueue =
            state.RequestQueue |> registerRequestToRequestQueue requestName

        replyChannel.Reply(newRequest.Id)

        let newState =
            { state with
                RequestQueue = updatedRequestQueue }

        postServerEvent ProcessRequestQueue

        return newState

    | RequestActivation(requestId, mode, targetUri, replyChannel) ->
        let updatedQueue, request =
            state.RequestQueue
            |> activateRequestOnRequestQueue requestId mode targetUri replyChannel

        match request with
        | Some _ -> ()
        | None ->
            // this request was not found on queue for activation -- it is either already running or finished,
            // --send the current state immediately
            replyChannel.Reply(state)

        do postServerEvent ProcessRequestQueue

        return
            { state with
                RequestQueue = updatedQueue }

    | TerminateRequest(requestId, outcome, emittedEvents) ->
        let updatedQueue, request =
            state.RequestQueue
            |> terminateRequestOnRequestQueue requestId outcome emittedEvents

        match request with
        | Some request -> do postServerEvent ProcessRequestQueue
        | None -> ()

        return
            { state with
                RequestQueue = updatedQueue }

    | CleanupDeadlockedRequestsOnRequestQueue ->
        let updatedQueue = state.RequestQueue |> cleanupDeadlockedRequestsOnRequestQueue

        match updatedQueue with
        | Some updatedQueue ->
            do postServerEvent ProcessRequestQueue

            return
                { state with
                    RequestQueue = updatedQueue }
        | None -> return state

    | ProcessRequestQueue ->
        (* TODO
        let! updatedRequestQueue =
            processRequestQueue postServerEvent state state.Workspace state.Settings state.RequestQueue

        return { state with RequestQueue = updatedRequestQueue }
        *)
        return state

    | RetireRequestsUpToIdCompleted ->
        (*
        let newState =
            { state with
                RetireRequestsUpToId = None }

        postServerEvent ProcessRequestQueue

        match newState.PendingWorkspaceConfigurationChange with
        | Some(_, newWorkspaceFolders) ->
            logger.LogDebug(
                "RetireRequestsUpToIdCompleted: resetting workspace folders as per 'PendingWorkspaceConfigurationChange'"
            )

            let updatedFolders =
                match newWorkspaceFolders with
                | Some folders -> folders
                | None ->
                    newState.Workspace.Folders
                    |> List.map (fun f -> { f with Solution = NotLoaded })

            let updatedWorkspace =
                { state.Workspace with
                    Initialized = true
                    Folders = updatedFolders }

            return
                { newState with
                    Workspace = updatedWorkspace
                    PendingWorkspaceConfigurationChange = None }

        | None -> return newState
*)
        // TODO
        return state

    | ClientInitialize lspClient ->
        let timer =
            new Threading.Timer(
                Threading.TimerCallback(fun _ -> do postServerEvent PeriodicTimerTick),
                null,
                dueTime = 100,
                period = 250
            )

        return
            { state with
                LspClient = Some lspClient
                PeriodicTickTimer = Some timer }

    | ClientShutdown ->
        match state.PeriodicTickTimer with
        | Some timer -> timer.Dispose()
        | None -> ()

        return
            { state with
                LspClient = None
                PeriodicTickTimer = None }

    | ClientCapabilityChange cc ->
        let experimentalCapsBoolValue boolPropName =
            cc.Experimental
            |> Option.map _.SelectToken(boolPropName)
            |> Option.bind Option.ofObj
            |> Option.map (fun t ->
                let v = t :?> JValue
                v.Value :?> bool)

        let oldSettings = state.Settings

        let newSettings =
            { oldSettings with
                UseMetadataUris =
                    experimentalCapsBoolValue "csharp.metadataUris"
                    |> Option.defaultValue oldSettings.UseMetadataUris }

        return
            { state with
                ClientCapabilities = cc
                Settings = newSettings }

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

    | WorkspaceConfigurationChange(reloadNoLaterThanIn, newWorkspaceFolders) ->
        (* TODO
        let effectiveWorkspaceConfigChange =
            let newDeadline = DateTime.Now + reloadNoLaterThanIn

            match state.PendingWorkspaceConfigurationChange with
            | None -> newDeadline, newWorkspaceFolders
            | Some(prevReloadDeadline, prevNewWorkspaceFolders) ->
                let updatedDeadline = min newDeadline prevReloadDeadline

                let updatedWorkspaceFolders =
                    newWorkspaceFolders |> Option.orElse prevNewWorkspaceFolders

                updatedDeadline, updatedWorkspaceFolders

        return
            { state with
                PendingWorkspaceConfigurationChange = Some effectiveWorkspaceConfigChange }
        *)
        return state // TODO

    | WorkspaceFolderActivationRequest wfUri ->
        Console.Error.WriteLine("WorkspaceFolderActivationRequest: {0}", wfUri)

        match workspaceFolder state.Workspace wfUri with
        | None -> return state
        | Some wf ->
            let progressReporter =
                ProgressReporter(state.LspClient.Value, state.ClientCapabilities)

            let updatedWf, asyncWfSolutionChange =
                workspaceFolderWithSolutionLoadInitiated
                    wf
                    progressReporter
                    state.LspClient.Value
                    state.Settings.SolutionPath

            async {
                match! asyncWfSolutionChange with
                | None -> ()
                | Some wfSolution -> postServerEvent (WorkspaceFolderActivationComplete(wfUri, wfSolution))
            }
            |> Async.Start

            let updatedWorkspace = updatedWf |> workspaceWithFolder state.Workspace

            return
                { state with
                    Workspace = updatedWorkspace }

    | WorkspaceFolderActivationComplete(wfUri, wfSolution) ->
        Console.Error.WriteLine("WorkspaceFolderActivationComplete: {0}, {1}", wfUri, wfSolution)

        match workspaceFolder state.Workspace wfUri with
        | Some wf ->
            let updatedWf = { wf with Solution = wfSolution }
            postServerEvent (WorkspaceFolderChange updatedWf)
            return state
        | None -> return state

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
                    let solution = workspaceFolderLoadedSolutionOrExn wf
                    let cshtmlPath = workspaceFolderUriToPath wf docUri |> _.Value

                    match! solutionGetRazorDocumentForPath solution cshtmlPath with
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

    // this has tick period of 100ms
    | PeriodicTimerTick ->
        do postServerEvent PushDiagnosticsProcessPendingDocuments
        do postServerEvent CleanupDeadlockedRequestsOnRequestQueue

        let updatedRequestQueue =
            dumpAndResetRequestStats state.Settings.DebugMode state.RequestQueue

        let state =
            { state with
                RequestQueue = updatedRequestQueue }

        (* TODO
        match state.PendingWorkspaceConfigurationChange with
        | Some(configurationChangeDeadline, _) ->
            if configurationChangeDeadline < DateTime.Now && state.RetireRequestsUpToId.IsNone then
                logger.LogDebug(
                    "PeriodicTimerTick: flushing current requests before workspace reload as per 'PendingWorkspaceConfigurationChange'"
                )

                let newState =
                    { state with
                        RetireRequestsUpToId = Some state.LastRequestId }

                do postServerEvent ProcessRequestQueue
                return newState
            else
                return state

        | None -> return state
*)

        return state
}

let serverEventLoop initialState (inbox: MailboxProcessor<ServerEvent>) =
    let logger = Logging.getLoggerByName "serverEventLoop"

    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent logger state inbox.Post
            return! loop newState
        with ex ->
            logger.LogError(ex, "serverEventLoop: crashed with {exception}", string ex)
            raise ex
    }

    loop initialState
