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
    { Settings: ServerSettings
      LspClient: ILspClient option
      ClientCapabilities: ClientCapabilities
      Workspace: LspWorkspace
      RequestQueue: RequestQueue
      WorkspaceReloadPending: DateTime option
      PushDiagnosticsDocumentBacklog: string list
      PushDiagnosticsCurrentDocTask: (string * Task) option
      PeriodicTickTimer: Threading.Timer option }

    static member Empty =
        { Settings = ServerSettings.Default
          LspClient = None
          ClientCapabilities = emptyClientCapabilities
          Workspace = LspWorkspace.Empty
          RequestQueue = RequestQueue.Empty
          WorkspaceReloadPending = None
          PushDiagnosticsDocumentBacklog = []
          PushDiagnosticsCurrentDocTask = None
          PeriodicTickTimer = None }

let processServerEvent state postServerEvent ev : Async<ServerState> = async {
    match ev with
    | SettingsChange newSettings ->
        let newState: ServerState = { state with Settings = newSettings }

        let solutionChanged =
            not (state.Settings.SolutionPath = newState.Settings.SolutionPath)

        if solutionChanged then
            postServerEvent (WorkspaceReloadRequested(TimeSpan.FromMilliseconds(int64 250)))

        return newState

    | RegisterRequest(requestName, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequest, updatedRequestQueue =
            registerRequestToRequestQueue requestName state.RequestQueue

        replyChannel.Reply(newRequest.Id)

        let newState =
            { state with
                RequestQueue = updatedRequestQueue }

        return newState

    | RequestActivation(requestId, requestMode, _targetUri, replyChannel) ->
        match addActivationListenerForRequestOnRequestQueue requestId replyChannel state.RequestQueue with
        | Some(_request, updatedRequestQueue) ->
            let updatedState =
                { state with
                    RequestQueue = updatedRequestQueue }

            return updatedState

        | None ->
            // Request is already running or finished, send it the current state immediately
            replyChannel.Reply(state)
            return state

    | RetireRequest(requestId, emittedEvents) ->
        match retireRequestFromRequestQueue state.Settings requestId state.RequestQueue with
        | Some newRequestQueue ->
            let newState =
                { state with
                    RequestQueue = newRequestQueue }

            for ev in emittedEvents do
                do postServerEvent ev

            do postServerEvent ProcessRequestQueue
            return newState

        | None ->
            logger.LogWarning("serverEventLoop/RetireRequest#{requestId}: no request to retire", requestId)

            return state
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
        let! updatedRequestQueue =
            processRequestQueue
                state.WorkspaceReloadPending.IsSome
                state
                state.Workspace
                state.Settings
                state.RequestQueue

        match updatedRequestQueue with
        | Some updatedRequestQueue ->
            // continue processing msgs from the queue
            postServerEvent ProcessRequestQueue

            let newState =
                { state with
                    RequestQueue = updatedRequestQueue }

            return newState

        | None -> return state

    | WorkspaceConfigurationChanged workspaceFolders ->
        let newWorkspace = workspaceFrom workspaceFolders
        return { state with Workspace = newWorkspace }

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

    // this has tick period of 100ms
    | PeriodicTimerTick ->
        do postServerEvent PushDiagnosticsProcessPendingDocuments
        do postServerEvent CleanupDeadlockedRequestsOnRequestQueue

        let updatedRequestQueue =
            dumpAndResetRequestStats state.Settings.DebugMode state.RequestQueue

        let state =
            { state with
                RequestQueue = updatedRequestQueue }

        let solutionReloadDeadline =
            state.WorkspaceReloadPending |> Option.defaultValue (DateTime.Now.AddDays 1)

        match solutionReloadDeadline < DateTime.Now with
        | true ->
            let! updatedWorkspace =
                workspaceWithSolutionsLoaded
                    state.Settings
                    state.LspClient.Value
                    state.ClientCapabilities
                    state.Workspace

            return
                { state with
                    Workspace = updatedWorkspace
                    WorkspaceReloadPending = None }

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
