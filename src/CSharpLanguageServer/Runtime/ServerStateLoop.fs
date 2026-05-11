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

type ServerEvent =
    | ServerStarted of ILspClient * (unit -> Async<JsonRpcStats>)
    | ClientInitialize
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | PushDiagnosticsBacklogUpdate
    | EnterRequestContext of int64 * string * RequestMode * AsyncReplyChannel<RequestContext>
    | GetWorkspace of AsyncReplyChannel<LspWorkspace>
    | GetWorkspaceFolder of DocumentUri * withSolutionReady: bool * AsyncReplyChannel<LspWorkspaceFolder option>
    | GetWorkspaceFolderUriList of AsyncReplyChannel<string list>
    | GetDebugInfo of AsyncReplyChannel<DebugInfo option>
    | LeaveRequestContext of int64 * LspWorkspaceUpdate
    | PeriodicTimerTick
    | ApplyWorkspaceUpdate of LspWorkspaceUpdate
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | ConfigurationChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceFolderConfigurationChanged of WorkspaceFolder list
    | WorkspaceFolderSolutionChanged of WorkspaceFolderSolutionChange
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
      GetRpcStats: (unit -> Async<JsonRpcStats>) option }

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
          GetRpcStats = None }

let makeRequestContext (state: ServerState) (inbox: MailboxProcessor<ServerEvent>) (requestMode: RequestMode) =
    let getWorkspaceFolder uri withSolutionReady =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspaceFolder(uri, withSolutionReady, rc))

    let getWorkspaceFolderList () =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspaceFolderUriList rc)

    let getWorkspaceSnapshot () =
        inbox.PostAndAsyncReply(fun rc -> GetWorkspace rc)

    RequestContext(
        requestMode,
        state.LspClient.Value,
        state.Config,
        getWorkspaceSnapshot,
        getWorkspaceFolder,
        getWorkspaceFolderList,
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

    | EnterRequestContext(requestRpcOrdinal, requestName, requestMode, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue =
            state.RequestQueue
            |> registerRequest requestRpcOrdinal requestName requestMode replyChannel

        return
            { state with
                RequestQueue = newRequestQueue }

    | GetWorkspace replyChannel ->
        replyChannel.Reply(state.Workspace)
        return state

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
                let newAwaiters = awaiter :: state.Workspace.ReadyAwaiters

                let newWorkspace =
                    { state.Workspace with
                        ReadyAwaiters = newAwaiters }

                return { state with Workspace = newWorkspace }

    | GetWorkspaceFolderUriList replyChannel ->
        replyChannel.Reply(state.Workspace.Folders |> List.map _.Uri)
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

        wsUpdate.ConfigurationChange
        |> Option.iter (fun cfg -> do postServerEvent (ConfigurationChange cfg))

        wsUpdate.TraceLevelChange
        |> Option.iter (fun level -> do postServerEvent (TraceLevelChange level))

        wsUpdate.FolderReconfiguration
        |> Option.iter (fun folders -> do postServerEvent (WorkspaceFolderConfigurationChanged folders))

        wsUpdate.ReloadRequested
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

    | WorkspaceFolderConfigurationChanged workspaceFolders ->
        let updatedWorkspace =
            state.Workspace
            |> workspaceTeardown
            |> workspaceFoldersReplaced workspaceFolders
            |> workspaceSolutionPathOverride state.Config

        return
            { state with
                Workspace = updatedWorkspace }

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

    | WorkspaceFolderSolutionChanged changes ->
        let newWorkspace =
            changes
            |> List.fold
                (fun ws (uri, generation, newSolution) ->
                    match ws |> workspaceFolder uri with
                    | None -> ws
                    | Some wf ->
                        if wf.Generation <> generation then
                            // Stale event from a cancelled/superseded load — discard silently.
                            ws
                        else
                            ws |> workspaceFolderUpdated { wf with Solution = newSolution })
                state.Workspace

        do postServerEvent ProcessSolutionAwaiters
        return { state with Workspace = newWorkspace }

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
        let tornDownWorkspace = workspaceTeardown state.Workspace

        postServerEvent ProcessRequestQueue

        return
            { state with
                Workspace = tornDownWorkspace
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
                        RequestQueue = updatedRequestQueue }
            | None -> return state

        | false -> return state

    | ProcessSolutionAwaiters ->
        let onWorkspaceSolutionLoad changes =
            postServerEvent (WorkspaceFolderSolutionChanged changes)

        let updatedWorkspace =
            state.Workspace
            |> workspaceLoadingStarted state.LspClient.Value state.ClientCapabilities onWorkspaceSolutionLoad
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
