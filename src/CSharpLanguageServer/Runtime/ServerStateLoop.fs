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
    | EnterRequestContext of
        int64 *
        string *
        RequestMode *
        System.Threading.CancellationTokenSource option *
        AsyncReplyChannel<RequestContext>
    | GetWorkspace of AsyncReplyChannel<LspWorkspace>
    | GetWorkspaceFolder of DocumentUri * withSolutionReady: bool * AsyncReplyChannel<LspWorkspaceFolder option>
    | GetWorkspaceFolderUriList of AsyncReplyChannel<string list>
    | GetDebugInfo of AsyncReplyChannel<CSharpConfiguration * LspWorkspace>
    | LeaveRequestContext of int64 * LspWorkspaceUpdate
    | PeriodicTimerTick
    | ApplyWorkspaceUpdate of LspWorkspaceUpdate
    | DrainIfPendingOperationsReady
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | ConfigurationChange of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceLoadCompleted of generation: Guid * (string * LspWorkspaceFolderSolution) list
    | WorkspaceFolderUpdates of string * LspWorkspaceFolderUpdateFn list
    | ProcessSolutionAwaiters

/// A workspace-disrupting operation to be applied after the request queue drains.
type WorkspacePendingOperation =
    | PendingReload of reloadNoLaterThan: DateTime
    | PendingFolderReplacement of newFolders: WorkspaceFolder list
    | PendingSolutionPathChange of newConfig: CSharpConfiguration

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
      SolutionReadyAwaiters: list<string * AsyncReplyChannel<LspWorkspaceFolder option>>
      PendingOperations: WorkspacePendingOperation list }

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
          SolutionReadyAwaiters = []
          PendingOperations = [] }

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

    | EnterRequestContext(requestRpcOrdinal, requestName, requestMode, cts, replyChannel) ->
        postServerEvent ProcessRequestQueue

        let newRequestQueue =
            state.RequestQueue
            |> registerRequest requestRpcOrdinal requestName requestMode cts replyChannel

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
                let newAwaiters = awaiter :: state.SolutionReadyAwaiters

                return
                    { state with
                        SolutionReadyAwaiters = newAwaiters }

    | GetWorkspaceFolderUriList replyChannel ->
        replyChannel.Reply(state.Workspace.Folders |> List.map _.Uri)
        return state

    | GetDebugInfo replyChannel ->
        replyChannel.Reply(state.Config, state.Workspace)
        return state

    | LeaveRequestContext(requestRpcOrdinal, wsUpdate) ->
        let newRequestQueue = state.RequestQueue |> finishRequest requestRpcOrdinal wsUpdate

        postServerEvent ProcessRequestQueue

        return
            { state with
                RequestQueue = newRequestQueue }

    | ApplyWorkspaceUpdate wsUpdate ->
        if wsUpdate.InitializeRequested then
            postServerEvent ClientInitialize

        if wsUpdate.ShutdownRequested then
            postServerEvent ClientShutdown

        wsUpdate.ClientCapabilityChange
        |> Option.iter (fun caps -> do postServerEvent (ClientCapabilityChange caps))

        wsUpdate.ConfigurationChange
        |> Option.iter (fun cfg -> do postServerEvent (ConfigurationChange cfg))

        wsUpdate.TraceLevelChange
        |> Option.iter (fun level -> do postServerEvent (TraceLevelChange level))

        // Detect solutionPathOverride changes and route them based on current phase:
        //   Ready | Loading | Reconfiguring → queue PendingSolutionPathChange; the
        //     existing drain machinery picks it up (DrainIfPendingOperationsReady
        //     for Ready/Loading, or the in-progress RequestQueueDrained for
        //     Reconfiguring).
        //   Configured | Uninitialized → apply workspaceSolutionPathOverride
        //     immediately so the next workspaceLoadStarted uses the correct path.
        //     No drain needed; nothing is running yet.
        //   ShuttingDown → ignore (halting).
        let solutionPathChangedConfig =
            wsUpdate.ConfigurationChange
            |> Option.filter (fun cfg -> cfg.solutionPathOverride <> state.Config.solutionPathOverride)

        let newPendingOps =
            let fromFolderReconfig =
                wsUpdate.FolderReconfiguration
                |> Option.map PendingFolderReplacement
                |> Option.toList

            let fromReloads =
                wsUpdate.ReloadRequested
                |> List.map (fun delay -> PendingReload(DateTime.UtcNow + delay))

            let fromSolutionPathChange =
                match solutionPathChangedConfig with
                | None -> []
                | Some newConfig ->
                    match state.Workspace.Phase with
                    | LspWorkspacePhase.Ready
                    | LspWorkspacePhase.Loading _
                    | LspWorkspacePhase.Reconfiguring -> [ PendingSolutionPathChange newConfig ]
                    | _ -> []

            state.PendingOperations
            @ fromFolderReconfig
            @ fromReloads
            @ fromSolutionPathChange

        let newWorkspace =
            match solutionPathChangedConfig with
            | Some newConfig ->
                match state.Workspace.Phase with
                | LspWorkspacePhase.Configured
                | LspWorkspacePhase.Uninitialized -> workspaceSolutionPathOverride newConfig state.Workspace
                | _ -> state.Workspace
            | None -> state.Workspace

        wsUpdate.FolderUpdates
        |> Map.toSeq
        |> Seq.iter (fun (wfUri, wfUpdates) -> do postServerEvent (WorkspaceFolderUpdates(wfUri, wfUpdates)))

        do postServerEvent PushDiagnosticsBacklogUpdate
        do postServerEvent DrainIfPendingOperationsReady

        return
            { state with
                Workspace = newWorkspace
                PendingOperations = newPendingOps }

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
                Workspace = workspaceConfigured state.Workspace
                PeriodicTickTimer = Some timer }

    | ClientShutdown ->
        Logging.setLspTraceClient None

        match state.PeriodicTickTimer with
        | Some timer -> timer.Dispose()
        | None -> ()

        // Transition to ShuttingDown (cancels in-flight load CTS if Loading).
        let updatedWorkspace = workspaceShuttingDown state.Workspace

        // Enter drain mode. If already draining (e.g. a Reconfiguring drain was
        // in progress), keep the existing queue — just update the phase so
        // RequestQueueDrained knows to halt rather than rebuild.
        let updatedRequestQueue =
            enterDrainingMode state.RequestQueue
            |> Option.defaultWith (fun () -> state.RequestQueue)

        postServerEvent ProcessRequestQueue

        // LspClient is intentionally kept alive here. makeRequestContext uses
        // state.LspClient.Value; clearing it now would crash the state loop if
        // any pending requests get activated during the drain. LspClient is
        // cleared in the RequestQueueDrained ShuttingDown branch once the drain
        // is complete and no further activations can occur.
        return
            { state with
                Workspace = updatedWorkspace
                PeriodicTickTimer = None
                ShutdownReceived = true
                RequestQueue = updatedRequestQueue }

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

    | WorkspaceLoadCompleted(generation, folderSolutionChanges) ->
        if state.Workspace.Generation <> generation then
            // Stale event from a cancelled/superseded load — discard silently.
            return state
        else
            let newWorkspace = state.Workspace |> workspaceLoadCompleted folderSolutionChanges

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
        match state.Workspace.Phase with
        | LspWorkspacePhase.Reconfiguring ->
            // A pending-operation drain (config change / folder replacement / reload)
            // has completed. Tear down, apply pending ops, and restart the load.
            for _, rc in state.SolutionReadyAwaiters do
                rc.Reply(None)

            let tornDownWorkspace = workspaceShutdown state.Workspace

            let applyPendingOperation (ws: LspWorkspace) (op: WorkspacePendingOperation) : LspWorkspace =
                match op with
                | PendingReload _ -> ws // workspace shape unchanged; folders already Uninitialized from teardown
                | PendingFolderReplacement newFolders ->
                    workspaceFrom newFolders |> workspaceSolutionPathOverride state.Config
                | PendingSolutionPathChange _ -> workspaceSolutionPathOverride state.Config ws

            let rebuiltWorkspace =
                state.PendingOperations |> List.fold applyPendingOperation tornDownWorkspace

            // Transition Uninitialized → Configured so workspaceLoadStarted can fire.
            let configuredWorkspace = workspaceConfigured rebuiltWorkspace

            postServerEvent ProcessRequestQueue
            postServerEvent ProcessSolutionAwaiters

            return
                { state with
                    Workspace = configuredWorkspace
                    SolutionReadyAwaiters = []
                    PendingOperations = []
                    RequestQueue = state.RequestQueue |> enterDispatchingMode }

        | LspWorkspacePhase.ShuttingDown ->
            // Client-initiated shutdown drain has completed. Tear down and halt;
            // no rebuild. LspClient is cleared here (not in ClientShutdown) so
            // that makeRequestContext does not crash for requests activated during
            // the drain.
            for _, rc in state.SolutionReadyAwaiters do
                rc.Reply(None)

            return
                { state with
                    Workspace = workspaceShutdown state.Workspace
                    LspClient = None
                    SolutionReadyAwaiters = []
                    PendingOperations = []
                    RequestQueue = state.RequestQueue |> enterDispatchingMode }

        | other ->
            // Should not happen; log and reset the queue gracefully.
            logger.LogWarning("RequestQueueDrained fired in unexpected phase {phase}; resetting queue", string other)

            return
                { state with
                    RequestQueue = state.RequestQueue |> enterDispatchingMode }

    | PeriodicTimerTick ->
        postServerEvent PushDiagnosticsProcessPendingDocuments
        postServerEvent DrainIfPendingOperationsReady

        let debugMode =
            state.Config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        let updatedRequestQueue =
            dumpAndResetRequestStats debugMode state.Workspace.Phase state.RequestQueue

        return
            { state with
                RequestQueue = updatedRequestQueue }

    | DrainIfPendingOperationsReady ->
        let shouldDrain =
            state.PendingOperations
            |> List.exists (fun op ->
                match op with
                | PendingFolderReplacement _
                | PendingSolutionPathChange _ -> true
                | PendingReload deadline -> deadline < DateTime.UtcNow)

        // Reconfiguring is only valid from Ready or Loading. In other phases
        // (e.g. Configured, Uninitialized) the solution has not started loading
        // yet, so there is nothing to drain and the pending ops will be picked
        // up once the workspace reaches Ready.
        let canReconfigure =
            match state.Workspace.Phase with
            | LspWorkspacePhase.Ready
            | LspWorkspacePhase.Loading _ -> true
            | _ -> false

        match shouldDrain && canReconfigure with
        | false -> return state
        | true ->
            match enterDrainingMode state.RequestQueue with
            | None -> return state
            | Some updatedRequestQueue ->
                postServerEvent ProcessRequestQueue

                return
                    { state with
                        RequestQueue = updatedRequestQueue
                        Workspace = workspaceReconfiguring state.Workspace }

    | ProcessSolutionAwaiters ->
        match state.SolutionReadyAwaiters with
        | [] -> return state
        | solutionReadyAwaiters ->
            match state.Workspace.Phase with
            | LspWorkspacePhase.Configured ->
                let workspaceLoadCompletionCallback folderSolutionChanges =
                    postServerEvent (WorkspaceLoadCompleted(state.Workspace.Generation, folderSolutionChanges))

                let newWorkspace =
                    state.Workspace
                    |> workspaceLoadStarted
                        state.LspClient.Value
                        state.ClientCapabilities
                        workspaceLoadCompletionCallback

                return { state with Workspace = newWorkspace }

            | LspWorkspacePhase.Ready ->
                // satisfy and release awaiters immediately where uri resolves to wf.Solution that is Loaded or Defunct
                for awaiter in state.SolutionReadyAwaiters do
                    let awaiterWfUri, awaiterRC = awaiter

                    let wf = state.Workspace |> workspaceFolder awaiterWfUri

                    match wf with
                    | None -> awaiterRC.Reply(None)
                    | Some wf ->
                        match wf.Solution with
                        | Loaded _ -> awaiterRC.Reply(Some wf)
                        | Defunct _ -> awaiterRC.Reply(None)
                        | _ -> failwithf "Unexpected wf.Solution state '%s'" (string (wf.Solution.GetType()))

                return
                    { state with
                        SolutionReadyAwaiters = [] }

            | _ -> return state
}

/// Checks key server-state invariants after each event is processed.
/// Active only when `csharp.debug.debugMode` is true in the server configuration.
/// Violations are logged as errors (log-and-continue, not failwith) so a bug
/// surfaces in the log without crashing the server in production.
let assertServerStateInvariants (state: ServerState) =
    let debugMode =
        state.Config.debug |> Option.bind _.debugMode |> Option.defaultValue false

    if debugMode then
        let phase = state.Workspace.Phase
        let queueMode = state.RequestQueue.Mode

        // Invariant A — Queue mode ↔ phase.
        // DrainingUpTo must be set iff phase is Reconfiguring or ShuttingDown.
        // Both transitions happen atomically in the same ServerEvent handler so
        // this must hold at every observable state boundary.
        let phaseExpectsDrain =
            match phase with
            | LspWorkspacePhase.Reconfiguring
            | LspWorkspacePhase.ShuttingDown -> true
            | _ -> false

        let queueIsDraining =
            match queueMode with
            | DrainingUpTo _ -> true
            | Dispatching -> false

        if phaseExpectsDrain && not queueIsDraining then
            logger.LogError(
                "Invariant A violated: phase is {phase} but queue mode is {mode} (expected DrainingUpTo)",
                string phase,
                string queueMode
            )

        if queueIsDraining && not phaseExpectsDrain then
            logger.LogError(
                "Invariant A violated: queue is {mode} but phase is {phase} (expected Reconfiguring or ShuttingDown)",
                string queueMode,
                string phase
            )

        // Invariant B — PeriodicTickTimer ↔ phase.
        // The timer is created in ClientInitialize (Uninitialized → Configured)
        // and disposed in ClientShutdown (any → ShuttingDown).  It should be
        // Some for every post-initialize, pre-shutdown phase.
        let phaseExpectsTimer =
            match phase with
            | LspWorkspacePhase.Configured
            | LspWorkspacePhase.Loading _
            | LspWorkspacePhase.Ready
            | LspWorkspacePhase.Reconfiguring -> true
            | _ -> false

        match state.PeriodicTickTimer, phaseExpectsTimer with
        | None, true ->
            logger.LogError(
                "Invariant B violated: PeriodicTickTimer is None but phase is {phase} (expected Some)",
                string phase
            )
        | Some _, false ->
            logger.LogError(
                "Invariant B violated: PeriodicTickTimer is Some but phase is {phase} (expected None)",
                string phase
            )
        | _ -> ()

let serverEventLoop initialState (inbox: MailboxProcessor<ServerEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent state inbox.Post inbox
            assertServerStateInvariants newState
            return! loop newState
        with ex ->
            logger.LogError(ex, "serverEventLoop: crashed with {exception}", string ex)
            raise ex
    }

    loop initialState
