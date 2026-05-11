module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

/// LspWorkspacePhase state machine:
///
/// Normal lifecycle:
///
/// <code>
///   Uninitialized ──► Configured ──► Loading ──► Ready ──► ShuttingDown
/// </code>
///
/// When dynamic server configuration arrives while already configured/loading/ready,
/// the phase detours through Reconfiguring and back to Configured:
///
/// <code>
///   Configured ──┐
///   Loading    ──┼──► Reconfiguring ──► Configured
///   Ready      ──┘
/// </code>
///
/// <c>Configured</c> is entered after <c>LifeCycle.handleInitialized</c> is processed,
/// ensuring the initial server configuration and workspace folder list are both known
/// before any solution(s) can be loaded.
///
/// <c>Reconfiguring</c> exists to drain the request queue before the workspace folder
/// solutions are torn down and reconfiguration is applied to them; in-flight requests
/// must finish against the current solutions before they are replaced.
[<RequireQualifiedAccess>]
type LspWorkspacePhase =
    | Uninitialized
    | Configured
    | Loading
    | Ready
    | Reconfiguring
    | ShuttingDown

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      Phase: LspWorkspacePhase
      LoadingCts: System.Threading.CancellationTokenSource option
      ReloadPending: DateTime option
      ReadyAwaiters: list<string * AsyncReplyChannel<LspWorkspaceFolder option>> }

    static member Empty =
        { Folders = []
          Phase = LspWorkspacePhase.Uninitialized
          LoadingCts = None
          ReloadPending = None
          ReadyAwaiters = [] }

type LspWorkspaceUpdate =
    { ClientInitializeEmitted: bool
      ClientShutdownEmitted: bool
      ClientCapabilityChange: ClientCapabilities option
      ConfigurationChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      ReloadRequested: TimeSpan list
      FolderReconfiguration: WorkspaceFolder list option
      FolderUpdates: Map<string, LspWorkspaceFolderUpdateFn list> }

    static member Empty =
        { ClientInitializeEmitted = false
          ClientShutdownEmitted = false
          ClientCapabilityChange = None
          ConfigurationChange = None
          TraceLevelChange = None
          FolderReconfiguration = None
          ReloadRequested = []
          FolderUpdates = Map.empty }

    member this.WithClientInitialize() =
        { this with
            ClientInitializeEmitted = true }

    member this.WithClientShutdown() =
        { this with
            ClientShutdownEmitted = true }

    member this.WithClientCapabilityChange(capabilities) =
        { this with
            ClientCapabilityChange = Some capabilities }

    member this.WithConfigurationChange(configuration) =
        { this with
            ConfigurationChange = Some configuration }

    member this.WithTraceLevelChange(traceLevel) =
        { this with
            TraceLevelChange = Some traceLevel }

    member this.WithReloadRequested(delay) =
        { this with
            ReloadRequested = this.ReloadRequested @ [ delay ] }

    member this.WithFolderReconfiguration(folders) =
        { this with
            FolderReconfiguration = Some folders }

    member this.WithFolderUpdates(wfUri: string, updates: LspWorkspaceFolderUpdateFn list) =
        let wfUpdateList = this.FolderUpdates |> Map.tryFind wfUri |> Option.defaultValue []

        let newFolderUpdates = this.FolderUpdates |> Map.add wfUri (wfUpdateList @ updates)

        { this with
            FolderUpdates = newFolderUpdates }

let workspaceSolutionPathOverride (config: CSharpConfiguration) (workspace: LspWorkspace) =
    let folders =
        match config.solutionPathOverride, workspace.Folders with
        | Some solutionPath, firstFolder :: rest ->
            let updatedFirstFolder =
                { firstFolder with
                    SolutionPathOverride = Some solutionPath }

            updatedFirstFolder :: rest
        | _ -> workspace.Folders

    { workspace with Folders = folders }

let workspaceFoldersReplaced (workspaceFolders: WorkspaceFolder list) (workspace: LspWorkspace) =
    if workspaceFolders.Length = 0 then
        failwith "workspaceFoldersReplaced: at least 1 workspace folder must be provided!"

    let folders =
        workspaceFolders
        |> Seq.map (fun f ->
            { LspWorkspaceFolder.Empty with
                Uri = f.Uri
                Name = f.Name })
        |> List.ofSeq

    { workspace with
        Folders = folders
        Phase = LspWorkspacePhase.Configured }

let workspaceFolder (uri: string) (workspace: LspWorkspace) =
    let workspaceFolderMatchesUri wf =
        uri.StartsWith wf.Uri || uri.StartsWith(workspaceFolderMetadataUriBase wf)

    workspace.Folders |> Seq.tryFind workspaceFolderMatchesUri

let workspaceFolderUpdated (updatedWf: LspWorkspaceFolder) (workspace: LspWorkspace) =
    let updatedFolders =
        workspace.Folders
        |> List.map (fun wf -> if wf.Uri = updatedWf.Uri then updatedWf else wf)

    { workspace with
        Folders = updatedFolders }

let workspacePDBacklogUpdatePendingReset (ws: LspWorkspace) : LspWorkspace * bool =
    let havePDBacklogUpdatePending =
        ws.Folders |> Seq.tryFind _.PushDiagnosticsBacklogUpdatePending |> Option.isSome

    let newWS =
        match havePDBacklogUpdatePending with
        | false -> ws
        | true ->
            let newWSFolders =
                ws.Folders
                |> List.map (fun wf ->
                    { wf with
                        PushDiagnosticsBacklogUpdatePending = false })

            { ws with Folders = newWSFolders }

    newWS, havePDBacklogUpdatePending

type WorkspaceFolderSolutionChange = (string * Guid * LspWorkspaceFolderSolution) list

/// For every workspace folder whose solution is still `Uninitialized`, kick off an async
/// solution load and return the workspace with those folders updated to `Loading`.
/// All folder loads run concurrently via Async.Parallel. `onSolutionChange` is called once,
/// after the last folder settles, with all `(uri, generation, newSolution)` tuples together.
let workspaceLoadingStarted
    (lspClient: ILspClient)
    (clientCapabilities: ClientCapabilities)
    (config: CSharpConfiguration)
    (onSolutionChange: WorkspaceFolderSolutionChange -> unit)
    (workspace: LspWorkspace)
    : LspWorkspace =
    let uninitializedFolders =
        workspace.Folders |> List.filter _.Solution.IsUninitialized

    match uninitializedFolders with
    | [] -> workspace
    | uninitializedFolders ->
        let folderLoads =
            uninitializedFolders
            |> List.map (fun wf ->
                let loadingAsync = workspaceFolderSolutionLoad lspClient clientCapabilities config wf

                { wf with Solution = Loading },
                async {
                    let! result = loadingAsync
                    return wf.Uri, wf.Generation, result
                })

        let updatedWorkspace =
            folderLoads
            |> List.fold (fun ws (updatedWf, _) -> ws |> workspaceFolderUpdated updatedWf) workspace

        let cts = new System.Threading.CancellationTokenSource()

        let aggregatingAsync = async {
            let! results = folderLoads |> List.map snd |> Async.Parallel
            onSolutionChange (results |> Array.toList)
        }

        Async.Start(aggregatingAsync, cancellationToken = cts.Token)

        { updatedWorkspace with
            Phase = LspWorkspacePhase.Loading
            LoadingCts = Some cts }

/// Satisfy and release awaiters immediately where uri resolves to wf.Solution that is Loaded or Defunct
let workspaceReadyAwaitersProcessed (workspace: LspWorkspace) : LspWorkspace =
    let mutable awaitersToKeep = []

    for awaiter in workspace.ReadyAwaiters do
        let awaiterWfUri, awaiterRC = awaiter

        let wf = workspace |> workspaceFolder awaiterWfUri

        match wf with
        | None -> awaiterRC.Reply(None)

        | Some wf ->
            match wf.Solution with
            | Loaded _ -> awaiterRC.Reply(Some wf)
            | Defunct _ -> awaiterRC.Reply(None)
            | Uninitialized
            | Loading -> awaitersToKeep <- awaiter :: awaitersToKeep

    { workspace with
        ReadyAwaiters = awaitersToKeep }

/// Apply a batch of folder-solution results to the workspace: patch each folder's Solution
/// (discarding stale generations), flush ReadyAwaiters, and advance Phase to Ready.
let workspaceFolderSolutionChangesApplied
    (changes: WorkspaceFolderSolutionChange)
    (workspace: LspWorkspace)
    : LspWorkspace =

    let applyChange ws (uri, generation, newSolution) =
        match ws |> workspaceFolder uri with
        | None -> ws
        | Some wf ->
            if wf.Generation <> generation then
                // Stale event from a cancelled/superseded load — discard silently.
                ws
            else
                ws |> workspaceFolderUpdated { wf with Solution = newSolution }

    changes |> List.fold applyChange workspace

let workspaceTeardown (workspace: LspWorkspace) : LspWorkspace =
    let tornDownFolders = workspace.Folders |> List.map workspaceFolderTeardown

    for (_, rc) in workspace.ReadyAwaiters do
        rc.Reply(None)

    match workspace.LoadingCts with
    | Some cts ->
        cts.Cancel()
        cts.Dispose()
    | None -> ()

    { workspace with
        Folders = tornDownFolders
        LoadingCts = None
        ReloadPending = None }
