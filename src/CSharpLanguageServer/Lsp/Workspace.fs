module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
type LspWorkspacePhase =
    | Uninitialized
    | Configured
    | Loading of CancellationTokenSource
    | Ready
    | ShuttingDown

type LspWorkspace =
    {
        Phase: LspWorkspacePhase
        Folders: LspWorkspaceFolder list

        /// Opaque identity token, bumped every time workspace is shut down.
        /// Used to detect and discard stale async completion events (e.g. from a cancelled
        /// solution load) that belong to a previous generation of the workspace.
        Generation: Guid
    }

    static member Empty =
        { Phase = LspWorkspacePhase.Uninitialized
          Folders = []
          Generation = Guid.NewGuid() }

type LspWorkspaceUpdate =
    { PhaseTransition: LspWorkspacePhase option
      ClientCapabilityChange: ClientCapabilities option
      ConfigurationChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      ReloadRequested: TimeSpan list
      FolderReconfiguration: WorkspaceFolder list option
      FolderUpdates: Map<string, LspWorkspaceFolderUpdateFn list> }

    static member Empty =
        { PhaseTransition = None
          ClientCapabilityChange = None
          ConfigurationChange = None
          TraceLevelChange = None
          FolderReconfiguration = None
          ReloadRequested = []
          FolderUpdates = Map.empty }

    member this.WithPhaseTransition(phase) =
        { this with
            PhaseTransition = Some phase }

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

let workspaceFrom (workspaceFolders: WorkspaceFolder list) =
    if workspaceFolders.Length = 0 then
        failwith "workspaceFrom: at least 1 workspace folder must be provided!"

    let folders =
        workspaceFolders
        |> Seq.map (fun f ->
            { LspWorkspaceFolder.Empty with
                Uri = f.Uri
                Name = f.Name })
        |> List.ofSeq

    { LspWorkspace.Empty with
        Folders = folders }

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

type LspWorkspaceLoadCompletionFn = (string * LspWorkspaceFolderSolution) list -> unit

let workspaceLoadAsync
    (lspClient: ILspClient)
    (clientCapabilities: ClientCapabilities)
    (wfs: LspWorkspaceFolder list)
    : Async<(string * LspWorkspaceFolderSolution) array> =

    let loadFolder wf = async {
        let! solution = workspaceFolderLoadAsync lspClient clientCapabilities wf
        return wf.Uri, solution
    }

    wfs |> List.map loadFolder |> Async.Parallel

let workspaceLoadStarted
    (lspClient: ILspClient)
    (clientCapabilities: ClientCapabilities)
    (workspaceLoadCompletionCallback: LspWorkspaceLoadCompletionFn)
    (workspace: LspWorkspace)
    : LspWorkspace =

    match workspace.Phase with
    | LspWorkspacePhase.Configured ->
        let wfsWithUninitializedSolution =
            workspace.Folders |> List.filter _.Solution.IsUninitialized

        let loadingAsync =
            workspaceLoadAsync lspClient clientCapabilities wfsWithUninitializedSolution

        let onLoadingTaskComplete (t: Task<(string * LspWorkspaceFolderSolution) array>) =
            if t.IsFaulted || t.IsCanceled then
                let errorMsg =
                    t.Exception
                    |> Option.ofObj
                    |> Option.map string
                    |> Option.defaultValue "Solution initialization has faulted or has been cancelled"

                let defunctResults =
                    wfsWithUninitializedSolution |> List.map (fun wf -> wf.Uri, Defunct errorMsg)

                workspaceLoadCompletionCallback defunctResults
            else
                workspaceLoadCompletionCallback (t.Result |> Array.toList)

        let cts = new CancellationTokenSource()
        let task = Async.StartAsTask(loadingAsync, cancellationToken = cts.Token)
        task.ContinueWith(onLoadingTaskComplete) |> ignore

        let markFolderAsLoading wf = { wf with Solution = Loading }
        let updatedFolders = workspace.Folders |> List.map markFolderAsLoading

        { workspace with
            Phase = LspWorkspacePhase.Loading cts
            Folders = updatedFolders }

    | _ -> failwithf "Cannot initiate load for a workspace that is in phase '%s'" (string (workspace.Phase.GetType()))

let workspaceLoadCompleted
    (folderSolutionChanges: (string * LspWorkspaceFolderSolution) list)
    (workspace: LspWorkspace)
    : LspWorkspace =

    match workspace.Phase with
    | LspWorkspacePhase.Loading _ ->
        let applyFolderSolutionChange wf =
            match folderSolutionChanges |> List.tryFind (fun (uri, _) -> uri = wf.Uri) with
            | Some(_, newSolution) -> { wf with Solution = newSolution }
            | None -> wf

        let updatedFolders = workspace.Folders |> List.map applyFolderSolutionChange

        { workspace with
            Phase = LspWorkspacePhase.Ready
            Folders = updatedFolders }

    | _ -> failwithf "Cannot complete load for a workspace that is in phase '%s'" (string (workspace.Phase.GetType()))

let workspaceShutdown (workspace: LspWorkspace) : LspWorkspace =
    let tornDownFolders = workspace.Folders |> List.map workspaceFolderTeardown

    { workspace with
        Phase = LspWorkspacePhase.Uninitialized
        Folders = tornDownFolders
        Generation = Guid.NewGuid() }
