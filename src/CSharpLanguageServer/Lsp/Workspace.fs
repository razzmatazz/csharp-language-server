module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
type LspWorkspacePhase =
    | Uninitialized
    | Initializing
    | Ready
    | ShuttingDown

type LspWorkspace =
    { Phase: LspWorkspacePhase
      Folders: LspWorkspaceFolder list }

    static member Empty =
        { Phase = LspWorkspacePhase.Uninitialized
          Folders = [] }

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

let workspaceWithSolutionPathOverride (config: CSharpConfiguration) (workspace: LspWorkspace) =
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

let workspaceWithFolderUpdated (updatedWf: LspWorkspaceFolder) (workspace: LspWorkspace) =
    let updatedFolders =
        workspace.Folders
        |> List.map (fun wf -> if wf.Uri = updatedWf.Uri then updatedWf else wf)

    { workspace with
        Folders = updatedFolders }

let workspaceWithPDBacklogUpdatePendingReset (ws: LspWorkspace) : LspWorkspace * bool =
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

let workspaceShutdown (workspace: LspWorkspace) : LspWorkspace =
    let tornDownFolders = workspace.Folders |> List.map workspaceFolderTeardown

    { workspace with
        Phase = LspWorkspacePhase.Uninitialized
        Folders = tornDownFolders }
