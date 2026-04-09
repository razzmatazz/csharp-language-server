module CSharpLanguageServer.Lsp.Workspace

open System

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      ReloadPending: DateTime option }

    static member Empty = { Folders = []; ReloadPending = None }

type LspWorkspaceUpdate =
    { ClientInitializeEmitted: bool
      ClientShutdownEmitted: bool
      ClientCapabilityChange: ClientCapabilities option
      SettingsChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      WorkspaceConfigurationChanged: WorkspaceFolder list option
      WorkspaceReloadRequested: TimeSpan list
      FolderUpdates: Map<string, LspWorkspaceFolderUpdateFn list> }

    static member Empty =
        { ClientInitializeEmitted = false
          ClientShutdownEmitted = false
          ClientCapabilityChange = None
          SettingsChange = None
          TraceLevelChange = None
          WorkspaceConfigurationChanged = None
          WorkspaceReloadRequested = []
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

    member this.WithSettingsChange(config) =
        { this with
            SettingsChange = Some config }

    member this.WithTraceLevelChange(traceLevel) =
        { this with
            TraceLevelChange = Some traceLevel }

    member this.WithWorkspaceConfigurationChanged(folders) =
        { this with
            WorkspaceConfigurationChanged = Some folders }

    member this.WithWorkspaceReloadRequested(delay) =
        { this with
            WorkspaceReloadRequested = this.WorkspaceReloadRequested @ [ delay ] }

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

let workspaceTeardown (workspace: LspWorkspace) : LspWorkspace =
    let tornDownFolders = workspace.Folders |> List.map workspaceFolderTeardown

    { workspace with
        Folders = tornDownFolders
        ReloadPending = None }
