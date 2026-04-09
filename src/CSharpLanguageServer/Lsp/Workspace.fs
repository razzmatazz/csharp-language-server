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
      DocumentClosed: string list
      DocumentOpened: (string * int * DateTime) list
      DocumentTouched: (string * DateTime) list
      SettingsChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      WorkspaceConfigurationChanged: WorkspaceFolder list option
      WorkspaceReloadRequested: TimeSpan list
      FolderChange: LspWorkspaceFolder list
      FolderUpdates: Map<string, LspWorkspaceFolderUpdateFn list> }

    static member Empty =
        { ClientInitializeEmitted = false
          ClientShutdownEmitted = false
          ClientCapabilityChange = None
          DocumentClosed = []
          DocumentOpened = []
          DocumentTouched = []
          SettingsChange = None
          TraceLevelChange = None
          WorkspaceConfigurationChanged = None
          FolderChange = []
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

    member this.WithDocumentClosed(uri) =
        { this with
            DocumentClosed = this.DocumentClosed @ [ uri ] }

    member this.WithDocumentOpened(uri, version, timestamp) =
        { this with
            DocumentOpened = this.DocumentOpened @ [ (uri, version, timestamp) ] }

    member this.WithDocumentTouched(uri, timestamp) =
        { this with
            DocumentTouched = this.DocumentTouched @ [ (uri, timestamp) ] }

    member this.WithSettingsChange(config) =
        { this with
            SettingsChange = Some config }

    member this.WithTraceLevelChange(traceLevel) =
        { this with
            TraceLevelChange = Some traceLevel }

    member this.WithWorkspaceConfigurationChanged(folders) =
        { this with
            WorkspaceConfigurationChanged = Some folders }

    member this.WithWorkspaceFolderChange(folder) =
        { this with
            FolderChange = this.FolderChange @ [ folder ] }

    member this.WithWorkspaceReloadRequested(delay) =
        { this with
            WorkspaceReloadRequested = this.WorkspaceReloadRequested @ [ delay ] }

    member this.WithFolderUpdate(wfUri: string, updateFn: LspWorkspaceFolderUpdateFn) =
        let wfUpdateList = this.FolderUpdates |> Map.tryFind wfUri |> Option.defaultValue []

        let newFolderUpdates =
            this.FolderUpdates |> Map.add wfUri (wfUpdateList @ [ updateFn ])

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

let workspaceTeardown (workspace: LspWorkspace) : LspWorkspace =
    let tornDownFolders = workspace.Folders |> List.map workspaceFolderTeardown

    { workspace with
        Folders = tornDownFolders
        ReloadPending = None }
