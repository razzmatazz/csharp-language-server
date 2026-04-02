module CSharpLanguageServer.Lsp.Workspace

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

type LspWorkspace =
    { Folders: LspWorkspaceFolder list }

    static member Empty = { Folders = [] }

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

let workspaceFolder (workspace: LspWorkspace) (uri: string) =
    let workspaceFolderMatchesUri wf =
        uri.StartsWith wf.Uri || uri.StartsWith(workspaceFolderMetadataUriBase wf)

    workspace.Folders |> Seq.tryFind workspaceFolderMatchesUri

let workspaceTeardown (workspace: LspWorkspace) : unit =
    workspace.Folders |> List.iter workspaceFolderTeardown

let workspaceWithFolder (workspace: LspWorkspace) (updatedWf: LspWorkspaceFolder) =
    let existingW = workspace.Folders |> Seq.tryFind (fun wf -> wf.Uri = updatedWf.Uri)

    let updatedFolders =
        match existingW with
        | Some existingWf ->
            do workspaceFolderTeardown existingWf

            let replaceByUri wf =
                if wf.Uri = existingWf.Uri then updatedWf else wf

            workspace.Folders |> List.map replaceByUri
        | None -> workspace.Folders @ [ updatedWf ]

    { workspace with
        Folders = updatedFolders }

let workspaceWithSolutionsLoaded (lspClient: ILspClient) (clientCapabilities: ClientCapabilities) workspace = async {
    let progressReporter = ProgressReporter(lspClient, clientCapabilities)

    do! progressReporter.Begin("Loading workspace")

    let mutable updatedWorkspace = workspace
    let mutable folderNum = 1

    for wf in workspace.Folders do
        let! updatedWf =
            wf
            |> workspaceFolderWithSolutionLoaded lspClient progressReporter workspace.Folders.Length folderNum

        updatedWorkspace <- updatedWf |> workspaceWithFolder updatedWorkspace
        folderNum <- folderNum + 1

    do! progressReporter.End("Finished loading workspace")

    return updatedWorkspace
}
