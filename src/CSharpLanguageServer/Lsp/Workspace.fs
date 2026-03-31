module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util

let logger = Logging.getLoggerByName "Lsp.Workspace"

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

let workspaceWithFolder (workspace: LspWorkspace) (updatedWf: LspWorkspaceFolder) =
    let existingW = workspace.Folders |> Seq.tryFind (fun wf -> wf.Uri = updatedWf.Uri)

    let updatedFolders =
        match existingW with
        | Some existingWf ->
            let replaceByUri wf =
                if wf.Uri = existingWf.Uri then updatedWf else wf

            workspace.Folders |> List.map replaceByUri
        | None -> workspace.Folders @ [ updatedWf ]

    { workspace with
        Folders = updatedFolders }

let workspaceWithSolutionsLoaded (lspClient: ILspClient) (clientCapabilities: ClientCapabilities) workspace = async {
    let progressReporter = ProgressReporter(lspClient, clientCapabilities)

    let beginMessage = sprintf "Loading workspace"

    do! progressReporter.Begin(beginMessage)

    let mutable updatedWorkspace = workspace
    let mutable folderNum = 1

    for wf in workspace.Folders do
        let wfRootDir = wf.Uri |> workspaceFolderUriToPath wf

        let beginMessage =
            sprintf "%s (%d/%d)..." (wfRootDir |> Option.defaultValue "???") folderNum workspace.Folders.Length

        do! progressReporter.Report(message = beginMessage)

        let! newSolution =
            solutionLoadSolutionWithPathOrOnDir lspClient progressReporter wf.SolutionPathOverride wfRootDir.Value

        let updatedWf = { wf with Solution = newSolution }
        updatedWorkspace <- updatedWf |> workspaceWithFolder updatedWorkspace

        do! progressReporter.Report(false, sprintf "Finished loading workspace folder %s" wf.Uri)

        folderNum <- folderNum + 1

    let endMessage = sprintf "Finished loading workspace"
    do! progressReporter.End(endMessage)

    return updatedWorkspace
}
