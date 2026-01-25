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

type LspWorkspaceOpenDocInfo = { Version: int; Touched: DateTime }

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      OpenDocs: Map<string, LspWorkspaceOpenDocInfo> }

    static member Empty = { Folders = []; OpenDocs = Map.empty }

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

let workspaceDocumentDetails (workspace: LspWorkspace) docType (u: string) =
    let wf = workspaceFolder workspace u

    let docAndDocType =
        match wf with
        | None -> None
        | Some wf -> workspaceFolderDocumentDetails wf docType u

    wf, docAndDocType

let workspaceDocument workspace docType (u: string) =
    let wf, docAndType = workspaceDocumentDetails workspace docType u
    let doc = docAndType |> Option.map fst
    wf, doc

let workspaceDocumentSemanticModel (workspace: LspWorkspace) (uri: DocumentUri) = async {
    let wf = workspaceFolder workspace uri

    match wf with
    | None -> return None, None
    | Some wf ->
        if uri.EndsWith ".cshtml" then
            let cshtmlPath = workspaceFolderUriToPath wf uri

            match wf.Solution, cshtmlPath with
            | Some solution, Some cshtmlPath ->
                match! solutionGetRazorDocumentForPath solution cshtmlPath with
                | None -> return Some wf, None
                | Some(_, compilation, cshtmlTree) ->
                    let semanticModel = compilation.GetSemanticModel(cshtmlTree) |> Option.ofObj
                    return Some wf, semanticModel

            | _, _ -> return None, None
        else
            let wf, docAndType = workspaceDocumentDetails workspace AnyDocument uri

            match docAndType with
            | Some(doc, _) ->
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask |> Async.map Option.ofObj
                return wf, semanticModel

            | None -> return wf, None
}

let workspaceDocumentSymbol
    (workspace: LspWorkspace)
    docType
    (uri: DocumentUri)
    (pos: Ionide.LanguageServerProtocol.Types.Position)
    =
    async {
        let wf = workspaceFolder workspace uri

        match wf, uri.EndsWith ".cshtml" with
        | None, _ -> return None, None

        | Some wf, true ->
            let cshtmlPath = workspaceFolderUriToPath wf uri

            match wf.Solution, cshtmlPath with
            | Some solution, Some cshtmlPath ->
                let! symbolInfo = solutionFindSymbolForRazorDocumentPath solution cshtmlPath pos
                return Some wf, symbolInfo

            | _, _ -> return Some wf, None

        | Some wf, false ->
            let docForUri = uri |> workspaceFolderDocumentDetails wf docType

            match docForUri with
            | None -> return Some wf, None
            | Some(doc, _) ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let position = Position.toRoslynPosition sourceText.Lines pos
                let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

                let symbolInfo =
                    symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)

                return Some wf, symbolInfo
    }

let workspaceDocumentVersion workspace uri =
    uri |> workspace.OpenDocs.TryFind |> Option.map _.Version

let workspaceWithSolutionsLoaded
    (settings: ServerSettings)
    (lspClient: ILspClient)
    (clientCapabilities: ClientCapabilities)
    workspace
    =
    async {
        let progressReporter = ProgressReporter(lspClient, clientCapabilities)

        let beginMessage =
            sprintf "Loading workspace (%d workspace folders)" workspace.Folders.Length

        do! progressReporter.Begin(beginMessage)

        let mutable updatedWorkspace = workspace

        for wf in workspace.Folders do
            let beginMessage = sprintf "Loading workspace folder %s..." wf.Uri

            let wfRootDir = wf.Uri |> workspaceFolderUriToPath wf

            let! newSolution =
                solutionLoadSolutionWithPathOrOnDir lspClient progressReporter settings.SolutionPath wfRootDir.Value

            let updatedWf = { wf with Solution = newSolution }
            updatedWorkspace <- updatedWf |> workspaceWithFolder updatedWorkspace

            do! progressReporter.Report(false, sprintf "Finished loading workspace folder %s" wf.Uri)

        let endMessage = sprintf "Finished loading workspace"
        do! progressReporter.End(endMessage)

        return updatedWorkspace
    }
