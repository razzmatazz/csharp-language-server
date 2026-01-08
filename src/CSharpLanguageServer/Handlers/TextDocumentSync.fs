namespace CSharpLanguageServer.Handlers

open System
open System.Text
open System.IO

open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module TextDocumentSync =
    let private applyLspContentChangesOnRoslynSourceText
        (changes: TextDocumentContentChangeEvent[])
        (initialSourceText: SourceText)
        =
        let applyLspContentChangeOnRoslynSourceText (sourceText: SourceText) (change: TextDocumentContentChangeEvent) =
            match change with
            | U2.C1 change ->
                let changeTextSpan =
                    change.Range
                    |> Range.toLinePositionSpan sourceText.Lines
                    |> sourceText.Lines.GetTextSpan

                TextChange(changeTextSpan, change.Text) |> sourceText.WithChanges
            | U2.C2 changeWoRange -> SourceText.From changeWoRange.Text

        changes |> Seq.fold applyLspContentChangeOnRoslynSourceText initialSourceText

    let provider (_cc: ClientCapabilities) : TextDocumentSyncOptions option =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Save = Some(U2.C2 { IncludeText = Some true })
            Change = Some TextDocumentSyncKind.Incremental }
        |> Some

    let didOpen (context: ServerRequestContext) (p: DidOpenTextDocumentParams) : Async<LspResult<unit>> = async {
        if p.TextDocument.Uri.EndsWith ".cshtml" then
            let wf = context.Workspace.SingletonFolder
            let u = p.TextDocument.Uri |> string
            let uri = Uri(u.Replace("%3A", ":", true, null))

            let matchingAdditionalDoc =
                wf.Solution.Value
                |> _.Projects
                |> Seq.collect _.AdditionalDocuments
                |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri)
                |> List.ofSeq

            let doc =
                if matchingAdditionalDoc.Length = 1 then
                    matchingAdditionalDoc |> Seq.head |> Some
                else
                    None

            let newSourceText = SourceText.From(p.TextDocument.Text, Encoding.UTF8)

            match doc with
            | Some doc ->
                let updatedWf =
                    doc.Project
                    |> _.RemoveAdditionalDocument(doc.Id)
                    |> _.AddAdditionalDocument(doc.Name, newSourceText, doc.Folders, doc.FilePath)
                    |> _.Project.Solution
                    |> (fun sln -> { wf with Solution = Some sln })

                context.Emit(WorkspaceFolderChange updatedWf)
                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

            | None ->
                let cshtmlPath = Uri.toPath p.TextDocument.Uri
                let project = solutionGetProjectForPath wf.Solution.Value cshtmlPath

                match project with
                | Some project ->
                    let projectBaseDir = Path.GetDirectoryName project.FilePath
                    let relativePath = Path.GetRelativePath(projectBaseDir, cshtmlPath)

                    let folders = relativePath.Split Path.DirectorySeparatorChar

                    let folders = folders |> Seq.take (folders.Length - 1)

                    let updatedWf =
                        project
                        |> _.AddAdditionalDocument(Path.GetFileName cshtmlPath, newSourceText, folders, cshtmlPath)
                        |> _.Project.Solution
                        |> (fun sln -> { wf with Solution = Some sln })

                    context.Emit(WorkspaceFolderChange updatedWf)
                    context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

                | None -> ()

            return Ok()
        else
            let wf, docInfo =
                workspaceDocumentDetails context.Workspace AnyDocument p.TextDocument.Uri

            match wf, docInfo with
            | Some wf, Some(doc, docType) ->
                match docType with
                | UserDocument ->
                    // we want to load the document in case it has been changed since we have the solution loaded
                    // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                    // went out of sync with editor

                    let updatedWf =
                        p.TextDocument.Text
                        |> SourceText.From
                        |> doc.WithText
                        |> _.Project.Solution
                        |> (fun sln -> { wf with Solution = Some sln })

                    context.Emit(WorkspaceFolderChange updatedWf)
                    context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))
                    return Ok()

                | _ -> return Ok()

            | Some wf, None ->
                let docFilePathMaybe = Util.tryParseFileUri p.TextDocument.Uri

                match docFilePathMaybe with
                | Some docFilePath ->
                    // ok, this document is not in solution, register a new document
                    let! newDocMaybe = solutionTryAddDocument docFilePath p.TextDocument.Text wf.Solution.Value

                    match newDocMaybe with
                    | None -> ()
                    | Some newDoc ->
                        let updatedWf =
                            newDoc |> _.Project.Solution |> (fun sln -> { wf with Solution = Some sln })

                        context.Emit(WorkspaceFolderChange updatedWf)
                        context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

                    return Ok()

                | None -> return Ok()

            | _, _ -> return Ok()
    }

    let didChange (context: ServerRequestContext) (p: DidChangeTextDocumentParams) : Async<LspResult<unit>> = async {
        if p.TextDocument.Uri.EndsWith ".cshtml" then
            let wf = context.Workspace.SingletonFolder

            let u = p.TextDocument.Uri |> string
            let uri = Uri(u.Replace("%3A", ":", true, null))

            let matchingAdditionalDoc =
                wf.Solution.Value
                |> _.Projects
                |> Seq.collect _.AdditionalDocuments
                |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri)
                |> List.ofSeq

            let doc =
                if matchingAdditionalDoc.Length = 1 then
                    matchingAdditionalDoc |> Seq.head |> Some
                else
                    None

            match doc with
            | None -> ()
            | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedSourceText =
                    sourceText |> applyLspContentChangesOnRoslynSourceText p.ContentChanges

                let updatedSolution =
                    doc.Project
                    |> _.RemoveAdditionalDocument(doc.Id)
                    |> _.AddAdditionalDocument(doc.Name, updatedSourceText, doc.Folders, doc.FilePath)
                    |> _.Project.Solution
                    |> Some

                let updatedWf = { wf with Solution = updatedSolution }
                context.Emit(WorkspaceFolderChange updatedWf)
                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

            return Ok()
        else
            let wf, doc = workspaceDocument context.Workspace UserDocument p.TextDocument.Uri

            match wf, doc with
            | Some wf, Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedSolution =
                    sourceText
                    |> applyLspContentChangesOnRoslynSourceText p.ContentChanges
                    |> doc.WithText
                    |> _.Project.Solution
                    |> Some

                let updatedWf = { wf with Solution = updatedSolution }

                context.Emit(WorkspaceFolderChange updatedWf)
                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))
            | _, _ -> ()

            return Ok()
    }

    let didClose (context: ServerRequestContext) (p: DidCloseTextDocumentParams) : Async<LspResult<unit>> = async {
        context.Emit(DocumentClosed p.TextDocument.Uri)
        return Ok()
    }

    let willSave (_context: ServerRequestContext) (_p: WillSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        return Ok()
    }

    let willSaveWaitUntil
        (_context: ServerRequestContext)
        (_p: WillSaveTextDocumentParams)
        : AsyncLspResult<TextEdit[] option> =
        async { return LspResult.notImplemented<TextEdit[] option> }

    let didSave (context: ServerRequestContext) (p: DidSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        if p.TextDocument.Uri.EndsWith ".cshtml" then
            return Ok()
        else
            let wf, doc = p.TextDocument.Uri |> workspaceDocument context.Workspace AnyDocument

            match wf, doc with
            | Some _, Some _ -> return Ok()

            | Some wf, None ->
                let docFilePath = Util.parseFileUri p.TextDocument.Uri

                // we need to add this file to solution if not already
                let! newDocMaybe = solutionTryAddDocument docFilePath p.Text.Value wf.Solution.Value

                match newDocMaybe with
                | Some newDoc ->
                    let updatedWf =
                        { wf with
                            Solution = Some newDoc.Project.Solution }

                    context.Emit(DocumentTouched(p.TextDocument.Uri, DateTime.Now))
                    context.Emit(WorkspaceFolderChange updatedWf)

                | None -> ()

                return Ok()

            | _, _ -> return Ok()
    }
