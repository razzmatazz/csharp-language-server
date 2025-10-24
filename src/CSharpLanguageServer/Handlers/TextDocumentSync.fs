namespace CSharpLanguageServer.Handlers

open System
open System.Text
open System.IO

open Microsoft.Extensions.Logging
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module TextDocumentSync =
    let private logger = Logging.getLoggerByName "TextDocumentSync"

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
            | U2.C2 changeWoRange -> SourceText.From(changeWoRange.Text)

        changes |> Seq.fold applyLspContentChangeOnRoslynSourceText initialSourceText

    let provider (_: ClientCapabilities) : TextDocumentSyncOptions option =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Save = Some(U2.C2 { IncludeText = Some true })
            Change = Some TextDocumentSyncKind.Incremental }
        |> Some


    let didOpen (context: ServerRequestContext) (openParams: DidOpenTextDocumentParams) : Async<LspResult<unit>> =
        if openParams.TextDocument.Uri.EndsWith(".cshtml") then
            let u = openParams.TextDocument.Uri |> string
            let uri = Uri(u.Replace("%3A", ":", true, null))

            let matchingAdditionalDoc =
                context.Solution.Projects
                |> Seq.collect (fun p -> p.AdditionalDocuments)
                |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri)
                |> List.ofSeq

            let doc =
                if matchingAdditionalDoc.Length = 1 then
                    matchingAdditionalDoc |> Seq.head |> Some
                else
                    None

            let newSourceText = SourceText.From(openParams.TextDocument.Text, Encoding.UTF8)

            match doc with
            | Some doc ->
                let updatedDoc =
                    doc.Project
                    |> _.RemoveAdditionalDocument(doc.Id)
                    |> _.AddAdditionalDocument(doc.Name, newSourceText, doc.Folders, doc.FilePath)

                context.Emit(OpenDocAdd(openParams.TextDocument.Uri, openParams.TextDocument.Version, DateTime.Now))
                context.Emit(SolutionChange updatedDoc.Project.Solution)

            | None ->
                let cshtmlPath = Uri.toPath openParams.TextDocument.Uri
                let project = getProjectForPathOnSolution context.Solution cshtmlPath

                match project with
                | Some project ->
                    let projectBaseDir = Path.GetDirectoryName(project.FilePath)
                    let relativePath = Path.GetRelativePath(projectBaseDir, cshtmlPath)

                    let folders = relativePath.Split(Path.DirectorySeparatorChar)

                    let folders = folders |> Seq.take (folders.Length - 1)

                    let newDoc =
                        project.AddAdditionalDocument(Path.GetFileName(cshtmlPath), newSourceText, folders, cshtmlPath)

                    context.Emit(OpenDocAdd(openParams.TextDocument.Uri, openParams.TextDocument.Version, DateTime.Now))
                    context.Emit(SolutionChange newDoc.Project.Solution)
                    ()

                | None -> ()

            Ok() |> async.Return
        else
            match context.GetDocumentForUriOfType AnyDocument openParams.TextDocument.Uri with
            | Some(doc, docType) ->
                match docType with
                | UserDocument ->
                    // we want to load the document in case it has been changed since we have the solution loaded
                    // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                    // went out of sync with editor
                    let updatedDoc = SourceText.From(openParams.TextDocument.Text) |> doc.WithText

                    context.Emit(OpenDocAdd(openParams.TextDocument.Uri, openParams.TextDocument.Version, DateTime.Now))
                    context.Emit(SolutionChange updatedDoc.Project.Solution)

                    Ok() |> async.Return

                | _ -> Ok() |> async.Return

            | None ->
                let docFilePathMaybe = Util.tryParseFileUri openParams.TextDocument.Uri

                match docFilePathMaybe with
                | Some docFilePath -> async {
                    // ok, this document is not in solution, register a new document
                    let! newDocMaybe = solutionTryAddDocument logger context.Solution docFilePath openParams.TextDocument.Text

                    match newDocMaybe with
                    | Some newDoc ->
                        context.Emit(
                            OpenDocAdd(openParams.TextDocument.Uri, openParams.TextDocument.Version, DateTime.Now)
                        )

                        context.Emit(SolutionChange newDoc.Project.Solution)

                    | None -> ()

                    return Ok()
                  }
                | None -> Ok() |> async.Return

    let didChange (context: ServerRequestContext) (changeParams: DidChangeTextDocumentParams) : Async<LspResult<unit>> = async {
        if changeParams.TextDocument.Uri.EndsWith(".cshtml") then
            let u = changeParams.TextDocument.Uri |> string
            let uri = Uri(u.Replace("%3A", ":", true, null))

            let matchingAdditionalDoc =
                context.Solution.Projects
                |> Seq.collect (fun p -> p.AdditionalDocuments)
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
                    sourceText
                    |> applyLspContentChangesOnRoslynSourceText changeParams.ContentChanges

                let updatedDoc =
                    doc.Project
                    |> _.RemoveAdditionalDocument(doc.Id)
                    |> _.AddAdditionalDocument(doc.Name, updatedSourceText, doc.Folders, doc.FilePath)

                context.Emit(OpenDocAdd(changeParams.TextDocument.Uri, changeParams.TextDocument.Version, DateTime.Now))
                context.Emit(SolutionChange updatedDoc.Project.Solution)

            return Ok()
        else
            let docMaybe = context.GetUserDocument changeParams.TextDocument.Uri

            match docMaybe with
            | None -> ()
            | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedSourceText =
                    sourceText
                    |> applyLspContentChangesOnRoslynSourceText changeParams.ContentChanges

                let updatedSolution = doc.WithText(updatedSourceText).Project.Solution

                context.Emit(SolutionChange updatedSolution)
                context.Emit(OpenDocAdd(changeParams.TextDocument.Uri, changeParams.TextDocument.Version, DateTime.Now))

            return Ok()
    }

    let didClose (context: ServerRequestContext) (closeParams: DidCloseTextDocumentParams) : Async<LspResult<unit>> =
        if closeParams.TextDocument.Uri.EndsWith(".cshtml") then
            Ok() |> async.Return
        else
            context.Emit(OpenDocRemove closeParams.TextDocument.Uri)
            Ok() |> async.Return

    let willSave (_context: ServerRequestContext) (_p: WillSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        return Ok()
    }

    let willSaveWaitUntil
        (_context: ServerRequestContext)
        (_p: WillSaveTextDocumentParams)
        : AsyncLspResult<TextEdit[] option> =
        async { return LspResult.notImplemented<TextEdit[] option> }

    let didSave (context: ServerRequestContext) (saveParams: DidSaveTextDocumentParams) : Async<LspResult<unit>> =
        if saveParams.TextDocument.Uri.EndsWith(".cshtml") then
            Ok() |> async.Return
        else
            // we need to add this file to solution if not already
            let doc = context.GetDocument saveParams.TextDocument.Uri

            match doc with
            | Some _ -> Ok() |> async.Return

            | None -> async {
                let docFilePath = Util.parseFileUri saveParams.TextDocument.Uri
                let! newDocMaybe = solutionTryAddDocument logger context.Solution docFilePath saveParams.Text.Value

                match newDocMaybe with
                | Some newDoc ->
                    context.Emit(OpenDocTouch(saveParams.TextDocument.Uri, DateTime.Now))
                    context.Emit(SolutionChange newDoc.Project.Solution)

                | None -> ()

                return Ok()
              }
