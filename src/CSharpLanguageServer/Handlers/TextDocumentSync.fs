namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types

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

    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind (fun x -> x.Synchronization)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : TextDocumentSyncOptions option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            Some
                { TextDocumentSyncOptions.Default with
                    OpenClose = Some true
                    Save = Some(U2.C2 { IncludeText = Some true })
                    Change = Some TextDocumentSyncKind.Incremental }

    let didOpenRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didOpen"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didChangeRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  SyncKind = TextDocumentSyncKind.Incremental }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didChange"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didSaveRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  IncludeText = Some true }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didSave"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didCloseRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didClose"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let willSaveRegistration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let willSaveWaitUntilRegistration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let didOpen (context: ServerRequestContext) (p: DidOpenTextDocumentParams) : Async<LspResult<unit>> =
        let wf, docAndDocTypeForUri =
            p.TextDocument.Uri |> workspaceDocumentDetails context.Workspace AnyDocument

        match wf, docAndDocTypeForUri with
        | Some(wf), Some(doc, docType) ->
            match docType with
            | UserDocument ->
                // we want to load the document in case it has been changed since we have the solution loaded
                // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                // went out of sync with editor
                let updatedDoc = SourceText.From(p.TextDocument.Text) |> doc.WithText

                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

                context.Emit(
                    WorkspaceFolderChange
                        { wf with
                            Solution = Some updatedDoc.Project.Solution }
                )

                Ok() |> async.Return

            | _ -> Ok() |> async.Return

        | Some wf, None ->
            let docFilePathMaybe = p.TextDocument.Uri |> workspaceFolderUriToPath wf

            match docFilePathMaybe with
            | Some docFilePath -> async {
                // ok, this document is not in solution, register a new document
                let! newDocMaybe = solutionTryAddDocument docFilePath p.TextDocument.Text wf.Solution.Value

                match newDocMaybe with
                | Some newDoc ->
                    context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

                    context.Emit(
                        WorkspaceFolderChange
                            { wf with
                                Solution = Some newDoc.Project.Solution }
                    )

                | None -> ()

                return Ok()
              }

            | None -> Ok() |> async.Return

        | _, _ -> Ok() |> async.Return

    let didChange (context: ServerRequestContext) (p: DidChangeTextDocumentParams) : Async<LspResult<unit>> = async {
        if p.TextDocument.Uri.EndsWith ".cshtml" then
            let wf = p.TextDocument.Uri |> workspaceFolder context.Workspace
            let wf = wf.Value // TODO: handle this

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
            // TODO: Add to project.AdditionalFiles
            return Ok()
        else
            let wf, doc = p.TextDocument.Uri |> workspaceDocument context.Workspace AnyDocument

            match wf, doc with
            | Some _, Some _ -> return Ok()

            | Some wf, None ->
                let docFilePath = p.TextDocument.Uri |> workspaceFolderUriToPath wf

                // we need to add this file to solution if not already
                let! newDocMaybe =
                    match docFilePath with
                    | Some docFilePath -> solutionTryAddDocument docFilePath p.Text.Value wf.Solution.Value
                    | None -> None |> async.Return

                match newDocMaybe with
                | Some newDoc ->
                    let updatedWf =
                        { wf with
                            Solution = Some newDoc.Project.Solution }

                    context.Emit(WorkspaceFolderChange updatedWf)
                    context.Emit(DocumentTouched(p.TextDocument.Uri, DateTime.Now))

                | None -> ()

                return Ok()

            | _, _ -> return Ok()
    }
