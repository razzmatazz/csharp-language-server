namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp.Workspace
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
            let docFilePathMaybe = Uri.tryParseFileUri p.TextDocument.Uri

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
        let wf, docMaybe =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match wf, docMaybe with
        | Some wf, Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            //logMessage (sprintf "TextDocumentDidChange: changeParams: %s" (string changeParams))
            //logMessage (sprintf "TextDocumentDidChange: sourceText: %s" (string sourceText))

            let updatedSourceText =
                sourceText |> applyLspContentChangesOnRoslynSourceText p.ContentChanges

            let updatedDoc = doc.WithText(updatedSourceText)

            //logMessage (sprintf "TextDocumentDidChange: newSourceText: %s" (string updatedSourceText))

            let updatedWf =
                { wf with
                    Solution = Some updatedDoc.Project.Solution }

            context.Emit(WorkspaceFolderChange updatedWf)
            context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

        | _, _ -> ()

        return Ok()
    }

    let didClose (context: ServerRequestContext) (p: DidCloseTextDocumentParams) : Async<LspResult<unit>> =
        context.Emit(DocumentClosed p.TextDocument.Uri)
        Ok() |> async.Return

    let willSave (_context: ServerRequestContext) (_p: WillSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        return Ok()
    }

    let willSaveWaitUntil
        (_context: ServerRequestContext)
        (_p: WillSaveTextDocumentParams)
        : AsyncLspResult<TextEdit[] option> =
        async { return LspResult.notImplemented<TextEdit[] option> }

    let didSave (context: ServerRequestContext) (p: DidSaveTextDocumentParams) : Async<LspResult<unit>> =
        let wf, doc = p.TextDocument.Uri |> workspaceDocument context.Workspace AnyDocument

        match wf, doc with
        | Some _, Some doc -> Ok() |> async.Return

        | Some wf, None -> async {
            let docFilePath = Uri.parseFileUri p.TextDocument.Uri

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
          }

        | _, _ -> Ok() |> async.Return
