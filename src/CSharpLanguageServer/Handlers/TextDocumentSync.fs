namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module TextDocumentSync =
    let private logger = LogProvider.getLoggerByName "TextDocumentSync"

    let private applyLspContentChangesOnRoslynSourceText
            (changes: TextDocumentContentChangeEvent[])
            (initialSourceText: SourceText) =

        let applyLspContentChangeOnRoslynSourceText (sourceText: SourceText) (change: TextDocumentContentChangeEvent) =
            match change.Range with
            | Some changeRange ->
                let changeTextSpan =
                    changeRange |> Range.toLinePositionSpan sourceText.Lines
                                |> sourceText.Lines.GetTextSpan

                TextChange(changeTextSpan, change.Text) |> sourceText.WithChanges

            | None -> SourceText.From(change.Text)

        changes |> Seq.fold applyLspContentChangeOnRoslynSourceText initialSourceText


    let provider (clientCapabilities: ClientCapabilities option) : TextDocumentSyncOptions option =
        Some
            { TextDocumentSyncOptions.Default with
                OpenClose = Some true
                Save = Some { IncludeText = Some true }
                Change = Some TextDocumentSyncKind.Incremental }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let didOpen (diagnosticsPost: DiagnosticsEvent -> unit)
                (context: ServerRequestContext)
                (openParams: DidOpenTextDocumentParams)
            : Async<LspResult<unit>> =
        match context.GetDocumentForUriOfType AnyDocument openParams.TextDocument.Uri with
        | Some (doc, docType) ->
            match docType with
            | UserDocument ->
                // we want to load the document in case it has been changed since we have the solution loaded
                // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                // went out of sync with editor
                let updatedDoc = SourceText.From(openParams.TextDocument.Text) |> doc.WithText

                context.Emit(OpenDocVersionAdd (openParams.TextDocument.Uri, openParams.TextDocument.Version))
                context.Emit(SolutionChange updatedDoc.Project.Solution)

                diagnosticsPost(
                    DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))

                LspResult.Ok() |> async.Return

            | _ ->
                LspResult.Ok() |> async.Return

        | None ->
            let docFilePathMaybe = Util.tryParseFileUri openParams.TextDocument.Uri

            match docFilePathMaybe with
            | Some docFilePath -> async {
                // ok, this document is not on solution, register a new document
                let! newDocMaybe =
                    tryAddDocument
                        logger
                        docFilePath
                        openParams.TextDocument.Text
                        context.Solution

                match newDocMaybe with
                | Some newDoc ->
                    context.Emit(SolutionChange newDoc.Project.Solution)

                    diagnosticsPost(
                        DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))

                | None -> ()

                return LspResult.Ok()
              }

            | None ->
                LspResult.Ok() |> async.Return

    let didChange (diagnosticsPost: DiagnosticsEvent -> unit)
                  (context: ServerRequestContext)
                  (changeParams: DidChangeTextDocumentParams)
            : Async<LspResult<unit>> =
      async {
        let docMaybe = context.GetUserDocument changeParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                //logMessage (sprintf "TextDocumentDidChange: changeParams: %s" (string changeParams))
                //logMessage (sprintf "TextDocumentDidChange: sourceText: %s" (string sourceText))

                let updatedSourceText = sourceText |> applyLspContentChangesOnRoslynSourceText changeParams.ContentChanges
                let updatedDoc = doc.WithText(updatedSourceText)

                //logMessage (sprintf "TextDocumentDidChange: newSourceText: %s" (string updatedSourceText))

                let updatedSolution = updatedDoc.Project.Solution

                context.Emit(SolutionChange updatedSolution)
                context.Emit(OpenDocVersionAdd (changeParams.TextDocument.Uri, changeParams.TextDocument.Version))

                diagnosticsPost(
                    DocumentOpenOrChange (changeParams.TextDocument.Uri, DateTime.Now))

        | None -> ()

        return Result.Ok()
    }

    let didClose (diagnosticsPost: DiagnosticsEvent -> unit)
                 (context: ServerRequestContext)
                 (closeParams: DidCloseTextDocumentParams)
            : Async<LspResult<unit>> =
        context.Emit(OpenDocVersionRemove closeParams.TextDocument.Uri)
        diagnosticsPost(DocumentClose closeParams.TextDocument.Uri)
        LspResult.Ok() |> async.Return

    let didSave (diagnosticsPost: DiagnosticsEvent -> unit)
                (context: ServerRequestContext)
                (saveParams: DidSaveTextDocumentParams)
            : Async<LspResult<unit>> =
        // we need to add this file to solution if not already
        let doc = context.GetDocument saveParams.TextDocument.Uri

        match doc with
        | Some _ ->
            LspResult.Ok() |> async.Return

        | None -> async {
            let docFilePath = Util.parseFileUri saveParams.TextDocument.Uri
            let! newDocMaybe =
                tryAddDocument
                    logger
                    docFilePath
                    saveParams.Text.Value
                    context.Solution

            match newDocMaybe with
            | Some newDoc ->
                context.Emit(SolutionChange newDoc.Project.Solution)

                diagnosticsPost(
                    DocumentOpenOrChange (saveParams.TextDocument.Uri, DateTime.Now))

            | None -> ()

            return LspResult.Ok()
          }
