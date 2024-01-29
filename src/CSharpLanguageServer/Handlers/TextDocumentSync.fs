namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module TextDocumentSync =
    let provider (clientCapabilities: ClientCapabilities option) : TextDocumentSyncOptions option =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Save = Some { IncludeText = Some true }
            Change = Some TextDocumentSyncKind.Incremental
        }
        |> Some

    let didOpen (logMessage: Util.AsyncLogFn)
                (diagnosticsPost: DiagnosticsEvent -> unit)
                (scope: ServerRequestScope)
                (openParams: DidOpenTextDocumentParams)
            : Async<LspResult<unit>> =
        match scope.GetDocumentForUriOfType AnyDocument openParams.TextDocument.Uri with
        | Some (doc, docType) ->
            match docType with
            | UserDocument ->
                // we want to load the document in case it has been changed since we have the solution loaded
                // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                // went out of sync with editor
                let updatedDoc = SourceText.From(openParams.TextDocument.Text) |> doc.WithText

                scope.Emit(OpenDocVersionAdd (openParams.TextDocument.Uri, openParams.TextDocument.Version))
                scope.Emit(SolutionChange updatedDoc.Project.Solution)

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
                        logMessage
                        docFilePath
                        openParams.TextDocument.Text
                        scope.Solution

                match newDocMaybe with
                | Some newDoc ->
                    scope.Emit(SolutionChange newDoc.Project.Solution)

                    diagnosticsPost(
                        DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))

                | None -> ()

                return LspResult.Ok()
              }

            | None ->
                LspResult.Ok() |> async.Return

    let didChange (diagnosticsPost: DiagnosticsEvent -> unit)
                  (scope: ServerRequestScope)
                  (changeParams: DidChangeTextDocumentParams)
            : Async<LspResult<unit>> =
      async {
        let docMaybe = scope.GetUserDocumentForUri changeParams.TextDocument.Uri
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

                scope.Emit(SolutionChange updatedSolution)
                scope.Emit(OpenDocVersionAdd (changeParams.TextDocument.Uri, changeParams.TextDocument.Version))

                diagnosticsPost(
                    DocumentOpenOrChange (changeParams.TextDocument.Uri, DateTime.Now))

        | None -> ()

        return Result.Ok()
    }

    let didClose (diagnosticsPost: DiagnosticsEvent -> unit)
                 (scope: ServerRequestScope)
                 (closeParams: DidCloseTextDocumentParams)
            : Async<LspResult<unit>> =
        scope.Emit(OpenDocVersionRemove closeParams.TextDocument.Uri)
        diagnosticsPost(DocumentClose closeParams.TextDocument.Uri)
        LspResult.Ok() |> async.Return

    let didSave (logMessage: Util.AsyncLogFn)
                (diagnosticsPost: DiagnosticsEvent -> unit)
                (scope: ServerRequestScope)
                (saveParams: DidSaveTextDocumentParams)
            : Async<LspResult<unit>> =
        // we need to add this file to solution if not already
        let doc = scope.GetAnyDocumentForUri saveParams.TextDocument.Uri

        match doc with
        | Some _ ->
            LspResult.Ok() |> async.Return

        | None -> async {
            let docFilePath = Util.parseFileUri saveParams.TextDocument.Uri
            let! newDocMaybe =
                tryAddDocument
                    logMessage
                    docFilePath
                    saveParams.Text.Value
                    scope.Solution

            match newDocMaybe with
            | Some newDoc ->
                scope.Emit(SolutionChange newDoc.Project.Solution)

                diagnosticsPost(
                    DocumentOpenOrChange (saveParams.TextDocument.Uri, DateTime.Now))

            | None -> ()

            return LspResult.Ok()
          }
