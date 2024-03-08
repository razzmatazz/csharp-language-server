namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.Types
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Hover =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (p: TextDocumentPositionParams) : AsyncLspResult<Hover option> = async {
        match! scope.GetSymbolAtPositionOnAnyDocument p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc, pos) ->
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol semanticModel |> markdown
            let hover =
                { Contents = MarkupContent content
                  // TODO: Support range
                  Range = None }
            return hover |> Some |> success
    }
