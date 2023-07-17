namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module Hover =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: TextDocumentPositionParams) : AsyncLspResult<Hover option> = async {
        match! wm.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc) ->
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol semanticModel |> markdown
            let hover =
                { Contents = MarkupContent content
                  // TODO: Support range
                  Range = None }
            return hover |> Some |> success
    }
