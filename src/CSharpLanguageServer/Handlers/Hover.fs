namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Hover =
    let provider (_: ClientCapabilities) : U2<bool, HoverOptions> option = Some(U2.C1 true)

    let handle (context: ServerRequestContext) (p: HoverParams) : AsyncLspResult<Hover option> = async {
        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> LspResult.success
        | Some(symbol, _, _) ->
            let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol

            let hover =
                { Contents =
                    { Kind = MarkupKind.Markdown
                      Value = content }
                    |> U3.C1
                  // TODO: Support range
                  Range = None }

            return hover |> Some |> LspResult.success
    }
