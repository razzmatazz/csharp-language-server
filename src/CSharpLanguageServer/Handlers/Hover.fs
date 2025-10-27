namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Hover =
    let provider (_cc: ClientCapabilities) : U2<bool, HoverOptions> option = Some(U2.C1 true)

    let makeHoverForSymbol symbol =
        let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol

        let hover =
            { Contents =
                { Kind = MarkupKind.Markdown
                  Value = content }
                |> U3.C1
              Range = None } // TODO: Support range

        hover |> Some

    let handle (context: ServerRequestContext) (p: HoverParams) : AsyncLspResult<Hover option> =
        context.FindSymbol p.TextDocument.Uri p.Position
        |> Async.bindOption (makeHoverForSymbol >> async.Return)
        |> Async.map LspResult.success
