namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Hover =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Hover
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, HoverOptions> option =
        match dynamicRegistration cc with
        | true -> Some(U2.C1 false)
        | false -> Some(U2.C1 true)

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: HoverRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/hover"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let makeHoverForSymbol symbol =
        let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol

        let hover =
            { Contents =
                { Kind = MarkupKind.Markdown
                  Value = content }
                |> U3.C1
              Range = None } // TODO: Support range

        hover |> Some

    let handle (context: ServerRequestContext) (p: HoverParams) : AsyncLspResult<Hover option> = async {
        let! wf, symInfo = workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position

        match symInfo with
        | Some(sym, _, _) -> return makeHoverForSymbol sym |> LspResult.success
        | None -> return None |> LspResult.success
    }
