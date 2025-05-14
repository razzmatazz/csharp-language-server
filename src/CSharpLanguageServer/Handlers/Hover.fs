namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.Types
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Hover =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.Hover)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, HoverOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> Some (U2.C1 false)
        | false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: HoverRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None
                }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/hover"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: HoverParams) : AsyncLspResult<Hover option> = async {
        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None ->
            return None |> LspResult.success
        | Some (symbol, doc) ->
            let content = DocumentationUtil.markdownDocForSymbolWithSignature symbol
            let hover =
                { Contents = { Kind = MarkupKind.Markdown; Value = content } |> U3.C1
                  // TODO: Support range
                  Range = None }
            return hover |> Some |> LspResult.success
    }
