namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Formatting
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, DocumentFormattingOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DocumentFormattingRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/formatting"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let formatDocument lspFormattingOptions doc : Async<TextEdit array option> = async {
        let! ct = Async.CancellationToken
        let! options = getDocumentFormattingOptionSet doc lspFormattingOptions
        let! newDoc = Formatter.FormatAsync(doc, options, cancellationToken = ct) |> Async.AwaitTask
        let! textEdits = getDocumentDiffAsLspTextEdits newDoc doc
        return textEdits |> Some
    }

    let handle (context: ServerRequestContext) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit[] option> =
        let lspFormattingOptions =
            p.Options |> context.State.Settings.GetEffectiveFormattingOptions

        let wf, doc = p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        doc
        |> async.Return
        |> Async.bindOption (formatDocument lspFormattingOptions)
        |> Async.map LspResult.success
