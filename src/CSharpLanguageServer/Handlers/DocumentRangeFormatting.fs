namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind _.RangeFormatting
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DocumentRangeFormattingOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: DocumentRangeFormattingRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/rangeFormatting"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        let lspFormattingOptions =
            p.Options |> context.State.Settings.GetEffectiveFormattingOptions

        let wf, docForUri =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match docForUri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken

            let! options = getDocumentFormattingOptionSet doc lspFormattingOptions
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let startPos = Position.toRoslynPosition sourceText.Lines p.Range.Start
            let endPos = Position.toRoslynPosition sourceText.Lines p.Range.End
            let! syntaxTree = doc.GetSyntaxRootAsync(ct) |> Async.AwaitTask
            let tokenStart = syntaxTree.FindToken(startPos).FullSpan.Start

            let! newDoc =
                Formatter.FormatAsync(doc, TextSpan.FromBounds(tokenStart, endPos), options, cancellationToken = ct)
                |> Async.AwaitTask

            let! textEdits = getDocumentDiffAsLspTextEdits newDoc doc
            return textEdits |> Some |> LspResult.success
    }
