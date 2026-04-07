namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.RangeFormatting
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, DocumentRangeFormattingOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DocumentRangeFormattingRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpDocuments |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/rangeFormatting"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: RequestContext)
        (p: DocumentRangeFormattingParams)
        : Async<LspResult<TextEdit[] option> * RequestEffects> =
        async {
            let lspFormattingOptions = p.Options |> context.Config.GetEffectiveFormattingOptions

            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

            let docForUri =
                wf |> Option.bind (workspaceFolderDocument UserDocument p.TextDocument.Uri)

            match docForUri with
            | None -> return None |> LspResult.success, RequestEffects.Empty
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
                return textEdits |> Some |> LspResult.success, RequestEffects.Empty
        }
