namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Document

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let provider (_cc: ClientCapabilities) : U2<bool, DocumentRangeFormattingOptions> option = Some(U2.C1 true)

    let handle (context: ServerRequestContext) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        let lspFormattingOptions =
            if context.State.Settings.ApplyFormattingOptions then
                Some p.Options
            else
                None

        match context.GetUserDocument p.TextDocument.Uri with
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
