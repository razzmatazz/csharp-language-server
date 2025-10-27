namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Document

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DocumentFormattingOptions> option =
        Some(U2.C1 true)

    let handle (context: ServerRequestContext) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
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
            let! newDoc = Formatter.FormatAsync(doc, options, cancellationToken = ct) |> Async.AwaitTask
            let! textEdits = getDocumentDiffAsLspTextEdits newDoc doc
            return textEdits |> Some |> LspResult.success
    }
