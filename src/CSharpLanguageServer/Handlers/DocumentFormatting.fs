namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DocumentFormattingOptions> option =
        Some (U2.C1 true)

    let handle (context: ServerRequestContext) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit [] option> = async {
        match context.GetUserDocument p.TextDocument.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! options = FormatUtil.getFormattingOptions context.State.Settings doc p.Options
            let! newDoc = Formatter.FormatAsync(doc, options, cancellationToken=ct) |> Async.AwaitTask
            let! textEdits = FormatUtil.getChanges newDoc doc
            return textEdits |> Some |> LspResult.success
    }
