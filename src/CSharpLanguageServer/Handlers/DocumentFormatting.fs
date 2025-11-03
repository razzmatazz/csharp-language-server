namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace


[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider (_cc: ClientCapabilities) : U2<bool, DocumentFormattingOptions> option = Some(U2.C1 true)

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

        p.TextDocument.Uri
        |> workspaceDocument context.Workspace UserDocument
        |> async.Return
        |> Async.bindOption (formatDocument lspFormattingOptions)
        |> Async.map LspResult.success
