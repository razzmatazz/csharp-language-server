namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! newDoc = Formatter.FormatAsync(doc, options) |> Async.AwaitTask
            let! textEdits = FormatUtil.getChanges newDoc doc
            return textEdits |> Some |> success
    }
