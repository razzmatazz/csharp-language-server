namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let startPos = Position.toRoslynPosition sourceText.Lines p.Range.Start
            let endPos = Position.toRoslynPosition sourceText.Lines p.Range.End
            let! syntaxTree = doc.GetSyntaxRootAsync() |> Async.AwaitTask
            let tokenStart = syntaxTree.FindToken(startPos).FullSpan.Start
            let! newDoc = Formatter.FormatAsync(doc, TextSpan.FromBounds(tokenStart, endPos), options) |> Async.AwaitTask
            let! textEdits = FormatUtil.getChanges newDoc doc
            return textEdits |> Some |> success
    }
