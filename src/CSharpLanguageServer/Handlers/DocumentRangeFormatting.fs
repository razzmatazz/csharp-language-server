namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp.Formatting
open Microsoft.CodeAnalysis.Formatting

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match scope.GetUserDocumentForUri p.TextDocument.Uri with
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
