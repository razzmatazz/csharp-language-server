namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.CSharp.Syntax

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module DocumentOnTypeFormatting =
    let provider (clientCapabilities: ClientCapabilities option) : DocumentOnTypeFormattingOptions option =
        Some
            { FirstTriggerCharacter = ';'
              MoreTriggerCharacter = Some([| '}'; ')' |]) }

    let handle (scope: ServerRequestScope) (p: DocumentOnTypeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match scope.GetUserDocumentForUri p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let pos = sourceText.Lines.GetPosition(new LinePosition(p.Position.Line, p.Position.Character))

            match p.Ch with
            | ';'
            | '}'
            | ')' ->
                let! root = doc.GetSyntaxRootAsync() |> Async.AwaitTask
                match FormatUtil.findFormatTarget root pos with
                | None -> return None |> success
                | Some node ->
                    let! newDoc = Formatter.FormatAsync(doc, TextSpan.FromBounds(node.FullSpan.Start, node.FullSpan.End), options) |> Async.AwaitTask
                    let! textEdits = FormatUtil.getChanges newDoc doc
                    return textEdits |> Some |> success
            | _ -> return None |> success
    }
