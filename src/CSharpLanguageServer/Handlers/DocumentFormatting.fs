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

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit [] option> = async {
        match scope.GetUserDocumentForUri p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! newDoc = Formatter.FormatAsync(doc, options) |> Async.AwaitTask
            let! textEdits = FormatUtil.getChanges newDoc doc
            return textEdits |> Some |> success
    }
