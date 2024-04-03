namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.RangeFormatting)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/rangeFormatting"
                  RegisterOptions = { DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

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
