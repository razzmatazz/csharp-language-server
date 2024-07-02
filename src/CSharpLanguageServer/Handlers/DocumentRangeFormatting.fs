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
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.RangeFormatting)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DocumentRangeFormattingOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: DocumentRangeFormattingRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None
                }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/rangeFormatting"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match context.GetUserDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let startPos = Position.toRoslynPosition sourceText.Lines p.Range.Start
            let endPos = Position.toRoslynPosition sourceText.Lines p.Range.End
            let! syntaxTree = doc.GetSyntaxRootAsync(ct) |> Async.AwaitTask
            let tokenStart = syntaxTree.FindToken(startPos).FullSpan.Start
            let! newDoc =
                Formatter.FormatAsync(doc, TextSpan.FromBounds(tokenStart, endPos), options, cancellationToken=ct)
                |> Async.AwaitTask
            let! textEdits = FormatUtil.getChanges newDoc doc
            return textEdits |> Some |> success
    }
