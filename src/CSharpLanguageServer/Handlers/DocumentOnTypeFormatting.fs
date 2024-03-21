namespace CSharpLanguageServer.Handlers

open System
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module DocumentOnTypeFormatting =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.OnTypeFormatting)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : DocumentOnTypeFormattingOptions option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false ->
            Some
                { FirstTriggerCharacter = ';'
                  MoreTriggerCharacter = Some([| '}'; ')' |]) }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/onTypeFormatting"
                  RegisterOptions =
                    { FirstTriggerCharacter = ';'
                      MoreTriggerCharacter = Some([| '}'; ')' |])
                      DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

    let handle (wm: IWorkspaceManager) (p: DocumentOnTypeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let options = FormatUtil.getFormattingOptions doc p.Options
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let pos = Position.toRoslynPosition sourceText.Lines p.Position

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
