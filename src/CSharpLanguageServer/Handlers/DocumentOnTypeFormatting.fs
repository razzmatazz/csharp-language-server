namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentOnTypeFormatting =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.OnTypeFormatting
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : DocumentOnTypeFormattingOptions option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            Some
                { FirstTriggerCharacter = ";"
                  MoreTriggerCharacter = Some([| "}"; ")" |]) }

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DocumentOnTypeFormattingRegistrationOptions =
                { FirstTriggerCharacter = ";"
                  MoreTriggerCharacter = Some([| "}"; ")" |])
                  DocumentSelector = documentSelectorForCSharpDocuments |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/onTypeFormatting"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let rec getSyntaxNode (token: SyntaxToken) : SyntaxNode option =
        if token.IsKind(SyntaxKind.EndOfFileToken) then
            getSyntaxNode (token.GetPreviousToken())
        else
            match token.Kind() with
            | SyntaxKind.SemicolonToken -> token.Parent |> Some
            | SyntaxKind.CloseBraceToken ->
                match token.Parent |> Option.ofObj with
                | Some parent ->
                    match parent.Kind() with
                    | SyntaxKind.Block -> parent.Parent |> Some
                    | _ -> parent |> Some
                | None -> None
            | SyntaxKind.CloseParenToken ->
                if
                    token.GetPreviousToken().IsKind(SyntaxKind.SemicolonToken)
                    && token.Parent.IsKind(SyntaxKind.ForStatement)
                then
                    token.Parent |> Some
                else
                    None
            | _ -> None

    let handle (context: ServerRequestContext) (p: DocumentOnTypeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
        let lspFormattingOptions =
            p.Options |> context.State.Settings.GetEffectiveFormattingOptions

        let wf, docForUri =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match docForUri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! options = getDocumentFormattingOptionSet doc lspFormattingOptions
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let pos = Position.toRoslynPosition sourceText.Lines p.Position

            match p.Ch with
            | ";"
            | "}"
            | ")" ->
                let! root = doc.GetSyntaxRootAsync(ct) |> Async.AwaitTask
                let tokenAtPos = root.FindToken pos

                match getSyntaxNode tokenAtPos with
                | None -> return None |> LspResult.success
                | Some node ->
                    let! newDoc =
                        Formatter.FormatAsync(
                            doc,
                            TextSpan.FromBounds(node.FullSpan.Start, node.FullSpan.End),
                            options,
                            cancellationToken = ct
                        )
                        |> Async.AwaitTask

                    let! textEdits = getDocumentDiffAsLspTextEdits newDoc doc
                    return textEdits |> Some |> LspResult.success
            | _ -> return None |> LspResult.success
    }
