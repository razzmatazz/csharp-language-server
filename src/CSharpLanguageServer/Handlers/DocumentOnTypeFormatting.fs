namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module DocumentOnTypeFormatting =
    let provider (_: ClientCapabilities) : DocumentOnTypeFormattingOptions option =
        Some
            { FirstTriggerCharacter = ";"
              MoreTriggerCharacter = Some([| "}"; ")" |]) }

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
        match context.GetUserDocument p.TextDocument.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! options = FormatUtil.getFormattingOptions context.State.Settings doc p.Options
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

                    let! textEdits = FormatUtil.getChanges newDoc doc
                    return textEdits |> Some |> LspResult.success
            | _ -> return None |> LspResult.success
    }
