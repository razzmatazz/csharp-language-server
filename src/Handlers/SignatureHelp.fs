namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open FSharpPlus

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

module SignatureInformation =
    let internal fromMethod (m: IMethodSymbol) =
        let parameters =
            m.Parameters
            |> Seq.map (fun p ->
                { Label = SymbolName.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat p
                  Documentation = None })
            |> Array.ofSeq

        let documentation =
            Documentation.Markup
                { Kind = MarkupKind.Markdown
                  Value = DocumentationUtil.markdownDocForSymbol m }

        { Label         = SymbolName.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat m
          Documentation = Some documentation
          Parameters    = Some parameters }

// Move it to Common? For now, it's only used in this file.
module Seq =
    let inline tryMaxBy (projection: 'T -> 'U) (source: 'T seq): 'T option =
        if isNull source || Seq.isEmpty source then
            None
        else
            Seq.maxBy projection source |> Some

[<RequireQualifiedAccess>]
module SignatureHelp =
    type InvocationContext =
        { Receiver: SyntaxNode
          ArgumentTypes: TypeInfo list
          Separators: SyntaxToken list }

    // Algorithm from omnisharp-roslyn (https://github.com/OmniSharp/omnisharp-roslyn/blob/2d582b05839dbd23baf6e78fa2279163723a824c/src/OmniSharp.Roslyn.CSharp/Services/Signatures/SignatureHelpService.cs#L139C1-L166C10)
    let private methodScore (types: TypeInfo list) (m: IMethodSymbol) =
        let score (invocation: TypeInfo) (definition: IParameterSymbol) =
            if isNull invocation.ConvertedType then
                1
            else if SymbolEqualityComparer.Default.Equals(invocation.ConvertedType, definition.Type) then
                2
            else
                0

        if m.Parameters.Length < types.Length then
            Microsoft.FSharp.Core.int.MinValue
        else
            Seq.zip types m.Parameters |> Seq.map (uncurry score) |> Seq.sum

    let provider: SignatureHelpOptions option =
        Some
            { TriggerCharacters = Some([| '('; ','; '<'; '{'; '[' |])
              RetriggerCharacters = None }

    let handle (wm: IWorkspaceManager) (p: SignatureHelpParams) : AsyncLspResult<SignatureHelp option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

            let position = Position.toRoslynPosition sourceText.Lines p.Position

            let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
            let! root = syntaxTree.GetRootAsync() |> Async.AwaitTask

            let rec findInvocationContext (node: SyntaxNode): InvocationContext option =
                match node with
                | :? InvocationExpressionSyntax as invocation when invocation.ArgumentList.Span.Contains(position) ->
                    Some { Receiver      = invocation.Expression
                           ArgumentTypes = invocation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression)) |> List.ofSeq
                           Separators    = invocation.ArgumentList.Arguments.GetSeparators() |> List.ofSeq }

                | :? BaseObjectCreationExpressionSyntax as objectCreation when
                    objectCreation.ArgumentList
                    |> Option.ofObj
                    |> Option.map (fun argList -> argList.Span.Contains(position))
                    |> Option.defaultValue false
                    ->
                    Some { Receiver      = objectCreation
                           ArgumentTypes = objectCreation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression)) |> List.ofSeq
                           Separators    = objectCreation.ArgumentList.Arguments.GetSeparators() |> List.ofSeq }

                | :? AttributeSyntax as attributeSyntax when
                    attributeSyntax.ArgumentList
                    |> Option.ofObj
                    |> Option.map (fun argList -> argList.Span.Contains(position))
                    |> Option.defaultValue false
                    ->
                    Some { Receiver      = attributeSyntax
                           ArgumentTypes = attributeSyntax.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression)) |> List.ofSeq
                           Separators    = attributeSyntax.ArgumentList.Arguments.GetSeparators() |> List.ofSeq }

                | _ ->
                    node
                    |> Option.ofObj
                    |> Option.bind (fun node -> findInvocationContext node.Parent)

            match root.FindToken(position).Parent |> findInvocationContext with
            | None -> return None |> success
            | Some invocation ->
                let methodGroup =
                    semanticModel.GetMemberGroup(invocation.Receiver).OfType<IMethodSymbol>()
                    |> List.ofSeq

                let matchingMethodMaybe =
                    methodGroup
                    |> Seq.map (fun m -> m, methodScore (invocation.ArgumentTypes) m)
                    |> Seq.tryMaxBy snd
                    |> Option.map fst

                let activeParameterMaybe =
                    invocation.Separators
                    |> List.tryFindIndex (fun comma -> comma.Span.Start >= position)

                let signatureHelpResult =
                    { Signatures = methodGroup |> Seq.map SignatureInformation.fromMethod |> Array.ofSeq
                      ActiveSignature = matchingMethodMaybe |> Option.map (fun m -> List.findIndex ((=) m) methodGroup)
                      ActiveParameter = activeParameterMaybe }

                return Some signatureHelpResult |> success
    }
