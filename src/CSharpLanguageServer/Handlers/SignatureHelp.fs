namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

module SignatureInformation =
    let internal fromMethod (m: IMethodSymbol) =
        let parameters =
            m.Parameters
            |> Seq.map (fun p ->
                { Label = SymbolName.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat p |> U2.C1
                  Documentation = None })
            |> Array.ofSeq

        let documentation =
            { Kind = MarkupKind.Markdown
              Value = DocumentationUtil.markdownDocForSymbol m }
            |> U2.C2

        { Label = SymbolName.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat m
          Documentation = Some documentation
          Parameters = Some parameters
          ActiveParameter = None }

[<RequireQualifiedAccess>]
module SignatureHelp =
    type InvocationContext =
        { Receiver: SyntaxNode
          ArgumentTypes: TypeInfo list
          Separators: SyntaxToken list }

    // Algorithm from omnisharp-roslyn (https://github.com/OmniSharp/omnisharp-roslyn/blob/2d582b05839dbd23baf6e78fa2279163723a824c/src/OmniSharp.Roslyn.CSharp/Services/Signatures/SignatureHelpService.cs#L139C1-L166C10)
    let private methodScore (types: TypeInfo list) (m: IMethodSymbol) =
        let score (invocation: TypeInfo, definition: IParameterSymbol) =
            if isNull invocation.ConvertedType then
                1
            else if SymbolEqualityComparer.Default.Equals(invocation.ConvertedType, definition.Type) then
                2
            else
                0

        if m.Parameters.Length < types.Length then
            Microsoft.FSharp.Core.int.MinValue
        else
            Seq.zip types m.Parameters |> Seq.map score |> Seq.sum

    let provider (_cc: ClientCapabilities) : SignatureHelpOptions option =
        { TriggerCharacters = Some([| "("; ","; "<"; "{"; "[" |])
          WorkDoneProgress = None
          RetriggerCharacters = None }
        |> Some

    let handle (context: ServerRequestContext) (p: SignatureHelpParams) : AsyncLspResult<SignatureHelp option> = async {
        let wf, docMaybe =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match docMaybe with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

            let position = p.Position |> Position.toRoslynPosition sourceText.Lines

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask

            let rec findInvocationContext (node: SyntaxNode) : InvocationContext option =
                match node with
                | :? InvocationExpressionSyntax as invocation when invocation.ArgumentList.Span.Contains(position) ->
                    Some
                        { Receiver = invocation.Expression
                          ArgumentTypes =
                            invocation.ArgumentList.Arguments
                            |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))
                            |> List.ofSeq
                          Separators = invocation.ArgumentList.Arguments.GetSeparators() |> List.ofSeq }

                | :? BaseObjectCreationExpressionSyntax as objectCreation when
                    objectCreation.ArgumentList
                    |> Option.ofObj
                    |> Option.map (fun argList -> argList.Span.Contains(position))
                    |> Option.defaultValue false
                    ->
                    let argumentList = objectCreation.ArgumentList |> Option.ofObj

                    Some
                        { Receiver = objectCreation
                          ArgumentTypes =
                            argumentList
                            |> Option.map (fun al -> al.Arguments |> List.ofSeq)
                            |> Option.defaultValue []
                            |> List.map (fun a -> semanticModel.GetTypeInfo(a.Expression))
                          Separators =
                            argumentList
                            |> Option.map (fun al -> al.Arguments.GetSeparators() |> List.ofSeq)
                            |> Option.defaultValue [] }

                | :? AttributeSyntax as attributeSyntax when
                    attributeSyntax.ArgumentList
                    |> Option.ofObj
                    |> Option.map (fun argList -> argList.Span.Contains(position))
                    |> Option.defaultValue false
                    ->
                    let argumentList = attributeSyntax.ArgumentList |> Option.ofObj

                    Some
                        { Receiver = attributeSyntax
                          ArgumentTypes =
                            argumentList
                            |> Option.map (fun al -> al.Arguments |> List.ofSeq)
                            |> Option.defaultValue []
                            |> List.map (fun a -> semanticModel.GetTypeInfo(a.Expression))
                          Separators =
                            argumentList
                            |> Option.map (fun al -> al.Arguments.GetSeparators() |> List.ofSeq)
                            |> Option.defaultValue [] }

                | _ ->
                    node
                    |> Option.ofObj
                    |> Option.bind (fun n -> n.Parent |> Option.ofObj)
                    |> Option.bind findInvocationContext

            match root.FindToken(position).Parent |> findInvocationContext with
            | None -> return None |> LspResult.success
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
                    |> Option.map uint

                let signatureHelpResult =
                    { Signatures = methodGroup |> Seq.map SignatureInformation.fromMethod |> Array.ofSeq
                      ActiveSignature =
                        matchingMethodMaybe
                        |> Option.map (fun m -> List.findIndex ((=) m) methodGroup |> uint32)
                      ActiveParameter = activeParameterMaybe }

                return Some signatureHelpResult |> LspResult.success
    }
