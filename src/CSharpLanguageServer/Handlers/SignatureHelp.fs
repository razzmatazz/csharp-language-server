namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module SignatureHelp =
    type InvocationContext = {
        Receiver: SyntaxNode
        ArgumentTypes: TypeInfo list
        Separators: SyntaxToken list
    }

    let provider (clientCapabilities: ClientCapabilities option) : SignatureHelpOptions option =
        Some { TriggerCharacters = Some([| '('; ','; '<'; '{'; '[' |])
               RetriggerCharacters = None }

    let handle (scope: ServerRequestScope) (sigHelpParams: SignatureHelpParams): AsyncLspResult<SignatureHelp option> =
        let docMaybe = scope.GetUserDocumentForUri sigHelpParams.TextDocument.Uri
        match docMaybe with
        | Some doc -> async {
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

            let position =
                LinePosition(sigHelpParams.Position.Line, sigHelpParams.Position.Character)
                |> sourceText.Lines.GetPosition

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! root = syntaxTree.GetRootAsync() |> Async.AwaitTask

            let rec findInvocationContext (node: SyntaxNode): InvocationContext option =
                match node with
                | :? InvocationExpressionSyntax as invocation ->
                    match invocation.ArgumentList.Span.Contains(position) with
                    | true ->
                        Some { Receiver      = invocation.Expression
                               ArgumentTypes = (invocation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (invocation.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }
                    | false -> findInvocationContext node.Parent

                | :? BaseObjectCreationExpressionSyntax as objectCreation ->
                    let argumentListContainsPosition =
                        objectCreation.ArgumentList
                        |> Option.ofObj
                        |> Option.map (fun argList -> argList.Span.Contains(position))

                    match argumentListContainsPosition with
                    | Some true ->
                        Some { Receiver      = objectCreation
                               ArgumentTypes = (objectCreation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (objectCreation.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }
                    | _ -> findInvocationContext node.Parent

                | :? AttributeSyntax as attributeSyntax ->
                    let argListContainsPosition =
                        attributeSyntax.ArgumentList
                        |> Option.ofObj
                        |> Option.map (fun argList -> argList.Span.Contains(position))

                    match argListContainsPosition with
                    | Some true ->
                        Some { Receiver      = attributeSyntax
                               ArgumentTypes = (attributeSyntax.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (attributeSyntax.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }

                    | _ -> findInvocationContext node.Parent

                | _ ->
                    match node |> Option.ofObj with
                    | Some nonNullNode -> findInvocationContext nonNullNode.Parent
                    | None -> None

            let invocationMaybe = root.FindToken(position).Parent
                                  |> findInvocationContext
            return
                match invocationMaybe with
                | Some invocation ->
                    let methodGroup =
                        (semanticModel.GetMemberGroup(invocation.Receiver)
                                      .OfType<IMethodSymbol>())
                        |> List.ofSeq

                    let methodScore (m: IMethodSymbol) =
                        if m.Parameters.Length < invocation.ArgumentTypes.Length then
                            -1
                        else
                            let maxParams = Math.Max(m.Parameters.Length, invocation.ArgumentTypes.Length)
                            let paramCountScore = maxParams - Math.Abs(m.Parameters.Length - invocation.ArgumentTypes.Length)

                            let minParams = Math.Min(m.Parameters.Length, invocation.ArgumentTypes.Length)
                            let paramMatchingScore =
                                [0..minParams-1]
                                |> Seq.map (fun pi -> (m.Parameters[pi], invocation.ArgumentTypes[pi]))
                                |> Seq.map (fun (pt, it) -> SymbolEqualityComparer.Default.Equals(pt.Type, it.ConvertedType))
                                |> Seq.map (fun m -> if m then 1 else 0)
                                |> Seq.sum

                            paramCountScore + paramMatchingScore

                    let matchingMethodMaybe =
                        methodGroup
                        |> Seq.map (fun m -> (m, methodScore m))
                        |> Seq.sortByDescending snd
                        |> Seq.map fst
                        |> Seq.tryHead

                    let signatureForMethod (m: IMethodSymbol) =
                        let parameters =
                            m.Parameters
                            |> Seq.map (fun p ->
                                { Label = p.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) |> U2.First
                                  Documentation = None })
                            |> Array.ofSeq

                        let documentation =
                            Documentation.Markup {
                                Kind = MarkupKind.Markdown
                                Value = DocumentationUtil.markdownDocForSymbol m
                            }

                        { Label           = m.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
                          Documentation   = Some documentation
                          Parameters      = Some parameters
                          ActiveParameter = None
                        }

                    let activeParameterMaybe: uint option =
                        match matchingMethodMaybe with
                        | Some _ ->
                            let mutable p: int = 0
                            for comma in invocation.Separators do
                                let commaBeforePos = comma.Span.Start < position
                                p <- p + (if commaBeforePos then 1 else 0)
                            Some (uint p)

                        | None -> None

                    let signatureHelpResult =
                        { Signatures = methodGroup
                                       |> Seq.map signatureForMethod
                                       |> Array.ofSeq

                          ActiveSignature = matchingMethodMaybe
                                            |> Option.map (fun m -> List.findIndex ((=) m) methodGroup)

                          ActiveParameter = activeParameterMaybe }

                    Some signatureHelpResult |> LspResult.success

                | None ->
                    None |> LspResult.success
          }

        | None ->
            None |> LspResult.success |> async.Return
