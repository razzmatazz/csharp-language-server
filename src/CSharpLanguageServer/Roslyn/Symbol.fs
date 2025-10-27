module CSharpLanguageServer.Roslyn.Symbol

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Text.RegularExpressions

open Microsoft.Build.Locator
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Build.Exceptions
open Microsoft.Build.Construction
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text
open Microsoft.Extensions.Logging

open CSharpLanguageServer
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.WorkspaceServices
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Util


type DocumentSymbolCollectorForMatchingSymbolName(documentUri, sym: ISymbol) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations = []
    let mutable suggestedLocations = []

    let collectIdentifier (identifier: SyntaxToken) exactMatch =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span |> Range.fromLinePositionSpan }

        if exactMatch then
            collectedLocations <- location :: collectedLocations
        else
            suggestedLocations <- location :: suggestedLocations

    member __.GetLocations() =
        if not (Seq.isEmpty collectedLocations) then
            collectedLocations |> Seq.rev |> List.ofSeq
        else
            suggestedLocations |> Seq.rev |> List.ofSeq

    override __.Visit(node: SyntaxNode | null) =
        let node = node |> nonNull "node"

        match sym.Kind, node with
        | SymbolKind.Method, (:? MethodDeclarationSyntax as m) when m.Identifier.ValueText = sym.Name ->
            let symMethod = sym :?> IMethodSymbol

            let methodArityMatches =
                symMethod.Parameters.Length = m.ParameterList.Parameters.Count

            collectIdentifier m.Identifier methodArityMatches

        | _, (:? TypeDeclarationSyntax as t) when t.Identifier.ValueText = sym.Name ->
            collectIdentifier t.Identifier false

        | _, (:? PropertyDeclarationSyntax as p) when p.Identifier.ValueText = sym.Name ->
            collectIdentifier p.Identifier false

        | _, (:? EventDeclarationSyntax as e) when e.Identifier.ValueText = sym.Name ->
            collectIdentifier e.Identifier false
        // TODO: collect other type of syntax nodes too

        | _ -> ()


        base.Visit node


let symbolGetContainingTypeOrThis (symbol: ISymbol) : INamedTypeSymbol =
    if symbol :? INamedTypeSymbol then
        symbol :?> INamedTypeSymbol
    else
        symbol.ContainingType


let symbolGetFullReflectionName (containingType: INamedTypeSymbol) =
    let stack = Stack<string>()
    stack.Push containingType.MetadataName
    let mutable ns = containingType.ContainingNamespace

    let mutable doContinue = true

    while doContinue do
        stack.Push ns.Name
        ns <- ns.ContainingNamespace

        doContinue <- ns <> null && not ns.IsGlobalNamespace

    String.Join(".", stack)
