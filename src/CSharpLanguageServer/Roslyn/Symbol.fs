module CSharpLanguageServer.Roslyn.Symbol

open System
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

let symbolGetMetadataName' (containingType: INamedTypeSymbol) =
    let unfoldNamespace (ns: INamespaceSymbol) =
        if ns = null || ns.IsGlobalNamespace then
            None
        else
            Some(ns.Name, ns.ContainingNamespace)

    let namespaceParts =
        Seq.unfold unfoldNamespace containingType.ContainingNamespace |> Seq.rev

    Seq.append namespaceParts [ containingType.MetadataName ] |> String.concat "."

let symbolGetContainingTypeOrThis (symbol: Microsoft.CodeAnalysis.ISymbol) =
    match symbol with
    | :? INamedTypeSymbol as namedType -> namedType
    | _ -> symbol.ContainingType

let symbolGetMetadataName (symbol: Microsoft.CodeAnalysis.ISymbol) =
    symbol |> symbolGetContainingTypeOrThis |> symbolGetMetadataName'
