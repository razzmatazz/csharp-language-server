namespace CSharpLanguageServer.Workspace

open System
open System.IO
open System.Collections.Generic
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.TypeSystem
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common
open CSharpLanguageServer.Handlers

module Util =

    type Interesting =
        | Solution of string
        | Projects of string []

    // TODO: Ignore some dirs? Like .git, node_modules.
    // TODO: Is it possible to make it lazy? It will start to find csproj files only if it can't find sln file.
    let findInteresting (rootDir: string) : Interesting option =
        try
            match Directory.EnumerateFiles(rootDir, "*.sln", SearchOption.AllDirectories) |> Seq.tryHead with
            | Some slnPath ->
                Some (Solution slnPath)
            | None ->
                let projs = Directory.EnumerateFiles(rootDir, "*.csproj", SearchOption.AllDirectories) |> Seq.toArray
                if Array.isEmpty projs then
                    None
                else
                    Some (Projects projs)
        with
        | _ ->
            None


    let getRegistrations (clientCapabilities: ClientCapabilities option): Registration list =
        let registrationBuilders =
            [ CallHierarchy.registration
              CodeAction.registration
              CodeLens.registration
              Color.registration
              Completion.registration
              Declaration.registration
              Definition.registration
              Diagnostic.registration
              DocumentFormatting.registration
              DocumentHighlight.registration
              DocumentLink.registration
              DocumentOnTypeFormatting.registration
              DocumentRangeFormatting.registration
              DocumentSymbol.registration
              ExecuteCommand.registration
              FoldingRange.registration
              Hover.registration
              Implementation.registration
              InlayHint.registration
              InlineValue.registration
              LinkEditingRange.registration
              Moniker.registration
              References.registration
              Rename.registration
              SelectionRange.registration
              SemanticTokens.registration
              SignatureHelp.registration
              TextDocumentSync.registration
              TypeDefinition.registration
              TypeHierarchy.registration
              WorkspaceSymbol.registration ]
        registrationBuilders
        |> List.map ((|>) clientCapabilities)
        |> List.filter (Option.isSome)
        |> List.map (Option.get)

    let getContainingTypeOrThis (symbol: ISymbol): INamedTypeSymbol =
        if (symbol :? INamedTypeSymbol) then
            symbol :?> INamedTypeSymbol
        else
            symbol.ContainingType

    let getFullReflectionName (containingType: INamedTypeSymbol) =
        let stack = Stack<string>();
        stack.Push(containingType.MetadataName);
        let mutable ns = containingType.ContainingNamespace;

        let mutable doContinue = true
        while doContinue do
            stack.Push(ns.Name);
            ns <- ns.ContainingNamespace

            doContinue <- ns <> null && not ns.IsGlobalNamespace

        String.Join(".", stack)

    let makeDocumentFromMetadata
            (project: Microsoft.CodeAnalysis.Project)
            (metadataModule: IModuleSymbol)
            (fullName: string) = async {
        let! compilation = project.GetCompilationAsync() |> Async.AwaitTask
        let reference = compilation.GetMetadataReference(metadataModule.ContainingAssembly)
        let peReference = reference :?> PortableExecutableReference |> Option.ofObj
        let assemblyLocation = peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

        let decompilerSettings = DecompilerSettings()
        decompilerSettings.ThrowOnAssemblyResolveErrors <- false // this shouldn't be a showstopper for us

        let decompiler = CSharpDecompiler(assemblyLocation, decompilerSettings)

        // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
        // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
        decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

        let fullTypeName = FullTypeName(fullName)

        let text = decompiler.DecompileTypeAsString(fullTypeName)

        let mdDocumentFilename = $"$metadata$/projects/{project.Name}/assemblies/{metadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
        let mdDocument = project.AddDocument(mdDocumentFilename, text)
        return (mdDocument, text)
    }


// Source from https://nbevans.wordpress.com/2014/08/09/a-simple-stereotypical-javascript-like-debounce-service-for-f/
type Debounce(timeout, fn) as x =
    let mailbox = MailboxProcessor.Start(fun agent ->
        let rec loop ida idb = async {
            match! agent.TryReceive(x.Timeout) with
            | Some _ -> return! loop ida (idb + 1)
            | None when ida <> idb ->
                fn ()
                return! loop 0 0
            | None -> return! loop 0 0
        }

        loop 0 0)

    /// Calls the function, after debouncing has been applied.
    member __.Bounce() = mailbox.Post(null)

    /// Timeout in ms
    member val Timeout = timeout with get, set


type DocumentSymbolCollectorForMatchingSymbolName (documentUri, sym: ISymbol) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations = []
    let mutable suggestedLocations = []

    let collectIdentifier (identifier: SyntaxToken) exactMatch =
        let location: Location =
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

    override __.Visit(node) =
        if sym.Kind = Microsoft.CodeAnalysis.SymbolKind.Method then
            if node :? MethodDeclarationSyntax then
                let nodeMethodDecl = node :?> MethodDeclarationSyntax

                if nodeMethodDecl.Identifier.ValueText = sym.Name then
                    let methodArityMatches =
                        let symMethod = sym :?> IMethodSymbol
                        symMethod.Parameters.Length = nodeMethodDecl.ParameterList.Parameters.Count

                    collectIdentifier nodeMethodDecl.Identifier methodArityMatches
        else
            if node :? TypeDeclarationSyntax then
                let typeDecl = node :?> TypeDeclarationSyntax
                if typeDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier typeDecl.Identifier false

            else if node :? PropertyDeclarationSyntax then
                let propertyDecl = node :?> PropertyDeclarationSyntax
                if propertyDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier propertyDecl.Identifier false

            else if node :? EventDeclarationSyntax then
                let eventDecl = node :?> EventDeclarationSyntax
                if eventDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier eventDecl.Identifier false

            // TODO: collect other type of syntax nodes too

        base.Visit(node)
