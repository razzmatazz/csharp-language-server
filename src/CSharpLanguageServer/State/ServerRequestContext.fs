namespace CSharpLanguageServer.State

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Types
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Util

type ServerRequestContext (requestId: int, state: ServerState, emitServerEvent) =
    let mutable solutionMaybe = state.Solution

    member _.RequestId = requestId
    member _.State = state
    member _.ClientCapabilities = state.ClientCapabilities
    member _.Solution = solutionMaybe.Value
    member _.OpenDocs = state.OpenDocs
    member _.DecompiledMetadata = state.DecompiledMetadata

    member _.WindowShowMessage (m: string) =
        match state.LspClient with
        | Some lspClient -> lspClient.WindowShowMessage(
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m })
        | None -> async.Return ()

    member this.GetDocumentForUriOfType = getDocumentForUriOfType this.State

    member this.GetUserDocument (u: string) =
        this.GetDocumentForUriOfType UserDocument u |> Option.map fst

    member this.GetDocument (u: string) =
        this.GetDocumentForUriOfType AnyDocument u |> Option.map fst

    member _.Emit ev =
        match ev with
        | SolutionChange newSolution ->
            solutionMaybe <- Some newSolution
        | _ -> ()

        emitServerEvent ev

    member this.EmitMany es =
        for e in es do this.Emit e

    member private this.ResolveSymbolLocation
            (project: Microsoft.CodeAnalysis.Project option)
            sym
            (l: Microsoft.CodeAnalysis.Location) = async {
        match l.IsInMetadata, l.IsInSource, project with
        | true, _, Some project ->
            let! ct = Async.CancellationToken
            let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

            let fullName = sym |> getContainingTypeOrThis |> getFullReflectionName

            let containingAssemblyName =
                l.MetadataModule
                |> nonNull "l.MetadataModule"
                |> _.ContainingAssembly.Name

            let uri = $"csharp:/metadata/projects/{project.Name}/assemblies/{containingAssemblyName}/symbols/{fullName}.cs"

            let mdDocument, stateChanges =
                match Map.tryFind uri state.DecompiledMetadata with
                | Some value ->
                    (value.Document, [])
                | None ->
                    let (documentFromMd, text) = makeDocumentFromMetadata compilation project l fullName

                    let csharpMetadata = { ProjectName = project.Name
                                           AssemblyName = containingAssemblyName
                                           SymbolName = fullName
                                           Source = text }
                    (documentFromMd, [
                        DecompiledMetadataAdd (uri, { Metadata = csharpMetadata; Document = documentFromMd })])

            this.EmitMany stateChanges

            // figure out location on the document (approx implementation)
            let! syntaxTree = mdDocument.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym)
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask
            collector.Visit(root)

            let fallbackLocationInMetadata = {
                Uri = uri
                Range = { Start = { Line = 0u; Character = 0u }; End = { Line = 0u; Character = 1u } } }

            return
                match collector.GetLocations() with
                | [] -> [fallbackLocationInMetadata]
                | ls -> ls

        | false, true, _ ->
            return
                match (Location.fromRoslynLocation l) with
                | Some loc -> [loc]
                | None -> []

        | _, _, _ ->
            return []
    }

    member this.ResolveSymbolLocations
            (symbol: Microsoft.CodeAnalysis.ISymbol)
            (project: Microsoft.CodeAnalysis.Project option) = async {
        let mutable aggregatedLspLocations = []
        for l in symbol.Locations do
            let! symLspLocations = this.ResolveSymbolLocation project symbol l

            aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations

        return aggregatedLspLocations
    }

    member this.FindSymbol' (uri: DocumentUri) (pos: Position): Async<(ISymbol * Document) option> = async {
        match this.GetDocument uri with
        | None -> return None
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = Position.toRoslynPosition sourceText.Lines pos
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
            return symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc)
     }

    member this.FindSymbol (uri: DocumentUri) (pos: Position): Async<ISymbol option> =
        this.FindSymbol' uri pos |> Async.map (Option.map fst)

    member private __._FindDerivedClasses (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> = async {
        match state.Solution with
        | None -> return []
        | Some currentSolution ->
            let! ct = Async.CancellationToken
            return!
                SymbolFinder.FindDerivedClassesAsync(symbol, currentSolution, transitive, cancellationToken=ct)
                |> Async.AwaitTask
    }

    member private __._FindDerivedInterfaces (symbol: INamedTypeSymbol) (transitive: bool):  Async<INamedTypeSymbol seq> = async {
        match state.Solution with
        | None -> return []
        | Some currentSolution ->
            let! ct = Async.CancellationToken
            return!
                SymbolFinder.FindDerivedInterfacesAsync(symbol, currentSolution, transitive, cancellationToken=ct)
                |> Async.AwaitTask
    }

    member __.FindImplementations (symbol: ISymbol): Async<ISymbol seq> = async {
        match state.Solution with
        | None -> return []
        | Some currentSolution ->
            let! ct = Async.CancellationToken
            return!
                SymbolFinder.FindImplementationsAsync(symbol, currentSolution, cancellationToken=ct)
                |> Async.AwaitTask
    }

    member __.FindImplementations' (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> = async {
        match state.Solution with
        | None -> return []
        | Some currentSolution ->
            let! ct = Async.CancellationToken
            return!
                SymbolFinder.FindImplementationsAsync(symbol, currentSolution, transitive, cancellationToken=ct)
                |> Async.AwaitTask
    }

    member this.FindDerivedClasses (symbol: INamedTypeSymbol): Async<INamedTypeSymbol seq> = this._FindDerivedClasses symbol true
    member this.FindDerivedClasses' (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> = this._FindDerivedClasses symbol transitive

    member this.FindDerivedInterfaces (symbol: INamedTypeSymbol):  Async<INamedTypeSymbol seq> = this._FindDerivedInterfaces symbol true
    member this.FindDerivedInterfaces' (symbol: INamedTypeSymbol) (transitive: bool):  Async<INamedTypeSymbol seq> = this._FindDerivedInterfaces symbol transitive

    member __.FindCallers (symbol: ISymbol): Async<SymbolCallerInfo seq> = async {
        match state.Solution with
        | None -> return []
        | Some currentSolution ->
            let! ct = Async.CancellationToken
            return!
                SymbolFinder.FindCallersAsync(symbol, currentSolution, cancellationToken=ct)
                |> Async.AwaitTask
    }

    member this.ResolveTypeSymbolLocations
            (project: Microsoft.CodeAnalysis.Project)
            (symbols: Microsoft.CodeAnalysis.ITypeSymbol list) = async {
        let mutable aggregatedLspLocations = []

        for sym in symbols do
            for l in sym.Locations do
                let! symLspLocations = this.ResolveSymbolLocation (Some project) sym l

                aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations

        return aggregatedLspLocations
    }

    member this.FindSymbols (pattern: string option): Async<Microsoft.CodeAnalysis.ISymbol seq> = async {
        let findTask ct =
                match pattern with
                | Some pat ->
                    fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsWithPatternAsync(sln, pat, SymbolFilter.TypeAndMember, cancellationToken=ct)
                | None ->
                    let true' = System.Func<string, bool>(fun _ -> true)
                    fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsAsync(sln, true', SymbolFilter.TypeAndMember, cancellationToken=ct)

        match this.State.Solution with
        | None -> return []
        | Some solution ->
            let! ct = Async.CancellationToken
            return! findTask ct solution |> Async.AwaitTask
    }

    member this.FindReferences (symbol: ISymbol) (withDefinition: bool): Async<Microsoft.CodeAnalysis.Location seq> = async {
        match this.State.Solution with
        | None -> return []
        | Some solution ->
            let! ct = Async.CancellationToken

            let locationsFromReferencedSym (r: ReferencedSymbol) =
                let locations = r.Locations |> Seq.map (fun rl -> rl.Location)

                match withDefinition with
                | true -> locations |> Seq.append r.Definition.Locations
                | false -> locations

            let! refs =
                SymbolFinder.FindReferencesAsync(symbol, solution, cancellationToken=ct)
                |> Async.AwaitTask

            return refs |> Seq.collect locationsFromReferencedSym
    }

    member this.GetDocumentVersion (uri: DocumentUri): int option =
        Uri.unescape uri
        |> this.OpenDocs.TryFind
        |> Option.map _.Version
