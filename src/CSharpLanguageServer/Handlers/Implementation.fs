namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Implementation =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Implementation
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: ImplementationRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/implementation"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let findImplLocationsOfSymbol wf config project (solution: Solution) (sym: ISymbol) = async {
        let! ct = Async.CancellationToken

        let baseImpls =
            SymbolFinder.FindImplementationsAsync(sym, solution, cancellationToken = ct)
            |> Async.AwaitTask

        // FindImplementationsAsync on a class symbol returns only interface
        // implementations, not derived classes. For class symbols also enumerate
        // transitively-derived classes via FindDerivedClassesAsync so
        // textDocument/implementation behaves correctly on class declarations.
        let derivedClasses = async {
            match sym with
            | :? INamedTypeSymbol as nts when nts.TypeKind = TypeKind.Class ->
                let! derivedClasses =
                    SymbolFinder.FindDerivedClassesAsync(nts, solution, true, cancellationToken = ct)
                    |> Async.AwaitTask

                return derivedClasses |> Seq.cast<ISymbol>
            | _ -> return Seq.empty
        }

        // FindImplementationsAsync on a virtual/abstract/override member returns only
        // interface implementations, not overrides; those come from FindOverridesAsync.
        let overrides = async {
            if sym.IsVirtual || sym.IsAbstract || sym.IsOverride then
                return!
                    SymbolFinder.FindOverridesAsync(sym, solution, cancellationToken = ct)
                    |> Async.AwaitTask
            else
                return Seq.empty
        }

        // The lookups are independent and Solution is immutable, so run them concurrently.
        let! results = Async.Parallel [ baseImpls; derivedClasses; overrides ]

        let impls = results |> Seq.concat |> Seq.distinct |> List.ofSeq

        // If Roslyn found no implementations/overrides/derived types for a metadata
        // symbol, fall back to the symbol's own declaration locations so that
        // useMetadataUris decompilation is triggered just like textDocument/definition.
        // Symbols declared in source get an empty result instead, so clients can tell
        // "no implementations" apart from "here is where you already are".
        let symbolsToResolve =
            match impls with
            | [] when sym.Locations |> Seq.exists _.IsInSource -> []
            | [] -> [ sym ]
            | _ -> impls

        let! symbolLocations, aggregatedWfUpdates =
            wf |> workspaceFolderSymbolsLocations config project symbolsToResolve

        return symbolLocations |> List.collect snd |> List.toArray, aggregatedWfUpdates
    }

    let handle
        (context: RequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option> * LspWorkspaceUpdate> =
        async {
            let! wf, solution = context.LoadWorkspaceFolder(p.TextDocument.Uri)

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | None -> return LspResult.success None, LspWorkspaceUpdate.Empty
                | Some(sym, project, _) ->
                    let! impls, wfUpdates = findImplLocationsOfSymbol wf context.Config project solution sym

                    let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)
                    let lspResult = impls |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success
                    return lspResult, wsUpdate

            | _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
