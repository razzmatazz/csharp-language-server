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

    let findImplLocationsOfSymbol wf config project (sym: ISymbol) = async {
        let! ct = Async.CancellationToken

        match wf.Solution with
        | Loaded(_, solution) ->
            let! baseImpls =
                SymbolFinder.FindImplementationsAsync(sym, solution, cancellationToken = ct)
                |> Async.AwaitTask

            // FindImplementationsAsync on a class symbol returns only interface
            // implementations, not derived classes. For class symbols also enumerate
            // transitively-derived classes via FindDerivedClassesAsync so
            // textDocument/implementation behaves correctly on class declarations.
            let! derivedClasses = async {
                match sym with
                | :? INamedTypeSymbol as nts when nts.TypeKind = TypeKind.Class ->
                    let! derivedClasses =
                        SymbolFinder.FindDerivedClassesAsync(nts, solution, true, cancellationToken = ct)
                        |> Async.AwaitTask

                    return derivedClasses |> List.ofSeq
                | _ -> return []
            }

            let impls = Seq.append baseImpls (derivedClasses |> Seq.cast<ISymbol>)

            let mutable aggregatedWf = wf
            let mutable aggregatedWfUpdates = []

            let locations = System.Collections.Generic.List<Location>()

            for i in impls do
                let! implLocations, wfUpdates = aggregatedWf |> workspaceFolderSymbolLocations config i project

                aggregatedWf <- wfUpdates |> List.fold (|>) aggregatedWf
                aggregatedWfUpdates <- aggregatedWfUpdates @ wfUpdates

                locations.AddRange(implLocations)

            return locations |> Seq.toArray, aggregatedWfUpdates

        | _ -> return [||], []
    }

    let handle
        (context: RequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option> * LspWorkspaceUpdate> =
        async {
            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

            match wf with
            | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | None -> return LspResult.success None, LspWorkspaceUpdate.Empty
                | Some(sym, project, _) ->
                    let! impls, wfUpdates = findImplLocationsOfSymbol wf context.Config project sym

                    let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)
                    let lspResult = impls |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success
                    return lspResult, wsUpdate


        }
