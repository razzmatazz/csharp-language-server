namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module TypeHierarchy =
    let private isTypeSymbol (symbol: ISymbol) =
        match symbol with
        | :? INamedTypeSymbol -> true
        | _ -> false

    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.TypeHierarchy
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, TypeHierarchyOptions, TypeHierarchyRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: TypeHierarchyRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/prepareTypeHierarchy"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare
        (context: RequestContext)
        (p: TypeHierarchyPrepareParams)
        : Async<LspResult<TypeHierarchyItem[] option> * RequestEffects> =
        async {
            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

            match wf with
            | None -> return None |> LspResult.success, RequestEffects.Empty
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | Some(symbol, project, _) when isTypeSymbol symbol ->
                    let! symLocations, updatedWf = workspaceFolderSymbolLocations wf context.Config symbol project

                    return
                        symLocations
                        |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation symbol)
                        |> Seq.toArray
                        |> Some
                        |> LspResult.success,
                        RequestEffects.Empty.WithWorkspaceFolderChange(updatedWf)

                | _ -> return LspResult.success None, RequestEffects.Empty
        }

    let supertypes
        (context: RequestContext)
        (p: TypeHierarchySupertypesParams)
        : Async<LspResult<TypeHierarchyItem[] option> * RequestEffects> =
        async {
            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.Item.Uri)

            match wf with
            | None -> return None |> LspResult.success, RequestEffects.Empty
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.Item.Uri p.Item.Range.Start wf

                match symInfo with
                | Some(symbol, project, _) when isTypeSymbol symbol ->
                    let typeSymbol = symbol :?> INamedTypeSymbol

                    let baseType =
                        typeSymbol.BaseType
                        |> Option.ofObj
                        |> Option.filter (fun sym -> sym.SpecialType = SpecialType.None)
                        |> Option.toList

                    let interfaces = Seq.toList typeSymbol.Interfaces
                    let supertypes = baseType @ interfaces

                    let items = System.Collections.Generic.List<TypeHierarchyItem>()
                    let mutable updatedWf = wf

                    for typeSym in supertypes do
                        let! locations, wf = workspaceFolderSymbolLocations updatedWf context.Config typeSym project

                        let typeSymItems =
                            locations |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation typeSym)

                        items.AddRange(typeSymItems)

                        updatedWf <- wf

                    return
                        items |> Seq.toArray |> Some |> LspResult.success,
                        RequestEffects.Empty.WithWorkspaceFolderChange(updatedWf)

                | _ -> return LspResult.success None, RequestEffects.Empty
        }

    let subtypes
        (context: RequestContext)
        (p: TypeHierarchySubtypesParams)
        : Async<LspResult<TypeHierarchyItem[] option> * RequestEffects> =
        async {
            let! ct = Async.CancellationToken
            let! wf, solution = p.Item.Uri |> context.GetWorkspaceFolderReadySolution

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.Item.Uri p.Item.Range.Start wf

                match symInfo with
                | Some(symbol, project, _) when isTypeSymbol symbol ->
                    let typeSymbol = symbol :?> INamedTypeSymbol
                    // We only want immediately derived classes/interfaces/implementations here (we only need
                    // subclasses not subclasses' subclasses)
                    let findDerivedClasses'
                        (symbol: INamedTypeSymbol)
                        (transitive: bool)
                        : Async<INamedTypeSymbol seq> =
                        SymbolFinder.FindDerivedClassesAsync(symbol, solution, transitive, cancellationToken = ct)
                        |> Async.AwaitTask

                    let findDerivedInterfaces'
                        (symbol: INamedTypeSymbol)
                        (transitive: bool)
                        : Async<INamedTypeSymbol seq> =
                        SymbolFinder.FindDerivedInterfacesAsync(symbol, solution, transitive, cancellationToken = ct)
                        |> Async.AwaitTask

                    let findImplementations'
                        (symbol: INamedTypeSymbol)
                        (transitive: bool)
                        : Async<INamedTypeSymbol seq> =
                        SymbolFinder.FindImplementationsAsync(symbol, solution, transitive, cancellationToken = ct)
                        |> Async.AwaitTask

                    let! subtypes =
                        [ findDerivedClasses' typeSymbol false
                          findDerivedInterfaces' typeSymbol false
                          findImplementations' typeSymbol false ]
                        |> Async.Parallel
                        |> Async.map (Seq.collect id >> Seq.toList)

                    let items = System.Collections.Generic.List<TypeHierarchyItem>()
                    let mutable updatedWf = wf

                    for typeSym in subtypes do
                        let! locations, wf = workspaceFolderSymbolLocations updatedWf context.Config typeSym project

                        let typeSymItems =
                            locations |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation typeSym)

                        items.AddRange(typeSymItems)

                        updatedWf <- wf

                    return
                        items |> Seq.toArray |> Some |> LspResult.success,
                        RequestEffects.Empty.WithWorkspaceFolderChange(updatedWf)

                | _ -> return None |> LspResult.success, RequestEffects.Empty

            | _, _ -> return None |> LspResult.success, RequestEffects.Empty
        }
