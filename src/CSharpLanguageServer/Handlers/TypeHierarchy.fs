namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
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

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: TypeHierarchyRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/prepareTypeHierarchy"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare
        (context: ServerRequestContext)
        (p: TypeHierarchyPrepareParams)
        : AsyncLspResult<TypeHierarchyItem[] option> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, _) when isTypeSymbol symbol ->
                let! symLocations, updatedWf = workspaceFolderSymbolLocations wf context.State.Settings symbol project

                context.Emit(WorkspaceFolderChange updatedWf)

                return
                    symLocations
                    |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation symbol)
                    |> Seq.toArray
                    |> Some
                    |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }

    let supertypes
        (context: ServerRequestContext)
        (p: TypeHierarchySupertypesParams)
        : AsyncLspResult<TypeHierarchyItem[] option> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.Item.Uri p.Item.Range.Start with
            | Some wf, Some(symbol, project, _) when isTypeSymbol symbol ->
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
                    let! locations, wf = workspaceFolderSymbolLocations updatedWf context.State.Settings typeSym project

                    let typeSymItems =
                        locations |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation typeSym)

                    items.AddRange(typeSymItems)

                    updatedWf <- wf

                context.Emit(WorkspaceFolderChange updatedWf)

                return items |> Seq.toArray |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }

    let subtypes
        (context: ServerRequestContext)
        (p: TypeHierarchySubtypesParams)
        : AsyncLspResult<TypeHierarchyItem[] option> =
        async {
            let! ct = Async.CancellationToken

            match! workspaceDocumentSymbol context.Workspace AnyDocument p.Item.Uri p.Item.Range.Start with
            | Some wf, Some(symbol, project, _) when isTypeSymbol symbol ->
                let typeSymbol = symbol :?> INamedTypeSymbol
                // We only want immediately derived classes/interfaces/implementations here (we only need
                // subclasses not subclasses' subclasses)
                let findDerivedClasses' (symbol: INamedTypeSymbol) (transitive: bool) : Async<INamedTypeSymbol seq> =
                    SymbolFinder.FindDerivedClassesAsync(symbol, wf.Solution.Value, transitive, cancellationToken = ct)
                    |> Async.AwaitTask

                let findDerivedInterfaces' (symbol: INamedTypeSymbol) (transitive: bool) : Async<INamedTypeSymbol seq> =
                    SymbolFinder.FindDerivedInterfacesAsync(
                        symbol,
                        wf.Solution.Value,
                        transitive,
                        cancellationToken = ct
                    )
                    |> Async.AwaitTask

                let findImplementations' (symbol: INamedTypeSymbol) (transitive: bool) : Async<INamedTypeSymbol seq> =
                    SymbolFinder.FindImplementationsAsync(symbol, wf.Solution.Value, transitive, cancellationToken = ct)
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
                    let! locations, wf = workspaceFolderSymbolLocations updatedWf context.State.Settings typeSym project

                    let typeSymItems =
                        locations |> Seq.map (TypeHierarchyItem.fromSymbolAndLocation typeSym)

                    items.AddRange(typeSymItems)

                    updatedWf <- wf

                context.Emit(WorkspaceFolderChange updatedWf)

                return items |> Seq.toArray |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
