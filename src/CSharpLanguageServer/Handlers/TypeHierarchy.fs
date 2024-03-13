namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module TypeHierarchy =
    let provider (clientCapabilities: ClientCapabilities option) : bool option = Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let prepare (scope: ServerRequestScope) (prepareParams: TypeHierarchyPrepareParams): AsyncLspResult<TypeHierarchyItem [] option> = async {
        match scope.GetUserDocumentForUri prepareParams.TextDocument.Uri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                prepareParams.Position
                |> Position.toLinePosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let symbol =
                SymbolFinder.FindSymbolAtPositionAsync(doc, position)
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Option.ofObj
                |> Option.filter (fun sym -> sym :? INamedTypeSymbol)
                |> Option.toList
            let! locations = scope.ResolveSymbolLocations doc.Project symbol
            let itemList =
                Seq.allPairs symbol locations
                |> Seq.map (uncurry HierarchyItem.fromSymbolAndLocation)
                |> Seq.toList
            return itemList |> List.toArray |> Some |> success
        | _ -> return None |> success
    }

    let supertypes (scope: ServerRequestScope) (p: TypeHierarchySupertypesParams): AsyncLspResult<TypeHierarchyItem [] option> = async {
        match scope.GetUserDocumentForUri p.Item.Uri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                p.Item.Range.Start
                |> Position.toLinePosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let symbol =
                SymbolFinder.FindSymbolAtPositionAsync(doc, position)
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Option.ofObj
                |> Option.bind (fun sym ->
                    match sym with
                    | :? INamedTypeSymbol as namedType -> Some namedType
                    | _ -> None)
            let baseType =
                symbol
                |> Option.bind (fun sym -> Option.ofObj sym.BaseType)
                |> Option.filter (fun sym -> sym.SpecialType = SpecialType.None)
                |> Option.toList
            let interfaces =
                symbol
                |> Option.toList
                |> List.collect (fun sym -> Seq.toList sym.Interfaces)
            let supertypes = baseType @ interfaces
            return
                supertypes
                |> Seq.map (fun sym -> scope.ResolveSymbolLocations doc.Project [sym])
                |> Seq.map Async.RunSynchronously
                |> Seq.zip supertypes
                |> Seq.collect (fun (sym, locs) -> Seq.map (fun loc -> (sym, loc)) locs)
                |> Seq.map (uncurry HierarchyItem.fromSymbolAndLocation)
                |> Seq.toArray
                |> Some
                |> success
        | _ -> return None |> success
    }

    let subtypes (scope: ServerRequestScope) (p: TypeHierarchySubtypesParams): AsyncLspResult<TypeHierarchyItem [] option> = async {
        match scope.GetUserDocumentForUri p.Item.Uri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                p.Item.Range.Start
                |> Position.toLinePosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let symbol =
                SymbolFinder.FindSymbolAtPositionAsync(doc, position)
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Option.ofObj
                |> Option.bind (fun sym ->
                    match sym with
                    | :? INamedTypeSymbol as namedType -> Some namedType
                    | _ -> None)
                |> Option.toList
            let derivedClasses =
                symbol
                |> Seq.collect (fun sym -> SymbolFinder.FindDerivedClassesAsync(sym, scope.Solution, false) |> Async.AwaitTask |> Async.RunSynchronously)
                |> Seq.toList
            let derivedInterfaces =
                symbol
                |> Seq.collect (fun sym -> SymbolFinder.FindDerivedInterfacesAsync(sym, scope.Solution, false) |> Async.AwaitTask |> Async.RunSynchronously)
                |> Seq.toList
            let implementations =
                symbol
                |> Seq.collect (fun sym -> SymbolFinder.FindImplementationsAsync(sym, scope.Solution, false) |> Async.AwaitTask |> Async.RunSynchronously)
                |> Seq.toList
            let subtypes = derivedClasses @ derivedInterfaces @ implementations
            return
                subtypes
                |> Seq.map (fun sym -> scope.ResolveSymbolLocations doc.Project [sym])
                |> Seq.map Async.RunSynchronously
                |> Seq.zip subtypes
                |> Seq.collect (fun (sym, locs) -> Seq.map (fun loc -> (sym, loc)) locs)
                |> Seq.map (uncurry HierarchyItem.fromSymbolAndLocation)
                |> Seq.toArray
                |> Some
                |> success
        | _ -> return None |> success
    }
