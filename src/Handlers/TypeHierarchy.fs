namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open FSharpPlus

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module TypeHierarchy =
    let private isTypeSymbol (symbol: ISymbol) =
        match symbol with
        | :? INamedTypeSymbol -> true
        | _ -> false

    let provider: bool option = Some true

    let prepare (wm: IWorkspaceManager) (p: TypeHierarchyPrepareParams) : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! wm.FindSymbol p.TextDocument.Uri p.Position with
        | Some symbol when isTypeSymbol symbol ->
            let! itemList = HierarchyItem.fromSymbol wm symbol
            return itemList |> List.toArray |> Some |> success
        | _ -> return None |> success
    }

    let supertypes
        (wm: IWorkspaceManager)
        (p: TypeHierarchySupertypesParams)
        : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! wm.FindSymbol p.Item.Uri p.Item.Range.Start with
        | Some symbol when isTypeSymbol symbol ->
            let typeSymbol = symbol :?> INamedTypeSymbol
            let baseType =
                typeSymbol.BaseType
                |> Option.ofObj
                |> Option.filter (fun sym -> sym.SpecialType = SpecialType.None)
                |> Option.toList
            let interfaces = Seq.toList typeSymbol.Interfaces
            let supertypes = baseType @ interfaces
            let! items = supertypes |> Seq.map (HierarchyItem.fromSymbol wm) |> Async.Parallel
            return items |> Seq.collect id |> Seq.toArray |> Some |> success
        | _ -> return None |> success
    }

    let subtypes (wm: IWorkspaceManager) (p: TypeHierarchySubtypesParams) : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! wm.FindSymbol p.Item.Uri p.Item.Range.Start with
        | Some symbol when isTypeSymbol symbol ->
            let typeSymbol = symbol :?> INamedTypeSymbol
            // We only want immediately derived classes/interfaces/implementations here (we only need
            // subclasses not subclasses' subclasses)
            let! subtypes =
                [ wm.FindDerivedClasses' typeSymbol false
                  wm.FindDerivedInterfaces' typeSymbol false
                  wm.FindImplementations' typeSymbol false ]
                |> Async.Parallel
                |> map (Seq.collect id >> Seq.toList)
            let! items = subtypes |> Seq.map (HierarchyItem.fromSymbol wm) |> Async.Parallel
            return items |> Seq.collect id |> Seq.toArray |> Some |> success
        | _ -> return None |> success
    }
