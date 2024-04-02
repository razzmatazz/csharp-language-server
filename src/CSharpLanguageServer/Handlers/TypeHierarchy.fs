namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open FSharpPlus

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module TypeHierarchy =
    let private isTypeSymbol (symbol: ISymbol) =
        match symbol with
        | :? INamedTypeSymbol -> true
        | _ -> false

    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.TypeHierarchy)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: TypeHierarchyRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/prepareTypeHierarchy"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare (context: ServerRequestContext) (p: TypeHierarchyPrepareParams) : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! context.FindSymbol p.TextDocument.Uri p.Position with
        | Some symbol when isTypeSymbol symbol ->
            let! itemList = HierarchyItem.fromSymbol context.ResolveSymbolLocations symbol
            return itemList |> List.toArray |> Some |> success
        | _ -> return None |> success
    }

    let supertypes
            (context: ServerRequestContext)
            (p: TypeHierarchySupertypesParams)
            : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! context.FindSymbol p.Item.Uri p.Item.Range.Start with
        | Some symbol when isTypeSymbol symbol ->
            let typeSymbol = symbol :?> INamedTypeSymbol
            let baseType =
                typeSymbol.BaseType
                |> Option.ofObj
                |> Option.filter (fun sym -> sym.SpecialType = SpecialType.None)
                |> Option.toList
            let interfaces = Seq.toList typeSymbol.Interfaces
            let supertypes = baseType @ interfaces
            let! items = supertypes |> Seq.map (HierarchyItem.fromSymbol context.ResolveSymbolLocations) |> Async.Parallel
            return items |> Seq.collect id |> Seq.toArray |> Some |> success
        | _ -> return None |> success
    }

    let subtypes (context: ServerRequestContext) (p: TypeHierarchySubtypesParams) : AsyncLspResult<TypeHierarchyItem[] option> = async {
        match! context.FindSymbol p.Item.Uri p.Item.Range.Start with
        | Some symbol when isTypeSymbol symbol ->
            let typeSymbol = symbol :?> INamedTypeSymbol
            // We only want immediately derived classes/interfaces/implementations here (we only need
            // subclasses not subclasses' subclasses)
            let! subtypes =
                [ context.FindDerivedClasses' typeSymbol false
                  context.FindDerivedInterfaces' typeSymbol false
                  context.FindImplementations' typeSymbol false ]
                |> Async.Parallel
                |> map (Seq.collect id >> Seq.toList)
            let! items = subtypes |> Seq.map (HierarchyItem.fromSymbol context.ResolveSymbolLocations) |> Async.Parallel
            return items |> Seq.collect id |> Seq.toArray |> Some |> success
        | _ -> return None |> success
    }
