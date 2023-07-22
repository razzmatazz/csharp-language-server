namespace CSharpLanguageServer.Handlers

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module CallHierarchy =
    let private isCallableSymbol (symbol: ISymbol): bool =
        if isNull symbol then
            false
        else
            List.contains
                symbol.Kind
                [ Microsoft.CodeAnalysis.SymbolKind.Method
                  Microsoft.CodeAnalysis.SymbolKind.Field
                  Microsoft.CodeAnalysis.SymbolKind.Event
                  Microsoft.CodeAnalysis.SymbolKind.Property ]

    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.CallHierarchy)
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
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/prepareCallHierarchy"
                  RegisterOptions = { DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }


    let prepare (wm: IWorkspaceManager) (p: CallHierarchyPrepareParams) : AsyncLspResult<CallHierarchyItem[] option> = async {
        match! wm.FindSymbol p.TextDocument.Uri p.Position with
        | Some symbol when isCallableSymbol symbol ->
            let! itemList = HierarchyItem.fromSymbol wm symbol
            return
                itemList
                |> List.toArray
                |> Some
                |> success
        | _ -> return None |> success
    }

    let incomingCalls
        (wm: IWorkspaceManager)
        (p: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall[] option> = async {
        let toCallHierarchyIncomingCalls (info: SymbolCallerInfo) : CallHierarchyIncomingCall seq =
            let fromRanges =
                info.Locations
                |> Seq.map (fun l -> l.GetLineSpan().Span |> Range.fromLinePositionSpan)
                |> Seq.toArray
            info.CallingSymbol.Locations
            |> Seq.map (fun loc ->
                { From = HierarchyItem.fromSymbolAndLocation (info.CallingSymbol) (loc |> Location.fromRoslynLocation)
                  FromRanges = fromRanges })

        match! wm.FindSymbol p.Item.Uri p.Item.Range.Start with
        | None -> return None |> success
        | Some symbol ->
            let! callers = wm.FindCallers symbol
            // TODO: If we remove info.IsDirect, then we will get lots of false positive. But if we keep it,
            // we will miss many callers. Maybe it should have some change in LSP protocol.
            return
                callers
                |> Seq.filter (fun info -> info.IsDirect && isCallableSymbol info.CallingSymbol)
                |> Seq.collect toCallHierarchyIncomingCalls
                |> Seq.toArray
                |> Some
                |> success
    }

    let outgoingCalls
        (wm: IWorkspaceManager)
        (p: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> = async {
        // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol.
        // Then how can we implement it? Parsing AST manually?
        return None |> success
    }
