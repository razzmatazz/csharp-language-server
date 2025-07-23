namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module CallHierarchy =
    let private isCallableSymbol (symbol: ISymbol): bool =
        List.contains
            symbol.Kind
            [ Microsoft.CodeAnalysis.SymbolKind.Method
              Microsoft.CodeAnalysis.SymbolKind.Field
              Microsoft.CodeAnalysis.SymbolKind.Event
              Microsoft.CodeAnalysis.SymbolKind.Property ]

    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.CallHierarchy)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U3<bool, CallHierarchyOptions, CallHierarchyRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U3.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: CallHierarchyRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  Id = None
                  WorkDoneProgress = None
                }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/prepareCallHierarchy"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare (context: ServerRequestContext) (p: CallHierarchyPrepareParams) : AsyncLspResult<CallHierarchyItem[] option> = async {
        match! context.FindSymbol p.TextDocument.Uri p.Position with
        | Some symbol when isCallableSymbol symbol ->
            let! itemList = CallHierarchyItem.fromSymbol context.ResolveSymbolLocations symbol
            return
                itemList
                |> List.toArray
                |> Some
                |> LspResult.success
        | _ -> return None |> LspResult.success
    }

    let incomingCalls
        (context: ServerRequestContext)
        (p: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall[] option> = async {
        let toCallHierarchyIncomingCalls (info: SymbolCallerInfo) : CallHierarchyIncomingCall seq =
            let fromRanges =
                info.Locations
                |> Seq.map (fun l -> l.GetLineSpan().Span |> Range.fromLinePositionSpan)
                |> Seq.toArray

            info.CallingSymbol.Locations
            |> Seq.map Location.fromRoslynLocation
            |> Seq.filter _.IsSome
            |> Seq.map _.Value
            |> Seq.map (fun loc ->
                { From = CallHierarchyItem.fromSymbolAndLocation (info.CallingSymbol) loc
                  FromRanges = fromRanges })

        match! context.FindSymbol p.Item.Uri p.Item.Range.Start with
        | None -> return None |> LspResult.success
        | Some symbol ->
            let! callers = context.FindCallers symbol
            // TODO: If we remove info.IsDirect, then we will get lots of false positive. But if we keep it,
            // we will miss many callers. Maybe it should have some change in LSP protocol.
            return
                callers
                |> Seq.filter (fun info -> info.IsDirect && isCallableSymbol info.CallingSymbol)
                |> Seq.collect toCallHierarchyIncomingCalls
                |> Seq.distinct
                |> Seq.toArray
                |> Some
                |> LspResult.success
    }

    let outgoingCalls
        (_context: ServerRequestContext)
        (_: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> = async {
        // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol.
        // Then how can we implement it? Parsing AST manually?
        return None |> LspResult.success
    }
