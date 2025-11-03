namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions

[<RequireQualifiedAccess>]
module CallHierarchy =
    let private isCallableSymbol (symbol: ISymbol) : bool =
        List.contains
            symbol.Kind
            [ Microsoft.CodeAnalysis.SymbolKind.Method
              Microsoft.CodeAnalysis.SymbolKind.Field
              Microsoft.CodeAnalysis.SymbolKind.Event
              Microsoft.CodeAnalysis.SymbolKind.Property ]

    let provider
        (clientCapabilities: ClientCapabilities)
        : U3<bool, CallHierarchyOptions, CallHierarchyRegistrationOptions> option =
        Some(U3.C1 true)

    let prepare
        (context: ServerRequestContext)
        (p: CallHierarchyPrepareParams)
        : AsyncLspResult<CallHierarchyItem[] option> =
        async {
            match! context.FindSymbol p.TextDocument.Uri p.Position with
            | Some(wf), Some(symbol) when isCallableSymbol symbol ->
                let! itemList = CallHierarchyItem.fromSymbol context.ResolveSymbolLocations symbol
                return itemList |> List.toArray |> Some |> LspResult.success
            | _ -> return None |> LspResult.success
        }

    let incomingCalls
        (context: ServerRequestContext)
        (p: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall[] option> =
        async {
            let! ct = Async.CancellationToken

            let toCallHierarchyIncomingCalls (info: SymbolCallerInfo) : CallHierarchyIncomingCall seq =
                let fromRanges =
                    info.Locations
                    |> Seq.map (fun l -> l.GetLineSpan().Span |> Range.fromLinePositionSpan)
                    |> Seq.toArray

                info.CallingSymbol.Locations
                |> Seq.choose Location.fromRoslynLocation
                |> Seq.map (fun loc ->
                    { From = CallHierarchyItem.fromSymbolAndLocation (info.CallingSymbol) loc
                      FromRanges = fromRanges })

            match! context.FindSymbol p.Item.Uri p.Item.Range.Start with
            | Some(wf), Some(symbol) ->
                let! callers =
                    SymbolFinder.FindCallersAsync(symbol, wf.Solution.Value, cancellationToken = ct)
                    |> Async.AwaitTask

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

            | _, _ -> return None |> LspResult.success
        }

    let outgoingCalls
        (_context: ServerRequestContext)
        (_: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> =
        async {
            // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol.
            // Then how can we implement it? Parsing AST manually?
            return None |> LspResult.success
        }
