namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions

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

    let provider (clientCapabilities: ClientCapabilities option) : bool option = Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let prepare (scope: ServerRequestScope) (prepareParams: CallHierarchyPrepareParams): AsyncLspResult<CallHierarchyItem[] option> = async {
        match scope.GetUserDocumentForUri prepareParams.TextDocument.Uri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                prepareParams.Position
                |> Position.toLinePosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let symbols =
                SymbolFinder.FindSymbolAtPositionAsync(doc, position)
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Option.ofObj
                |> Option.filter isCallableSymbol
                |> Option.toList
            let! locations = scope.ResolveSymbolLocations doc.Project symbols
            return Seq.allPairs symbols locations
                |> Seq.map (uncurry HierarchyItem.fromSymbolAndLocation)
                |> Seq.toArray
                |> Some
                |> success
        | _ -> return None |> success
    }

    let incomingCalls
        (scope: ServerRequestScope)
        (incomingParams: CallHierarchyIncomingCallsParams)
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

        match scope.GetUserDocumentForUri incomingParams.Item.Uri with
        | None -> return None |> success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                incomingParams.Item.Range.Start
                |> Position.toLinePosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
            let callers =
                symbol
                |> Option.ofObj
                |> Option.toList
                |> Seq.collect (fun sym -> SymbolFinder.FindCallersAsync(sym, scope.Solution) |> Async.AwaitTask |> Async.RunSynchronously)
                |> Seq.filter (fun info -> info.IsDirect && isCallableSymbol info.CallingSymbol)
            return
                callers
                |> Seq.collect toCallHierarchyIncomingCalls
                |> Seq.toArray
                |> Some
                |> success
    }

    let outgoingCalls
        (scope: ServerRequestScope)
        (outgoingParams: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> = async {
        // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol.
        // Then how can we implement it? Parsing AST manually?
        return None |> success
    }
