namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module CallHierarchy =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let prepare (scope: ServerRequestScope) (prepareParams: CallHierarchyPrepareParams): AsyncLspResult<CallHierarchyItem [] option> = async {
        match scope.GetUserDocumentForUri prepareParams.TextDocument.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                prepareParams.Position
                |> roslynLinePositionForLspPosition sourceText.Lines
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
                |> Seq.map (uncurry toHierarchyItem)
                |> Seq.toArray
                |> Some
                |> LspResult.success
    }

    let handleIncomingCalls (scope: ServerRequestScope) (incomingParams: CallHierarchyIncomingCallsParams): AsyncLspResult<CallHierarchyIncomingCall [] option> = async {
        let toCallHierarchyIncomingCalls (info: SymbolCallerInfo): CallHierarchyIncomingCall seq =
            let fromRanges = info.Locations |> Seq.map (fun l -> l.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan) |> Seq.toArray
            info.CallingSymbol.Locations
            |> Seq.map (fun loc ->
                { From = toHierarchyItem (info.CallingSymbol) (loc |> lspLocationForRoslynLocation)
                  FromRanges = fromRanges })

        match scope.GetUserDocumentForUri incomingParams.Item.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position =
                incomingParams.Item.Range.Start
                |> roslynLinePositionForLspPosition sourceText.Lines
                |> sourceText.Lines.GetPosition
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
            let callers =
                symbol
                |> Option.ofObj
                |> Option.toList
                |> Seq.collect (fun sym -> SymbolFinder.FindCallersAsync(sym, scope.Solution) |> Async.AwaitTask |> Async.RunSynchronously)
                |> Seq.filter (fun info -> info.IsDirect && isCallableSymbol info.CallingSymbol)
            return callers
                |> Seq.collect toCallHierarchyIncomingCalls
                |> Seq.toArray
                |> Some
                |> LspResult.success
    }

    let handleOutgoingCalls (scope: ServerRequestScope) (outgoingParams: CallHierarchyOutgoingCallsParams): AsyncLspResult<CallHierarchyOutgoingCall [] option> = async {
        // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol. Then how can we implement it? Parsing AST manually?
        return None |> LspResult.success
    }
