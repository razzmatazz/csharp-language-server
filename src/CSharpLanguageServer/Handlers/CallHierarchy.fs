namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module CallHierarchy =
    let private isCallableSymbol (symbol: ISymbol) : bool =
        List.contains
            symbol.Kind
            [ Microsoft.CodeAnalysis.SymbolKind.Method
              Microsoft.CodeAnalysis.SymbolKind.Field
              Microsoft.CodeAnalysis.SymbolKind.Event
              Microsoft.CodeAnalysis.SymbolKind.Property ]

    let provider (_cc: ClientCapabilities) : U3<bool, CallHierarchyOptions, CallHierarchyRegistrationOptions> option =
        Some(U3.C1 true)

    let prepare
        (context: ServerRequestContext)
        (p: CallHierarchyPrepareParams)
        : AsyncLspResult<CallHierarchyItem[] option> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, _) when isCallableSymbol symbol ->
                let! locations, updatedWf = workspaceFolderSymbolLocations wf context.State.Settings symbol project

                context.Emit(WorkspaceFolderChange updatedWf)

                return
                    locations
                    |> Seq.map (CallHierarchyItem.fromSymbolAndLocation symbol)
                    |> Seq.toArray
                    |> Some
                    |> LspResult.success

            | _ -> return None |> LspResult.success
        }

    let incomingCalls
        (context: ServerRequestContext)
        (p: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall[] option> =
        async {
            let! ct = Async.CancellationToken

            let toCallHierarchyIncomingCalls
                (pathToUri: string -> string)
                (info: SymbolCallerInfo)
                : CallHierarchyIncomingCall seq =
                let fromRanges =
                    info.Locations
                    |> Seq.map (fun l -> l.GetLineSpan().Span |> Range.fromLinePositionSpan)
                    |> Seq.toArray

                info.CallingSymbol.Locations
                |> Seq.choose (Location.fromRoslynLocation pathToUri)
                |> Seq.map (fun loc ->
                    { From = CallHierarchyItem.fromSymbolAndLocation info.CallingSymbol loc
                      FromRanges = fromRanges })

            match! workspaceDocumentSymbol context.Workspace AnyDocument p.Item.Uri p.Item.Range.Start with
            | Some wf, Some(symbol, _, _) ->
                let! callers =
                    SymbolFinder.FindCallersAsync(symbol, wf.Solution.Value, cancellationToken = ct)
                    |> Async.AwaitTask

                let wfPathToUri = workspaceFolderPathToUri wf

                // TODO: If we remove info.IsDirect, then we will get lots of false positive. But if we keep it,
                // we will miss many callers. Maybe it should have some change in LSP protocol.
                return
                    callers
                    |> Seq.filter (fun info -> info.IsDirect && isCallableSymbol info.CallingSymbol)
                    |> Seq.collect (toCallHierarchyIncomingCalls wfPathToUri)
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
