namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module CallHierarchy =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.CallHierarchy
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, CallHierarchyOptions, CallHierarchyRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: CallHierarchyRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/prepareCallHierarchy"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private isCallableSymbol (symbol: ISymbol) : bool =
        List.contains
            symbol.Kind
            [ Microsoft.CodeAnalysis.SymbolKind.Method
              Microsoft.CodeAnalysis.SymbolKind.Field
              Microsoft.CodeAnalysis.SymbolKind.Event
              Microsoft.CodeAnalysis.SymbolKind.Property ]

    let prepare (context: RequestContext) (p: CallHierarchyPrepareParams) : AsyncLspResult<CallHierarchyItem[] option> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        match wf with
        | None -> return None |> LspResult.success
        | Some wf ->
            let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

            match symInfo with
            | Some(symbol, project, _) when isCallableSymbol symbol ->
                let! locations, updatedWf = workspaceFolderSymbolLocations wf context.Config symbol project

                context.UpdateEffects(_.WithWorkspaceFolderChange(updatedWf))

                return
                    locations
                    |> Seq.map (CallHierarchyItem.fromSymbolAndLocation symbol)
                    |> Seq.toArray
                    |> Some
                    |> LspResult.success

            | _ -> return None |> LspResult.success
    }

    let incomingCalls
        (context: RequestContext)
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

            let! wf, solution = p.Item.Uri |> context.GetWorkspaceFolderReadySolution

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.Item.Uri p.Item.Range.Start wf

                match symInfo with
                | None -> return LspResult.success None
                | Some(symbol, _, _) ->
                    let! callers =
                        SymbolFinder.FindCallersAsync(symbol, solution, cancellationToken = ct)
                        |> Async.AwaitTask

                    let wfPathToUri path = workspaceFolderPathToUri path wf

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
        (_context: RequestContext)
        (_: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> =
        async {
            // TODO: There is no memthod of SymbolFinder which can find all outgoing calls of a specific symbol.
            // Then how can we implement it? Parsing AST manually?
            return None |> LspResult.success
        }
