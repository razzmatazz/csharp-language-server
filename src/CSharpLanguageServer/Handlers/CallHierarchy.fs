namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
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

    let prepare
        (context: RequestContext)
        (p: CallHierarchyPrepareParams)
        : Async<LspResult<CallHierarchyItem[] option> * LspWorkspaceUpdate> =
        async {
            let! wf, _ = context.LoadWorkspaceFolder(p.TextDocument.Uri)

            match wf with
            | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | Some(symbol, project, _) when isCallableSymbol symbol ->
                    let! locations, wfUpdates = wf |> workspaceFolderSymbolLocations context.Config symbol project

                    let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)

                    let lspResult =
                        locations
                        |> Seq.map (CallHierarchyItem.fromSymbolAndLocation symbol)
                        |> Seq.toArray
                        |> Some
                        |> LspResult.success

                    return lspResult, wsUpdate

                | _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }

    let incomingCalls
        (context: RequestContext)
        (p: CallHierarchyIncomingCallsParams)
        : Async<LspResult<CallHierarchyIncomingCall[] option> * LspWorkspaceUpdate> =
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

            let! wf, solution = p.Item.Uri |> context.LoadWorkspaceFolder

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.Item.Uri p.Item.Range.Start wf

                match symInfo with
                | None -> return LspResult.success None, LspWorkspaceUpdate.Empty
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
                        |> LspResult.success,
                        LspWorkspaceUpdate.Empty

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }

    let outgoingCalls
        (context: RequestContext)
        (p: CallHierarchyOutgoingCallsParams)
        : Async<LspResult<CallHierarchyOutgoingCall[] option> * LspWorkspaceUpdate> =
        async {
            let! ct = Async.CancellationToken

            let! wf, solution = p.Item.Uri |> context.LoadWorkspaceFolder

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.Item.Uri p.Item.Range.Start wf

                match symInfo with
                | Some(symbol, _, _) when isCallableSymbol symbol ->
                    // SymbolFinder has no outgoing counterpart to FindCallersAsync, so walk
                    // every declaration body of the symbol (a partial method can have
                    // several) and resolve each invocation, object creation and
                    // constructor initializer through that declaration's semantic model.
                    let callSitesByTarget =
                        System.Collections.Generic.Dictionary<ISymbol, ResizeArray<Ionide.LanguageServerProtocol.Types.Range>>(
                            SymbolEqualityComparer.Default
                        )

                    for syntaxRef in symbol.DeclaringSyntaxReferences do
                        match solution.GetDocument(syntaxRef.SyntaxTree) |> Option.ofObj with
                        | None -> ()
                        | Some doc ->
                            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                            let! declNode = syntaxRef.GetSyntaxAsync(ct) |> Async.AwaitTask

                            match semanticModel |> Option.ofObj with
                            | None -> ()
                            | Some(semanticModel: SemanticModel) ->
                                let isCallNode (n: SyntaxNode) =
                                    n :? CSharp.Syntax.InvocationExpressionSyntax
                                    || n :? CSharp.Syntax.BaseObjectCreationExpressionSyntax
                                    || n :? CSharp.Syntax.ConstructorInitializerSyntax

                                // Anchor each call site at its callee name token, not at the
                                // start of the whole expression: in a fluent chain every
                                // link's invocation node begins at the chain head, so all
                                // links would otherwise report the same position.
                                let callAnchor (n: SyntaxNode) =
                                    match n with
                                    | :? CSharp.Syntax.InvocationExpressionSyntax as inv ->
                                        match inv.Expression with
                                        | :? CSharp.Syntax.MemberAccessExpressionSyntax as ma -> ma.Name.GetLocation()
                                        | :? CSharp.Syntax.MemberBindingExpressionSyntax as mb -> mb.Name.GetLocation()
                                        | expr -> expr.GetLocation()
                                    | :? CSharp.Syntax.ObjectCreationExpressionSyntax as oc -> oc.Type.GetLocation()
                                    | :? CSharp.Syntax.ConstructorInitializerSyntax as ci ->
                                        ci.ThisOrBaseKeyword.GetLocation()
                                    | _ -> n.GetLocation()

                                for callNode in declNode.DescendantNodes() |> Seq.filter isCallNode do
                                    // Normalize to the original definition so constructed
                                    // generic instantiations (Echo<int>, Echo<string>)
                                    // group as one target.
                                    let target =
                                        semanticModel.GetSymbolInfo(callNode, ct).Symbol
                                        |> Option.ofObj
                                        |> Option.map _.OriginalDefinition

                                    match target with
                                    | Some target when isCallableSymbol target ->
                                        let range =
                                            (callAnchor callNode).GetLineSpan().Span |> Range.fromLinePositionSpan

                                        match callSitesByTarget.TryGetValue target with
                                        | true, ranges -> ranges.Add range
                                        | false, _ -> callSitesByTarget[target] <- ResizeArray [ range ]
                                    | _ -> ()

                    let wfPathToUri path = workspaceFolderPathToUri path wf

                    // Targets without a source location (e.g. BCL symbols) are dropped,
                    // mirroring what incomingCalls does for callers.
                    let toOutgoingCall (KeyValue(target: ISymbol, ranges)) =
                        target.Locations
                        |> Seq.choose (Location.fromRoslynLocation wfPathToUri)
                        |> Seq.tryHead
                        |> Option.map (fun loc ->
                            { To = CallHierarchyItem.fromSymbolAndLocation target loc
                              FromRanges = ranges |> Seq.toArray })

                    return
                        callSitesByTarget
                        |> Seq.choose toOutgoingCall
                        |> Seq.toArray
                        |> Some
                        |> LspResult.success,
                        LspWorkspaceUpdate.Empty

                | _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
