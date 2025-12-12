namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider (_cc: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option = Some(U2.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: WorkspaceSymbolParams)
        : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> =
        async {
            let pattern = if String.IsNullOrEmpty(p.Query) then None else Some p.Query

            let! ct = Async.CancellationToken

            let mutable symbolInfos = []

            for wf in context.Workspace.Folders do
                let wfPathToUri = workspaceFolderPathToUri wf

                let! symbols =
                    match wf.Solution with
                    | None -> async.Return Seq.empty
                    | Some solution ->
                        match pattern with
                        | Some pat ->
                            SymbolFinder.FindSourceDeclarationsWithPatternAsync(
                                solution,
                                pat,
                                SymbolFilter.TypeAndMember,
                                cancellationToken = ct
                            )
                            |> Async.AwaitTask
                        | None ->
                            let true' = System.Func<string, bool>(fun _ -> true)

                            SymbolFinder.FindSourceDeclarationsAsync(
                                solution,
                                true',
                                SymbolFilter.TypeAndMember,
                                cancellationToken = ct
                            )
                            |> Async.AwaitTask

                let symbolInfosForWf =
                    symbols
                    |> Seq.map (SymbolInformation.fromSymbol wfPathToUri SymbolDisplayFormat.MinimallyQualifiedFormat)
                    |> Seq.collect id
                    |> List.ofSeq

                symbolInfos <- symbolInfos @ symbolInfosForWf

            return
                symbolInfos
                // TODO: make 100 configurable?
                |> Seq.truncate 100
                |> Seq.toArray
                |> U2.C1
                |> Some
                |> LspResult.success
        }

    let resolve (_context: ServerRequestContext) (_p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        LspResult.notImplemented<WorkspaceSymbol> |> async.Return
