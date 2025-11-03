namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider (_: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option = Some(U2.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: WorkspaceSymbolParams)
        : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> =
        async {
            let! ct = Async.CancellationToken

            let pattern = if String.IsNullOrEmpty(p.Query) then None else Some p.Query

            let! symbols =
                match context.Workspace.Solution with
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

            return
                symbols
                |> Seq.map (SymbolInformation.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat)
                |> Seq.collect id
                // TODO: make 100 configurable?
                |> Seq.truncate 100
                |> Seq.toArray
                |> U2.C1
                |> Some
                |> LspResult.success
        }

    let resolve (_context: ServerRequestContext) (_p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        LspResult.notImplemented<WorkspaceSymbol> |> async.Return
