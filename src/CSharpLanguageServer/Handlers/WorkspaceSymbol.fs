namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.Workspace
        |> Option.bind _.Symbol
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registrationOptions: WorkspaceSymbolRegistrationOptions =
                { ResolveProvider = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "workspace/symbol"
                  RegisterOptions = registrationOptions |> serialize |> Some }

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
