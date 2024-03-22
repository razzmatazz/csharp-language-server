namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Formatting)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : U2<bool, WorkspaceSymbolOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> true |> U2.First |> Some

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/symbol"
                  RegisterOptions = { ResolveProvider = Some true } |> serialize |> Some }

    let findSymbolsInSolution (solution: Solution)
                              pattern
                              (_limit: int option)
            : Async<ISymbol seq> = async {
        let findTask =
            match pattern with
            | Some pat ->
                fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsWithPatternAsync(sln, pat, SymbolFilter.TypeAndMember)
            | None ->
                fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsAsync(sln, (fun _ -> true), SymbolFilter.TypeAndMember)

        return! findTask solution |> Async.AwaitTask
    }

    let handle (scope: ServerRequestScope)
               (p: WorkspaceSymbolParams)
            : AsyncLspResult<U2<SymbolInformation[],WorkspaceSymbol[]> option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = findSymbolsInSolution scope.Solution pattern (Some 20)
        return
            symbols
            |> Seq.map (SymbolInformation.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat)
            |> Seq.collect id
            // TODO: make 100 configurable?
            |> Seq.truncate 100
            |> Seq.toArray
            |> U2.First
            |> Some
            |> success
    }

    let resolve (scope: ServerRequestScope) (p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        LspResult.notImplemented<WorkspaceSymbol> |> async.Return
