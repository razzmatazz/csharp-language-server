namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    open Microsoft.CodeAnalysis.FindSymbols
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Formatting)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : U2<bool, WorkspaceSymbolOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> U2.First true |> Some

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
            : Async<SymbolInformation list> = async {
        let findTask =
            match pattern with
            | Some pat ->
                fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsWithPatternAsync(sln, pat, SymbolFilter.TypeAndMember)
            | None ->
                fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsAsync(sln, (fun _ -> true), SymbolFilter.TypeAndMember)
        let! symbolsFound = findTask solution |> Async.AwaitTask
        return symbolsFound
            |> Seq.collect (SymbolInformation.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat)
            |> List.ofSeq
    }

    let handle (scope: ServerRequestScope)
               (p: WorkspaceSymbolParams)
            : AsyncLspResult<U2<SymbolInformation array,WorkspaceSymbol array> option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = findSymbolsInSolution scope.Solution pattern (Some 20)

        return
            symbols
            |> Array.ofSeq
            |> U2.First
            |> Some
            |> success
    }
