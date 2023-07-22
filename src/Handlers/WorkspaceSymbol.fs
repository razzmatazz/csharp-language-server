namespace CSharpLanguageServer.Handlers

open System
open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.LspUtil
open CSharpLanguageServer.Common.Types

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

    let handle (wm: IWorkspaceManager) (p: WorkspaceSymbolParams) : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = wm.FindSymbols pattern
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

    let resolve (wm: IWorkspaceManager) (p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        notImplemented
