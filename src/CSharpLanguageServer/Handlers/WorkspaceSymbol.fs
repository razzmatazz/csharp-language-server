namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.Workspace
        |> Option.bind (fun x -> x.Symbol)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some (U2.C1 true)

    let registration (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registrationOptions: WorkspaceSymbolRegistrationOptions =
                { ResolveProvider = None
                  WorkDoneProgress = None
                }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/symbol"
                  RegisterOptions = registrationOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: WorkspaceSymbolParams) : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = context.FindSymbols pattern
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
