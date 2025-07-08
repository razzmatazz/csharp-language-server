namespace CSharpLanguageServer.Handlers

open System

open FSharp.Control
open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider (_: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option =
        U2.C1 true |> Some

    let registration (_: ClientCapabilities) : Registration option =
        let registrationOptions: WorkspaceSymbolRegistrationOptions =
            { ResolveProvider = None
              WorkDoneProgress = None
            }

        Some
            { Id = Guid.NewGuid().ToString()
              Method = "workspace/symbol"
              RegisterOptions = registrationOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: WorkspaceSymbolParams) : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> = async {
        let pattern = if String.IsNullOrEmpty(p.Query) then None else Some p.Query

        let! symbols = context.FindSymbols pattern |> AsyncSeq.toArrayAsync
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
