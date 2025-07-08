namespace CSharpLanguageServer.Handlers

open System

open FSharp.Control
open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let private logger = LogProvider.getLoggerByName "WorkspaceSymbol"

    let provider (_: ClientCapabilities) : U2<bool, WorkspaceSymbolOptions> option =
        let workspaceSymbolOptions: WorkspaceSymbolOptions =
            { WorkDoneProgress = None
              ResolveProvider = None
            }

        U2.C2 workspaceSymbolOptions |> Some

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

        let emptySymbolInformationList: SymbolInformation[] = Array.empty

        match context.State.Solution, p.PartialResultToken with
        | None, _ ->
            return U2.C1 emptySymbolInformationList |> Some |> LspResult.success

        | Some _, None ->
            failwith "will send full results"

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

        | Some _, Some partialResultToken ->
            failwith "will send partial results"

            let sendSymbolInformationPartialResult (sym: SymbolInformation list) = async {
                let progressParams: ProgressParams =
                    let partialResult: U2<SymbolInformation[], WorkspaceSymbol[]> = sym |> Array.ofList |> U2.C1

                    { Token = partialResultToken; Value = serialize partialResult }

                logger.error (
                    Log.setMessage "sending partial result"
                )

                let lspClient = context.State.LspClient.Value
                do! lspClient.Progress(progressParams)
            }

            do! context.FindSymbols pattern
                |> AsyncSeq.map (SymbolInformation.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat)
                |> AsyncSeq.iterAsync sendSymbolInformationPartialResult

            return U2.C1 emptySymbolInformationList |> Some |> LspResult.success
    }

    let resolve (_context: ServerRequestContext) (_p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        LspResult.notImplemented<WorkspaceSymbol> |> async.Return
