namespace CSharpLanguageServer.State

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

type ServerRequestContext(requestId: int, state: ServerState, emitServerEvent: ServerStateEvent -> unit) =
    member _.RequestId = requestId
    member _.State = state
    member _.Workspace = state.Workspace
    member _.ClientCapabilities = state.ClientCapabilities

    member _.WindowShowMessage(m: string) =
        match state.LspClient with
        | Some lspClient ->
            lspClient.WindowShowMessage(
                { Type = MessageType.Info
                  Message = sprintf "csharp-ls: %s" m }
            )
        | None -> async.Return()

    member _.Emit ev = emitServerEvent ev

    member this.ResolveSymbolLocations
        (symbol: Microsoft.CodeAnalysis.ISymbol)
        (project: Microsoft.CodeAnalysis.Project option)
        =
        async {
            let! aggregatedLspLocations, wf =
                workspaceFolderSymbolLocations symbol project this.Workspace.SingletonFolder

            this.Emit(WorkspaceFolderChange wf)
            return aggregatedLspLocations
        }
