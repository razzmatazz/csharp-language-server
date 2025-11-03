namespace CSharpLanguageServer.State

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

type ServerRequestContext(requestId: int, state: ServerState, emitServerEvent) =
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
            let mutable wf = this.Workspace.SingletonFolder
            let mutable aggregatedLspLocations = []

            for l in symbol.Locations do
                let! symLspLocations, updatedWf = workspaceFolderResolveSymbolLocation project symbol l wf

                aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations
                wf <- updatedWf

            this.Emit(WorkspaceFolderChange wf)

            return aggregatedLspLocations
        }

    member this.FindSymbol' (uri: DocumentUri) (pos: Position) = async {
        let wf, docForUri = uri |> workspaceDocument this.Workspace AnyDocument

        match wf, docForUri with
        | Some wf, Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = Position.toRoslynPosition sourceText.Lines pos
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

            let symbolInfo =
                symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)

            return Some wf, symbolInfo

        | wf, _ -> return (wf, None)
    }

    member this.FindSymbol (uri: DocumentUri) (pos: Position) : Async<option<LspWorkspaceFolder> * option<ISymbol>> = async {
        let! wf, symbolInfo = this.FindSymbol' uri pos

        match symbolInfo with
        | Some(sym, _, _) -> return (wf, Some sym)
        | None -> return (wf, None)
    }
