namespace CSharpLanguageServer.State

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

type ServerRequestContext(requestId: int, state: ServerState, emitServerEvent) =
    let mutable solutionMaybe = state.Workspace.Solution

    member _.RequestId = requestId
    member _.State = state
    member _.Workspace = state.Workspace
    member _.ClientCapabilities = state.ClientCapabilities
    member _.Solution = solutionMaybe.Value

    member _.WindowShowMessage(m: string) =
        match state.LspClient with
        | Some lspClient ->
            lspClient.WindowShowMessage(
                { Type = MessageType.Info
                  Message = sprintf "csharp-ls: %s" m }
            )
        | None -> async.Return()

    member _.Emit ev =
        match ev with
        | WorkspaceFolderChange wf -> solutionMaybe <- wf.Solution
        | _ -> ()

        emitServerEvent ev

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

    member this.FindSymbol' (uri: DocumentUri) (pos: Position) : Async<(ISymbol * Project * Document option) option> = async {
        let docForUri = uri |> workspaceDocument this.Workspace AnyDocument

        match docForUri with
        | None -> return None
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = Position.toRoslynPosition sourceText.Lines pos
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
            return symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)
    }

    member this.FindSymbol (uri: DocumentUri) (pos: Position) : Async<ISymbol option> =
        this.FindSymbol' uri pos |> Async.map (Option.map (fun (sym, _, _) -> sym))
