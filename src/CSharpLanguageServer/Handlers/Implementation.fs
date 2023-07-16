namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module Implementation =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: TextDocumentPositionParams) : AsyncLspResult<GotoResult option> = async {
        match! wm.FindSymbol p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some symbol ->
            let! impls = wm.FindImplementations symbol
            let! locations = impls |> Seq.map wm.ResolveSymbolLocations |> Async.Parallel

            return
                locations
                |> Array.collect List.toArray
                |> GotoResult.Multiple
                |> Some
                |> success
    }
