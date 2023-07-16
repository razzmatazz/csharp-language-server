namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module References =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: ReferenceParams) : AsyncLspResult<Location[] option> = async {
        match! wm.FindSymbol p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some symbol ->
            let! refs = wm.FindReferences symbol
            return
                refs
                |> Seq.collect (fun r -> r.Locations)
                |> Seq.map (fun rl -> Location.fromRoslynLocation rl.Location)
                |> Seq.toArray
                |> Some
                |> success
    }
