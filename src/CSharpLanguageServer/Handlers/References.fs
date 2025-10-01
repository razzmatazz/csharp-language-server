namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module References =
    let provider (_: ClientCapabilities) : U2<bool, ReferenceOptions> option = Some(U2.C1 true)

    let handle (context: ServerRequestContext) (p: ReferenceParams) : AsyncLspResult<Location[] option> = async {
        match! context.FindSymbol p.TextDocument.Uri p.Position with
        | None -> return None |> LspResult.success
        | Some symbol ->
            let! locations = context.FindReferences symbol p.Context.IncludeDeclaration

            return
                locations
                |> Seq.choose Location.fromRoslynLocation
                |> Seq.distinct
                |> Seq.toArray
                |> Some
                |> LspResult.success
    }
