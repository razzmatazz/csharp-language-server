namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Definition =
    let provider (_cc: ClientCapabilities) : U2<bool, DefinitionOptions> option = Some(U2.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: DefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            match! context.FindSymbol' p.TextDocument.Uri p.Position with
            | None -> return None |> LspResult.success
            | Some(symbol, project, _) ->
                let! locations = context.ResolveSymbolLocations symbol (Some project)
                return locations |> Array.ofList |> Definition.C2 |> U2.C1 |> Some |> LspResult.success
        }
