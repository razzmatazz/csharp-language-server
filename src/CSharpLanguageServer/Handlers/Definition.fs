namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module Definition =
    let provider (_cc: ClientCapabilities) : U2<bool, DefinitionOptions> option = Some(U2.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: DefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, _) ->
                let! locations, updatedWf = workspaceFolderSymbolLocations symbol project wf

                context.Emit(WorkspaceFolderChange updatedWf)

                return locations |> Array.ofList |> Definition.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
