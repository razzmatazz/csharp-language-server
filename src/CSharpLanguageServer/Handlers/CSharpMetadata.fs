namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.State


[<RequireQualifiedAccess>]
module CSharpMetadata =
    let handle
        (context: ServerRequestContext)
        (p: CSharpMetadataParams)
        : AsyncLspResult<CSharpMetadataResponse option> =
        async {
            let workspaceFolder = context.State.Workspace.SingletonFolder

            let metadataMaybe =
                workspaceFolder.DecompiledMetadata
                |> Map.tryFind p.TextDocument.Uri
                |> Option.map _.Metadata

            return metadataMaybe |> LspResult.success
        }
