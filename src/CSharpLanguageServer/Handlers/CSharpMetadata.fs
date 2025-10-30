namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module CSharpMetadata =
    let handle
        (context: ServerRequestContext)
        (p: CSharpMetadataParams)
        : AsyncLspResult<CSharpMetadataResponse option> =

        p.TextDocument.Uri
        |> workspaceFolder context.Workspace
        |> Option.map _.DecompiledMetadata
        |> Option.bind (Map.tryFind p.TextDocument.Uri)
        |> Option.map _.Metadata
        |> LspResult.success
        |> async.Return
