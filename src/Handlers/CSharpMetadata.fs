namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.AsyncLspResult

open CSharpLanguageServer.Common.Types

module CSharpMetadata =
    let handle (wm: IWorkspaceManager) (p: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> =
        wm.FindMetadata p.TextDocument.Uri |> success
