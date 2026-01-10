namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (cc: ClientCapabilities) : DocumentLinkOptions option = None

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option = None

    let handle (context: ServerRequestContext) (p: DocumentLinkParams) : AsyncLspResult<DocumentLink[] option> =
        LspResult.notImplemented<DocumentLink[] option> |> async.Return

    let resolve (context: ServerRequestContext) (p: DocumentLink) : AsyncLspResult<DocumentLink> =
        LspResult.notImplemented<DocumentLink> |> async.Return
