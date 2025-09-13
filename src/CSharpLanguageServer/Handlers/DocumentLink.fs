namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (clientCapabilities: ClientCapabilities) : DocumentLinkOptions option = None

    let handle (context: ServerRequestContext) (p: DocumentLinkParams) : AsyncLspResult<DocumentLink[] option> =
        LspResult.notImplemented<DocumentLink[] option> |> async.Return

    let resolve (context: ServerRequestContext) (p: DocumentLink) : AsyncLspResult<DocumentLink> =
        LspResult.notImplemented<DocumentLink> |> async.Return
