namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (cc: ClientCapabilities) : DocumentLinkOptions option = None

    let registration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: DocumentLinkParams) : AsyncLspResult<DocumentLink[] option> = async {
        return LspResult.notImplemented<DocumentLink[] option>
    }

    let resolve (_a: ActivateServerRequest) (_p: DocumentLink) : AsyncLspResult<DocumentLink> = async {
        return LspResult.notImplemented<DocumentLink>
    }
