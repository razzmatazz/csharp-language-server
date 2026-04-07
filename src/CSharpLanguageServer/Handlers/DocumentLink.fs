namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (cc: ClientCapabilities) : DocumentLinkOptions option = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (p: DocumentLinkParams)
        : Async<LspResult<DocumentLink[] option> * RequestEffects> =
        async { return LspResult.notImplemented<DocumentLink[] option>, RequestEffects.Empty }

    let resolve (context: RequestContext) (p: DocumentLink) : Async<LspResult<DocumentLink> * RequestEffects> = async {
        return LspResult.notImplemented<DocumentLink>, RequestEffects.Empty
    }
