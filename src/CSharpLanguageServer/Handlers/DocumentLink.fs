namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (cc: ClientCapabilities) : DocumentLinkOptions option = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle (context: RequestContext) (p: DocumentLinkParams) : AsyncLspResult<DocumentLink[] option> =
        LspResult.notImplemented<DocumentLink[] option> |> async.Return

    let resolve (context: RequestContext) (p: DocumentLink) : AsyncLspResult<DocumentLink> =
        LspResult.notImplemented<DocumentLink> |> async.Return
