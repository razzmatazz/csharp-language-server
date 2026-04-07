namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (cc: ClientCapabilities) : DocumentLinkOptions option = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (p: DocumentLinkParams)
        : Async<LspResult<DocumentLink[] option> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<DocumentLink[] option>, LspWorkspaceUpdate.Empty }

    let resolve (context: RequestContext) (p: DocumentLink) : Async<LspResult<DocumentLink> * LspWorkspaceUpdate> = async {
        return LspResult.notImplemented<DocumentLink>, LspWorkspaceUpdate.Empty
    }
