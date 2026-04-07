namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Declaration =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let handle
        (_context: RequestContext)
        (_p: DeclarationParams)
        : Async<LspResult<U2<Declaration, DeclarationLink array> option> * LspWorkspaceUpdate> =
        async {
            return LspResult.notImplemented<U2<Declaration, DeclarationLink array> option>, LspWorkspaceUpdate.Empty
        }
