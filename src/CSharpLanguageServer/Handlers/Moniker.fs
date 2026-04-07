namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Moniker =
    let provider (_cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let handle
        (_context: RequestContext)
        (_p: MonikerParams)
        : Async<LspResult<Moniker array option> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<Moniker array option>, LspWorkspaceUpdate.Empty }
