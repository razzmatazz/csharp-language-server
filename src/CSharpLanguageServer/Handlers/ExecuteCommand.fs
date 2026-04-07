namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider (_cc: ClientCapabilities) : ExecuteCommandOptions option = None

    let registration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let handle
        (_context: RequestContext)
        (_p: ExecuteCommandParams)
        : Async<LspResult<LSPAny option> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<LSPAny option>, LspWorkspaceUpdate.Empty }
