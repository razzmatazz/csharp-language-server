namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (def: LinkedEditingRangeParams)
        : Async<LspResult<LinkedEditingRanges option> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<LinkedEditingRanges option>, LspWorkspaceUpdate.Empty }
