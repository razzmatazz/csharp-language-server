namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Color =
    let provider (cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (p: DocumentColorParams)
        : Async<LspResult<ColorInformation[]> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<ColorInformation[]>, LspWorkspaceUpdate.Empty }

    let present
        (context: RequestContext)
        (p: ColorPresentationParams)
        : Async<LspResult<ColorPresentation[]> * LspWorkspaceUpdate> =
        async { return LspResult.notImplemented<ColorPresentation[]>, LspWorkspaceUpdate.Empty }
