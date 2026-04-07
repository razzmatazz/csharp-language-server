namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Color =
    let provider (cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (p: DocumentColorParams)
        : Async<LspResult<ColorInformation[]> * RequestEffects> =
        async { return LspResult.notImplemented<ColorInformation[]>, RequestEffects.Empty }

    let present
        (context: RequestContext)
        (p: ColorPresentationParams)
        : Async<LspResult<ColorPresentation[]> * RequestEffects> =
        async { return LspResult.notImplemented<ColorPresentation[]>, RequestEffects.Empty }
