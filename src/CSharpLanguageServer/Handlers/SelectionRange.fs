namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let handle
        (_ctx: RequestContext)
        (_p: SelectionRangeParams)
        : Async<LspResult<SelectionRange array option> * RequestEffects> =
        async { return LspResult.notImplemented<SelectionRange array option>, RequestEffects.Empty }
