namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (def: LinkedEditingRangeParams)
        : Async<LspResult<LinkedEditingRanges option> * RequestEffects> =
        async { return LspResult.notImplemented<LinkedEditingRanges option>, RequestEffects.Empty }
