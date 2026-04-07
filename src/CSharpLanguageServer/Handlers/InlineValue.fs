namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module InlineValue =
    let provider (_cc: ClientCapabilities) : InlineValueOptions option = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: RequestContext)
        (p: InlineValueParams)
        : Async<LspResult<InlineValue[] option> * RequestEffects> =
        async { return LspResult.notImplemented<InlineValue[] option>, RequestEffects.Empty }
