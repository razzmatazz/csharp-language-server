namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module InlineValue =
    let provider (_cc: ClientCapabilities) : InlineValueOptions option = None

    let registration (_s: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: InlineValueParams) : AsyncLspResult<InlineValue[] option> = async {
        return LspResult.notImplemented<InlineValue[] option>
    }
