namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Moniker =
    let provider (_cc: ClientCapabilities) = None

    let registration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: MonikerParams) : AsyncLspResult<Moniker array option> = async {
        return LspResult.notImplemented<Moniker array option>
    }
