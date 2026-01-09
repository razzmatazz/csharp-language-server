namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Moniker =
    let provider (_cc: ClientCapabilities) = None

    let registration (_cc: ClientCapabilities) : Registration option = None

    let handle (_context: ServerRequestContext) (_p: MonikerParams) : AsyncLspResult<Moniker array option> =
        LspResult.notImplemented<Moniker array option> |> async.Return
