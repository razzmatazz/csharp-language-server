namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Moniker =
    let provider (_cc: ClientCapabilities) = None

    let registration (_cc: ClientCapabilities) : Registration option = None

    let handle (_context: ServerRequestContext) (_p: TextDocumentPositionParams) : AsyncLspResult<Moniker[] option> =
        notImplemented<Moniker[] option> |> async.Return
