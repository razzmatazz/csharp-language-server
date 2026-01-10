namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_ctx: ServerRequestContext) (_p: SelectionRangeParams) : AsyncLspResult<SelectionRange array option> =
        LspResult.notImplemented<SelectionRange array option> |> async.Return
