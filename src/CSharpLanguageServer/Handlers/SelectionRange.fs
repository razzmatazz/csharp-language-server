namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_cc: ClientCapabilities) : Registration option = None

    let handle (_ctx: ServerRequestContext) (_p: SelectionRangeParams) : AsyncLspResult<SelectionRange list option> =
        LspResult.notImplemented<SelectionRange list option> |> async.Return
