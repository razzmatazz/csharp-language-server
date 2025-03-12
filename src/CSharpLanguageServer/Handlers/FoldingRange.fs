namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module FoldingRange =
    let provider (_c: ClientCapabilities) : bool option = None

    let registration (_c: ClientCapabilities) : Registration option = None

    let handle (_c: ServerRequestContext) (_p: FoldingRangeParams) : AsyncLspResult<FoldingRange list option> =
        LspResult.notImplemented<FoldingRange list option> |> async.Return
