namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module FoldingRange =
    let provider (_c: ClientCapabilities) : bool option = None

    let registration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: FoldingRangeParams) : AsyncLspResult<FoldingRange array option> =
        LspResult.notImplemented<FoldingRange array option> |> async.Return
