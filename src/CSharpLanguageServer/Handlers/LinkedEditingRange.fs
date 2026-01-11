namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (cc: ClientCapabilities) = None

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option = None

    let handle
        (context: ServerRequestContext)
        (def: LinkedEditingRangeParams)
        : AsyncLspResult<LinkedEditingRanges option> =
        LspResult.notImplemented<LinkedEditingRanges option> |> async.Return
