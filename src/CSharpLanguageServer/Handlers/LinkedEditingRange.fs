namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (clientCapabilities: ClientCapabilities) = None

    let handle (context: ServerRequestContext) (def: LinkedEditingRangeParams) : AsyncLspResult<LinkedEditingRanges option> =
        LspResult.notImplemented<LinkedEditingRanges option> |> async.Return
