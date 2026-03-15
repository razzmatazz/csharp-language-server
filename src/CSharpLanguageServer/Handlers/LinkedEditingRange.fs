namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (cc: ClientCapabilities) = None

    let registration (_s: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: LinkedEditingRangeParams) : AsyncLspResult<LinkedEditingRanges option> = async {
        return LspResult.notImplemented<LinkedEditingRanges option>
    }
