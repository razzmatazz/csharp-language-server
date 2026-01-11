namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module InlineValue =
    let provider (_cc: ClientCapabilities) : InlineValueOptions option = None

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option = None

    let handle (context: ServerRequestContext) (p: InlineValueParams) : AsyncLspResult<InlineValue[] option> =
        LspResult.notImplemented<InlineValue[] option> |> async.Return
