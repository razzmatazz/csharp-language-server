namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module InlineValue =
    let provider (clientCapabilities: ClientCapabilities) : InlineValueOptions option = None

    let registration (clientCapabilities: ClientCapabilities) : Registration option = None

    let handle (context: ServerRequestContext) (p: InlineValueParams) : AsyncLspResult<InlineValue[] option> =
        LspResult.notImplemented<InlineValue[] option> |> async.Return
