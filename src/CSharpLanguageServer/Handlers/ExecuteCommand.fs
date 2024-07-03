namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider (_cc: ClientCapabilities) : ExecuteCommandOptions option = None

    let registration (_cc: ClientCapabilities) : Registration option = None

    let handle (_context: ServerRequestContext) (_p: ExecuteCommandParams) : AsyncLspResult<LSPAny> =
        notImplemented<LSPAny> |> async.Return
