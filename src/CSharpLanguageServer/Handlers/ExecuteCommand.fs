namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider (_cc: ClientCapabilities) : ExecuteCommandOptions option = None

    let handle (_context: ServerRequestContext) (_p: ExecuteCommandParams) : AsyncLspResult<LSPAny option> =
        LspResult.notImplemented<LSPAny option> |> async.Return
