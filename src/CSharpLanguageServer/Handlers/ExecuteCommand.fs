namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider (_cc: ClientCapabilities) : ExecuteCommandOptions option = None

    let registration (_s: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: ExecuteCommandParams) : AsyncLspResult<LSPAny option> = async {
        return LspResult.notImplemented<LSPAny option>
    }
