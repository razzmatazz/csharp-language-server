namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider (clientCapabilities: ClientCapabilities option) : ExecuteCommandOptions option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (p: ExecuteCommandParams) : AsyncLspResult<LSPAny> =
        notImplemented<LSPAny> |> async.Return
