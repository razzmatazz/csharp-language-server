namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider (clientCapabilities: ClientCapabilities option) : bool option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (context: ServerRequestContext) (p: SelectionRangeParams) : AsyncLspResult<SelectionRange list option> =
        LspResult.notImplemented<SelectionRange list option> |> async.Return
