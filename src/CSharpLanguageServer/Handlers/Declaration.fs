namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Declaration =
    let provider (_cc: ClientCapabilities option) : bool option = None

    let registration (_cc: ClientCapabilities option) : Registration option = None

    let handle (_context: ServerRequestContext) (_p: TextDocumentPositionParams) : AsyncLspResult<Declaration option> =
        LspResult.notImplemented<Declaration option> |> async.Return
