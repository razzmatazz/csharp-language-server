namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Declaration =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_cc: ClientCapabilities) : Registration option = None

    let handle (_context: ServerRequestContext) (_p: TextDocumentPositionParams) : AsyncLspResult<Declaration option> =
        LspResult.notImplemented<Declaration option> |> async.Return
