namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module LinkedEditingRange =
    let provider (clientCapabilities: ClientCapabilities option) = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (context: ServerRequestContext) (def: TextDocumentPositionParams) : AsyncLspResult<LinkedEditingRanges option> =
        LspResult.notImplemented<LinkedEditingRanges option> |> async.Return
