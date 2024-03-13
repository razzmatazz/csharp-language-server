namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module FoldingRange =
    let provider (clientCapabilities: ClientCapabilities option) : bool option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (p: FoldingRangeParams) : AsyncLspResult<FoldingRange list option> =
        LspResult.notImplemented<FoldingRange list option> |> async.Return
