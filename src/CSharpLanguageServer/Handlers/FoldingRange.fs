namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module FoldingRange =
    let provider (clientCapabilities: ClientCapabilities option) : bool option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (p: FoldingRangeParams) : AsyncLspResult<FoldingRange list option> =
        notImplemented<FoldingRange list option> |> async.Return
