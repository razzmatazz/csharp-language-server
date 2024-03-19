namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Moniker =
    let provider (clientCapabilities: ClientCapabilities option) = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (def: TextDocumentPositionParams) : AsyncLspResult<Moniker[] option> =
        notImplemented<Moniker[] option> |> async.Return
