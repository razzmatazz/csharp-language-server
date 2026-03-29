namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Declaration =
    let provider (_cc: ClientCapabilities) : bool option = None

    let registration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let handle
        (_context: RequestContext)
        (_p: DeclarationParams)
        : AsyncLspResult<U2<Declaration, DeclarationLink array> option> =
        LspResult.notImplemented<U2<Declaration, DeclarationLink array> option>
        |> async.Return
