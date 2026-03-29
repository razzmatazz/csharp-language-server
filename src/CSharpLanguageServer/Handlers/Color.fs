namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Color =
    let provider (cc: ClientCapabilities) = None

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option = None

    let handle (context: RequestContext) (p: DocumentColorParams) : AsyncLspResult<ColorInformation[]> =
        LspResult.notImplemented<ColorInformation[]> |> async.Return

    let present (context: RequestContext) (p: ColorPresentationParams) : AsyncLspResult<ColorPresentation[]> =
        LspResult.notImplemented<ColorPresentation[]> |> async.Return
