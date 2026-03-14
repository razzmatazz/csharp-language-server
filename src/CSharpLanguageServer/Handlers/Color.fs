namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Color =
    let provider (_cc: ClientCapabilities) = None

    let registration (_s: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let handle (_a: ActivateServerRequest) (_p: DocumentColorParams) : AsyncLspResult<ColorInformation[]> = async {
        return LspResult.notImplemented<ColorInformation[]>
    }

    let present (_a: ActivateServerRequest) (_p: ColorPresentationParams) : AsyncLspResult<ColorPresentation[]> = async {
        return LspResult.notImplemented<ColorPresentation[]>
    }
