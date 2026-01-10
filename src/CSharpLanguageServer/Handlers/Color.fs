namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Color =
    let provider (cc: ClientCapabilities) = None

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option = None

    let handle (context: ServerRequestContext) (p: DocumentColorParams) : AsyncLspResult<ColorInformation[]> =
        LspResult.notImplemented<ColorInformation[]> |> async.Return

    let present (context: ServerRequestContext) (p: ColorPresentationParams) : AsyncLspResult<ColorPresentation[]> =
        LspResult.notImplemented<ColorPresentation[]> |> async.Return
