namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Color =
    let provider (clientCapabilities: ClientCapabilities option)  = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (p: DocumentColorParams) : AsyncLspResult<ColorInformation[]> =
        LspResult.notImplemented<ColorInformation[]> |> async.Return

    let present (scope: ServerRequestScope) (p: ColorPresentationParams) : AsyncLspResult<ColorPresentation[]> =
        LspResult.notImplemented<ColorPresentation[]> |> async.Return
