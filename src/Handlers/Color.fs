namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module Color =
    let provider (clientCapabilities: ClientCapabilities option)  = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (wm: IWorkspaceManager) (p: DocumentColorParams) : AsyncLspResult<ColorInformation[]> = notImplemented

    let present (wm: IWorkspaceManager) (p: ColorPresentationParams) : AsyncLspResult<ColorPresentation[]> =
        notImplemented
