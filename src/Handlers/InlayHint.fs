namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module InlayHint =
    let provider: InlayHintOptions option = None

    let handle (wm: IWorkspaceManager) (p: InlayHintParams) : AsyncLspResult<InlayHint[] option> = notImplemented

    let resolve (wm: IWorkspaceManager) (p: InlayHint) : AsyncLspResult<InlayHint> = notImplemented
