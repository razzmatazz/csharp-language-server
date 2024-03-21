namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module DocumentLink =
    let provider (clientCapabilities: ClientCapabilities option) : DocumentLinkOptions option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (wm: IWorkspaceManager) (p: DocumentLinkParams) : AsyncLspResult<DocumentLink[] option> = notImplemented

    let resolve (wm: IWorkspaceManager) (p: DocumentLink) : AsyncLspResult<DocumentLink> = notImplemented
