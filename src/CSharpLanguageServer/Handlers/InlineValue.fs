namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module InlineValue =
    let provider (clientCapabilities: ClientCapabilities option) : InlineValueOptions option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (wm: IWorkspaceManager) (p: InlineValueParams) : AsyncLspResult<InlineValue[] option> = notImplemented