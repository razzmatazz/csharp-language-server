namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module InlineValue =
    let provider: InlineValueOptions option = None

    let handle (wm: IWorkspaceManager) (p: InlineValueParams) : AsyncLspResult<InlineValue[] option> = notImplemented
