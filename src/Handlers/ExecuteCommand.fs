namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module ExecuteCommand =
    let provider: ExecuteCommandOptions option = None

    let handle (wm: IWorkspaceManager) (p: ExecuteCommandParams) : AsyncLspResult<LSPAny> = notImplemented
