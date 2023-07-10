namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module SignatureHelp =
    let provider: SignatureHelpOptions option = None

    let handle (wm: IWorkspaceManager) (p: SignatureHelpParams) : AsyncLspResult<SignatureHelp option> = notImplemented
