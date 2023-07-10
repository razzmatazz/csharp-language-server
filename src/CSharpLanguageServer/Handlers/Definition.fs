namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module Definition =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: TextDocumentPositionParams) : AsyncLspResult<GotoResult option> =
        notImplemented
