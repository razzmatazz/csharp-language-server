namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: SelectionRangeParams) : AsyncLspResult<SelectionRange list option> =
        notImplemented
