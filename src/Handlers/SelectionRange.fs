namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module SelectionRange =
    let provider (clientCapabilities: ClientCapabilities option) : bool option = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (wm: IWorkspaceManager) (p: SelectionRangeParams) : AsyncLspResult<SelectionRange list option> =
        notImplemented
