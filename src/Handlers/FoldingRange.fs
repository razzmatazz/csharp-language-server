namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module FoldingRange =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: FoldingRangeParams) : AsyncLspResult<FoldingRange list option> = notImplemented
