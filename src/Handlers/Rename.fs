namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module Rename =
    let provider: U2<bool, RenameOptions> option = None

    let prepare (wm: IWorkspaceManager) (p: PrepareRenameParams) : AsyncLspResult<PrepareRenameResult option> =
        notImplemented

    let handle (wm: IWorkspaceManager) (p: RenameParams) : AsyncLspResult<WorkspaceEdit option> = notImplemented
