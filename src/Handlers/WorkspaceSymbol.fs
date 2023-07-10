namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider: U2<bool, WorkspaceSymbolOptions> option = None

    let handle (wm: IWorkspaceManager) (p: WorkspaceSymbolParams) : AsyncLspResult<U2<SymbolInformation[], WorkspaceSymbol[]> option> =
        notImplemented

    let resolve (wm: IWorkspaceManager) (p: WorkspaceSymbol) : AsyncLspResult<WorkspaceSymbol> =
        notImplemented
