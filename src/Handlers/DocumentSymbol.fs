namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module DocumentSymbol =
    let provider: bool option = None

    let handle
        (wm: IWorkspaceManager)
        (p: DocumentSymbolParams)
        : AsyncLspResult<U2<SymbolInformation[], DocumentSymbol[]> option> =
        notImplemented
