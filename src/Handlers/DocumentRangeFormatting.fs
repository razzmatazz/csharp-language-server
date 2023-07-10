namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module DocumentRangeFormatting =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> =
        notImplemented
