namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module DocumentFormatting =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: DocumentFormattingParams) : AsyncLspResult<TextEdit[] option> = notImplemented
