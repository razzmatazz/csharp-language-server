namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module DocumentOnTypeFormatting =
    let provider: DocumentOnTypeFormattingOptions option = None

    let handle (wm: IWorkspaceManager) (p: DocumentOnTypeFormattingParams) : AsyncLspResult<TextEdit[] option> =
        notImplemented
