namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module CodeAction =
    let provider: U2<bool, CodeActionOptions> option = None

    let handle (wm: IWorkspaceManager) (p: CodeActionParams) : AsyncLspResult<TextDocumentCodeActionResult option> =
        notImplemented

    let resolve (wm: IWorkspaceManager) (p: CodeAction) : AsyncLspResult<CodeAction option> = notImplemented
