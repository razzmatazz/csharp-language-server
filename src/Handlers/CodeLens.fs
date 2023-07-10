namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module CodeLens =
    let provider: CodeLensOptions option = None

    let handle (wm: IWorkspaceManager) (p: CodeLensParams) : AsyncLspResult<CodeLens[] option> = notImplemented

    let resolve (wm: IWorkspaceManager) (p: CodeLens) : AsyncLspResult<CodeLens> = notImplemented
