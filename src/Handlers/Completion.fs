namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module Completion =
    let provider: CompletionOptions option = None

    let handle (wm: IWorkspaceManager) (p: CompletionParams) : AsyncLspResult<CompletionList option> = notImplemented

    let resolve (wm: IWorkspaceManager) (p: CompletionItem) : AsyncLspResult<CompletionItem> = notImplemented
