namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

module TextDocumentSync =
    let provider: TextDocumentSyncOptions option = None

    let didOpen (wm: IWorkspaceManager) (p: DidOpenTextDocumentParams): Async<unit> = ignoreNotification

    let didChange (wm: IWorkspaceManager) (p: DidChangeTextDocumentParams): Async<unit> = ignoreNotification

    let didClose (wm: IWorkspaceManager) (p: DidCloseTextDocumentParams): Async<unit> = ignoreNotification

    let willSave (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): Async<unit> = ignoreNotification

    let willSaveUntil (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): AsyncLspResult<TextEdit [] option> = notImplemented

    let didSave (wm: IWorkspaceManager) (p: DidSaveTextDocumentParams): Async<unit> = ignoreNotification
