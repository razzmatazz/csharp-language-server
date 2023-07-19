namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

module TextDocumentSync =
    let provider: TextDocumentSyncOptions option =
        Some
            { TextDocumentSyncOptions.Default with
                OpenClose = Some true
                Change = Some TextDocumentSyncKind.Incremental }

    let didOpen (wm: IWorkspaceManager) (p: DidOpenTextDocumentParams): Async<unit> =
        wm.OpenDocument p.TextDocument.Uri p.TextDocument.Version

    let didChange (wm: IWorkspaceManager) (p: DidChangeTextDocumentParams): Async<unit> =
        wm.ChangeDocument p.TextDocument.Uri p.TextDocument.Version.Value p.ContentChanges

    let didClose (wm: IWorkspaceManager) (p: DidCloseTextDocumentParams): Async<unit> =
        wm.CloseDocument p.TextDocument.Uri

    let willSave (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): Async<unit> = ignoreNotification

    let willSaveUntil (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): AsyncLspResult<TextEdit [] option> = notImplemented

    let didSave (wm: IWorkspaceManager) (p: DidSaveTextDocumentParams): Async<unit> = ignoreNotification
