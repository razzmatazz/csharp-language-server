namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

module TextDocumentSync =
    let provider (clientCapabilities: ClientCapabilities option) : TextDocumentSyncOptions option =
        Some
            { TextDocumentSyncOptions.Default with
                OpenClose = Some true
                Save = Some { IncludeText = Some true }
                Change = Some TextDocumentSyncKind.Incremental }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let didOpen (wm: IWorkspaceManager) (p: DidOpenTextDocumentParams): Async<unit> =
        wm.OpenDocument p.TextDocument.Uri p.TextDocument.Version p.TextDocument.Text

    let didChange (wm: IWorkspaceManager) (p: DidChangeTextDocumentParams): Async<unit> =
        wm.ChangeDocument p.TextDocument.Uri p.TextDocument.Version p.ContentChanges

    let didClose (wm: IWorkspaceManager) (p: DidCloseTextDocumentParams): Async<unit> =
        wm.CloseDocument p.TextDocument.Uri

    let willSave (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): Async<unit> = ignoreNotification

    let willSaveUntil (wm: IWorkspaceManager) (p: WillSaveTextDocumentParams): AsyncLspResult<TextEdit [] option> = notImplemented

    let didSave (wm: IWorkspaceManager) (p: DidSaveTextDocumentParams): Async<unit> =
        wm.SaveDocument p.TextDocument.Uri p.Text
