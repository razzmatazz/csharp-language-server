namespace Ionide.LanguageServerProtocol

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

type ILspServer =
  inherit System.IDisposable
  // Notifications
  /// The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
  /// folder configuration changes.
  abstract WorkspaceDidChangeWorkspaceFolders: DidChangeWorkspaceFoldersParams -> Async<unit>
  /// The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress
  /// initiated on the server side.
  abstract WindowWorkDoneProgressCancel: WorkDoneProgressCancelParams -> Async<unit>
  /// The did create files notification is sent from the client to the server when
  /// files were created from within the client.
  ///
  /// @since 3.16.0
  abstract WorkspaceDidCreateFiles: CreateFilesParams -> Async<unit>
  /// The did rename files notification is sent from the client to the server when
  /// files were renamed from within the client.
  ///
  /// @since 3.16.0
  abstract WorkspaceDidRenameFiles: RenameFilesParams -> Async<unit>
  /// The will delete files request is sent from the client to the server before files are actually
  /// deleted as long as the deletion is triggered from within the client.
  ///
  /// @since 3.16.0
  abstract WorkspaceDidDeleteFiles: DeleteFilesParams -> Async<unit>
  /// A notification sent when a notebook opens.
  ///
  /// @since 3.17.0
  abstract NotebookDocumentDidOpen: DidOpenNotebookDocumentParams -> Async<unit>
  abstract NotebookDocumentDidChange: DidChangeNotebookDocumentParams -> Async<unit>
  /// A notification sent when a notebook document is saved.
  ///
  /// @since 3.17.0
  abstract NotebookDocumentDidSave: DidSaveNotebookDocumentParams -> Async<unit>
  /// A notification sent when a notebook closes.
  ///
  /// @since 3.17.0
  abstract NotebookDocumentDidClose: DidCloseNotebookDocumentParams -> Async<unit>
  /// The initialized notification is sent from the client to the
  /// server after the client is fully initialized and the server
  /// is allowed to send requests from the server to the client.
  abstract Initialized: InitializedParams -> Async<unit>
  /// The exit event is sent from the client to the server to
  /// ask the server to exit its process.
  abstract Exit: unit -> Async<unit>
  /// The configuration change notification is sent from the client to the server
  /// when the client's configuration has changed. The notification contains
  /// the changed configuration as defined by the language client.
  abstract WorkspaceDidChangeConfiguration: DidChangeConfigurationParams -> Async<unit>
  /// The document open notification is sent from the client to the server to signal
  /// newly opened text documents. The document's truth is now managed by the client
  /// and the server must not try to read the document's truth using the document's
  /// uri. Open in this sense means it is managed by the client. It doesn't necessarily
  /// mean that its content is presented in an editor. An open notification must not
  /// be sent more than once without a corresponding close notification send before.
  /// This means open and close notification must be balanced and the max open count
  /// is one.
  abstract TextDocumentDidOpen: DidOpenTextDocumentParams -> Async<unit>
  /// The document change notification is sent from the client to the server to signal
  /// changes to a text document.
  abstract TextDocumentDidChange: DidChangeTextDocumentParams -> Async<unit>
  /// The document close notification is sent from the client to the server when
  /// the document got closed in the client. The document's truth now exists where
  /// the document's uri points to (e.g. if the document's uri is a file uri the
  /// truth now exists on disk). As with the open notification the close notification
  /// is about managing the document's content. Receiving a close notification
  /// doesn't mean that the document was open in an editor before. A close
  /// notification requires a previous open notification to be sent.
  abstract TextDocumentDidClose: DidCloseTextDocumentParams -> Async<unit>
  /// The document save notification is sent from the client to the server when
  /// the document got saved in the client.
  abstract TextDocumentDidSave: DidSaveTextDocumentParams -> Async<unit>
  /// A document will save notification is sent from the client to the server before
  /// the document is actually saved.
  abstract TextDocumentWillSave: WillSaveTextDocumentParams -> Async<unit>
  /// The watched files notification is sent from the client to the server when
  /// the client detects changes to file watched by the language client.
  abstract WorkspaceDidChangeWatchedFiles: DidChangeWatchedFilesParams -> Async<unit>
  abstract SetTrace: SetTraceParams -> Async<unit>
  abstract CancelRequest: CancelParams -> Async<unit>
  abstract Progress: ProgressParams -> Async<unit>
  // Requests
  /// A request to resolve the implementation locations of a symbol at a given text
  /// document position. The request's parameter is of type {@link TextDocumentPositionParams}
  /// the response is of type {@link Definition} or a Thenable that resolves to such.
  abstract TextDocumentImplementation:
    ImplementationParams -> AsyncLspResult<option<U2<Definition, array<DefinitionLink>>>>

  /// A request to resolve the type definition locations of a symbol at a given text
  /// document position. The request's parameter is of type {@link TextDocumentPositionParams}
  /// the response is of type {@link Definition} or a Thenable that resolves to such.
  abstract TextDocumentTypeDefinition:
    TypeDefinitionParams -> AsyncLspResult<option<U2<Definition, array<DefinitionLink>>>>

  /// A request to list all color symbols found in a given text document. The request's
  /// parameter is of type {@link DocumentColorParams} the
  /// response is of type {@link ColorInformation ColorInformation[]} or a Thenable
  /// that resolves to such.
  abstract TextDocumentDocumentColor: DocumentColorParams -> AsyncLspResult<array<ColorInformation>>
  /// A request to list all presentation for a color. The request's
  /// parameter is of type {@link ColorPresentationParams} the
  /// response is of type {@link ColorInformation ColorInformation[]} or a Thenable
  /// that resolves to such.
  abstract TextDocumentColorPresentation: ColorPresentationParams -> AsyncLspResult<array<ColorPresentation>>
  /// A request to provide folding ranges in a document. The request's
  /// parameter is of type {@link FoldingRangeParams}, the
  /// response is of type {@link FoldingRangeList} or a Thenable
  /// that resolves to such.
  abstract TextDocumentFoldingRange: FoldingRangeParams -> AsyncLspResult<option<array<FoldingRange>>>
  /// A request to resolve the type definition locations of a symbol at a given text
  /// document position. The request's parameter is of type {@link TextDocumentPositionParams}
  /// the response is of type {@link Declaration} or a typed array of {@link DeclarationLink}
  /// or a Thenable that resolves to such.
  abstract TextDocumentDeclaration: DeclarationParams -> AsyncLspResult<option<U2<Declaration, array<DeclarationLink>>>>
  /// A request to provide selection ranges in a document. The request's
  /// parameter is of type {@link SelectionRangeParams}, the
  /// response is of type {@link SelectionRange SelectionRange[]} or a Thenable
  /// that resolves to such.
  abstract TextDocumentSelectionRange: SelectionRangeParams -> AsyncLspResult<option<array<SelectionRange>>>

  /// A request to result a `CallHierarchyItem` in a document at a given position.
  /// Can be used as an input to an incoming or outgoing call hierarchy.
  ///
  /// @since 3.16.0
  abstract TextDocumentPrepareCallHierarchy:
    CallHierarchyPrepareParams -> AsyncLspResult<option<array<CallHierarchyItem>>>

  /// A request to resolve the incoming calls for a given `CallHierarchyItem`.
  ///
  /// @since 3.16.0
  abstract CallHierarchyIncomingCalls:
    CallHierarchyIncomingCallsParams -> AsyncLspResult<option<array<CallHierarchyIncomingCall>>>

  /// A request to resolve the outgoing calls for a given `CallHierarchyItem`.
  ///
  /// @since 3.16.0
  abstract CallHierarchyOutgoingCalls:
    CallHierarchyOutgoingCallsParams -> AsyncLspResult<option<array<CallHierarchyOutgoingCall>>>

  /// @since 3.16.0
  abstract TextDocumentSemanticTokensFull: SemanticTokensParams -> AsyncLspResult<option<SemanticTokens>>

  /// @since 3.16.0
  abstract TextDocumentSemanticTokensFullDelta:
    SemanticTokensDeltaParams -> AsyncLspResult<option<U2<SemanticTokens, SemanticTokensDelta>>>

  /// @since 3.16.0
  abstract TextDocumentSemanticTokensRange: SemanticTokensRangeParams -> AsyncLspResult<option<SemanticTokens>>
  /// A request to provide ranges that can be edited together.
  ///
  /// @since 3.16.0
  abstract TextDocumentLinkedEditingRange: LinkedEditingRangeParams -> AsyncLspResult<option<LinkedEditingRanges>>
  /// The will create files request is sent from the client to the server before files are actually
  /// created as long as the creation is triggered from within the client.
  ///
  /// The request can return a `WorkspaceEdit` which will be applied to workspace before the
  /// files are created. Hence the `WorkspaceEdit` can not manipulate the content of the file
  /// to be created.
  ///
  /// @since 3.16.0
  abstract WorkspaceWillCreateFiles: CreateFilesParams -> AsyncLspResult<option<WorkspaceEdit>>
  /// The will rename files request is sent from the client to the server before files are actually
  /// renamed as long as the rename is triggered from within the client.
  ///
  /// @since 3.16.0
  abstract WorkspaceWillRenameFiles: RenameFilesParams -> AsyncLspResult<option<WorkspaceEdit>>
  /// The did delete files notification is sent from the client to the server when
  /// files were deleted from within the client.
  ///
  /// @since 3.16.0
  abstract WorkspaceWillDeleteFiles: DeleteFilesParams -> AsyncLspResult<option<WorkspaceEdit>>
  /// A request to get the moniker of a symbol at a given text document position.
  /// The request parameter is of type {@link TextDocumentPositionParams}.
  /// The response is of type {@link Moniker Moniker[]} or `null`.
  abstract TextDocumentMoniker: MonikerParams -> AsyncLspResult<option<array<Moniker>>>

  /// A request to result a `TypeHierarchyItem` in a document at a given position.
  /// Can be used as an input to a subtypes or supertypes type hierarchy.
  ///
  /// @since 3.17.0
  abstract TextDocumentPrepareTypeHierarchy:
    TypeHierarchyPrepareParams -> AsyncLspResult<option<array<TypeHierarchyItem>>>

  /// A request to resolve the supertypes for a given `TypeHierarchyItem`.
  ///
  /// @since 3.17.0
  abstract TypeHierarchySupertypes: TypeHierarchySupertypesParams -> AsyncLspResult<option<array<TypeHierarchyItem>>>
  /// A request to resolve the subtypes for a given `TypeHierarchyItem`.
  ///
  /// @since 3.17.0
  abstract TypeHierarchySubtypes: TypeHierarchySubtypesParams -> AsyncLspResult<option<array<TypeHierarchyItem>>>
  /// A request to provide inline values in a document. The request's parameter is of
  /// type {@link InlineValueParams}, the response is of type
  /// {@link InlineValue InlineValue[]} or a Thenable that resolves to such.
  ///
  /// @since 3.17.0
  abstract TextDocumentInlineValue: InlineValueParams -> AsyncLspResult<option<array<InlineValue>>>
  /// A request to provide inlay hints in a document. The request's parameter is of
  /// type {@link InlayHintsParams}, the response is of type
  /// {@link InlayHint InlayHint[]} or a Thenable that resolves to such.
  ///
  /// @since 3.17.0
  abstract TextDocumentInlayHint: InlayHintParams -> AsyncLspResult<option<array<InlayHint>>>
  /// A request to resolve additional properties for an inlay hint.
  /// The request's parameter is of type {@link InlayHint}, the response is
  /// of type {@link InlayHint} or a Thenable that resolves to such.
  ///
  /// @since 3.17.0
  abstract InlayHintResolve: InlayHint -> AsyncLspResult<InlayHint>
  /// The document diagnostic request definition.
  ///
  /// @since 3.17.0
  abstract TextDocumentDiagnostic: DocumentDiagnosticParams -> AsyncLspResult<DocumentDiagnosticReport>
  /// The workspace diagnostic request definition.
  ///
  /// @since 3.17.0
  abstract WorkspaceDiagnostic: WorkspaceDiagnosticParams -> AsyncLspResult<WorkspaceDiagnosticReport>
  /// The initialize request is sent from the client to the server.
  /// It is sent once as the request after starting up the server.
  /// The requests parameter is of type {@link InitializeParams}
  /// the response if of type {@link InitializeResult} of a Thenable that
  /// resolves to such.
  abstract Initialize: InitializeParams -> AsyncLspResult<InitializeResult>
  /// A shutdown request is sent from the client to the server.
  /// It is sent once when the client decides to shutdown the
  /// server. The only notification that is sent after a shutdown request
  /// is the exit event.
  abstract Shutdown: unit -> AsyncLspResult<unit>
  /// A document will save request is sent from the client to the server before
  /// the document is actually saved. The request can return an array of TextEdits
  /// which will be applied to the text document before it is saved. Please note that
  /// clients might drop results if computing the text edits took too long or if a
  /// server constantly fails on this request. This is done to keep the save fast and
  /// reliable.
  abstract TextDocumentWillSaveWaitUntil: WillSaveTextDocumentParams -> AsyncLspResult<option<array<TextEdit>>>
  /// Request to request completion at a given text document position. The request's
  /// parameter is of type {@link TextDocumentPosition} the response
  /// is of type {@link CompletionItem CompletionItem[]} or {@link CompletionList}
  /// or a Thenable that resolves to such.
  ///
  /// The request can delay the computation of the {@link CompletionItem.detail `detail`}
  /// and {@link CompletionItem.documentation `documentation`} properties to the `completionItem/resolve`
  /// request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
  /// `filterText`, `insertText`, and `textEdit`, must not be changed during resolve.
  abstract TextDocumentCompletion: CompletionParams -> AsyncLspResult<option<U2<array<CompletionItem>, CompletionList>>>
  /// Request to resolve additional information for a given completion item.The request's
  /// parameter is of type {@link CompletionItem} the response
  /// is of type {@link CompletionItem} or a Thenable that resolves to such.
  abstract CompletionItemResolve: CompletionItem -> AsyncLspResult<CompletionItem>
  /// Request to request hover information at a given text document position. The request's
  /// parameter is of type {@link TextDocumentPosition} the response is of
  /// type {@link Hover} or a Thenable that resolves to such.
  abstract TextDocumentHover: HoverParams -> AsyncLspResult<option<Hover>>
  abstract TextDocumentSignatureHelp: SignatureHelpParams -> AsyncLspResult<option<SignatureHelp>>
  /// A request to resolve the definition location of a symbol at a given text
  /// document position. The request's parameter is of type {@link TextDocumentPosition}
  /// the response is of either type {@link Definition} or a typed array of
  /// {@link DefinitionLink} or a Thenable that resolves to such.
  abstract TextDocumentDefinition: DefinitionParams -> AsyncLspResult<option<U2<Definition, array<DefinitionLink>>>>
  /// A request to resolve project-wide references for the symbol denoted
  /// by the given text document position. The request's parameter is of
  /// type {@link ReferenceParams} the response is of type
  /// {@link Location Location[]} or a Thenable that resolves to such.
  abstract TextDocumentReferences: ReferenceParams -> AsyncLspResult<option<array<Location>>>
  /// Request to resolve a {@link DocumentHighlight} for a given
  /// text document position. The request's parameter is of type {@link TextDocumentPosition}
  /// the request response is an array of type {@link DocumentHighlight}
  /// or a Thenable that resolves to such.
  abstract TextDocumentDocumentHighlight: DocumentHighlightParams -> AsyncLspResult<option<array<DocumentHighlight>>>

  /// A request to list all symbols found in a given text document. The request's
  /// parameter is of type {@link TextDocumentIdentifier} the
  /// response is of type {@link SymbolInformation SymbolInformation[]} or a Thenable
  /// that resolves to such.
  abstract TextDocumentDocumentSymbol:
    DocumentSymbolParams -> AsyncLspResult<option<U2<array<SymbolInformation>, array<DocumentSymbol>>>>

  /// A request to provide commands for the given text document and range.
  abstract TextDocumentCodeAction: CodeActionParams -> AsyncLspResult<option<array<U2<Command, CodeAction>>>>
  /// Request to resolve additional information for a given code action.The request's
  /// parameter is of type {@link CodeAction} the response
  /// is of type {@link CodeAction} or a Thenable that resolves to such.
  abstract CodeActionResolve: CodeAction -> AsyncLspResult<CodeAction>

  /// A request to list project-wide symbols matching the query string given
  /// by the {@link WorkspaceSymbolParams}. The response is
  /// of type {@link SymbolInformation SymbolInformation[]} or a Thenable that
  /// resolves to such.
  ///
  /// @since 3.17.0 - support for WorkspaceSymbol in the returned data. Clients
  ///  need to advertise support for WorkspaceSymbols via the client capability
  ///  `workspace.symbol.resolveSupport`.
  abstract WorkspaceSymbol:
    WorkspaceSymbolParams -> AsyncLspResult<option<U2<array<SymbolInformation>, array<WorkspaceSymbol>>>>

  /// A request to resolve the range inside the workspace
  /// symbol's location.
  ///
  /// @since 3.17.0
  abstract WorkspaceSymbolResolve: WorkspaceSymbol -> AsyncLspResult<WorkspaceSymbol>
  /// A request to provide code lens for the given text document.
  abstract TextDocumentCodeLens: CodeLensParams -> AsyncLspResult<option<array<CodeLens>>>
  /// A request to resolve a command for a given code lens.
  abstract CodeLensResolve: CodeLens -> AsyncLspResult<CodeLens>
  /// A request to provide document links
  abstract TextDocumentDocumentLink: DocumentLinkParams -> AsyncLspResult<option<array<DocumentLink>>>
  /// Request to resolve additional information for a given document link. The request's
  /// parameter is of type {@link DocumentLink} the response
  /// is of type {@link DocumentLink} or a Thenable that resolves to such.
  abstract DocumentLinkResolve: DocumentLink -> AsyncLspResult<DocumentLink>
  /// A request to format a whole document.
  abstract TextDocumentFormatting: DocumentFormattingParams -> AsyncLspResult<option<array<TextEdit>>>
  /// A request to format a range in a document.
  abstract TextDocumentRangeFormatting: DocumentRangeFormattingParams -> AsyncLspResult<option<array<TextEdit>>>
  /// A request to format a document on type.
  abstract TextDocumentOnTypeFormatting: DocumentOnTypeFormattingParams -> AsyncLspResult<option<array<TextEdit>>>
  /// A request to rename a symbol.
  abstract TextDocumentRename: RenameParams -> AsyncLspResult<option<WorkspaceEdit>>
  /// A request to test and perform the setup necessary for a rename.
  ///
  /// @since 3.16 - support for default behavior
  abstract TextDocumentPrepareRename: PrepareRenameParams -> AsyncLspResult<option<PrepareRenameResult>>
  /// A request send from the client to the server to execute a command. The request might return
  /// a workspace edit which the client will apply to the workspace.
  abstract WorkspaceExecuteCommand: ExecuteCommandParams -> AsyncLspResult<option<LSPAny>>

type ILspClient =
  inherit System.IDisposable
  // Notifications
  /// The show message notification is sent from a server to a client to ask
  /// the client to display a particular message in the user interface.
  abstract WindowShowMessage: ShowMessageParams -> Async<unit>
  /// The log message notification is sent from the server to the client to ask
  /// the client to log a particular message.
  abstract WindowLogMessage: LogMessageParams -> Async<unit>
  /// The telemetry event notification is sent from the server to the client to ask
  /// the client to log telemetry data.
  abstract TelemetryEvent: LSPAny -> Async<unit>
  /// Diagnostics notification are sent from the server to the client to signal
  /// results of validation runs.
  abstract TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>
  abstract LogTrace: LogTraceParams -> Async<unit>
  abstract CancelRequest: CancelParams -> Async<unit>
  abstract Progress: ProgressParams -> Async<unit>
  // Requests
  /// The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders.
  abstract WorkspaceWorkspaceFolders: unit -> AsyncLspResult<option<array<WorkspaceFolder>>>
  /// The 'workspace/configuration' request is sent from the server to the client to fetch a certain
  /// configuration setting.
  ///
  /// This pull model replaces the old push model where the client signaled configuration change via an
  /// event. If the server still needs to react to configuration changes (since the server caches the
  /// result of `workspace/configuration` requests) the server should register for an empty configuration
  /// change event and empty the cache if such an event is received.
  abstract WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<array<LSPAny>>
  /// The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress
  /// reporting from the server.
  abstract WindowWorkDoneProgressCreate: WorkDoneProgressCreateParams -> AsyncLspResult<unit>
  /// @since 3.16.0
  abstract WorkspaceSemanticTokensRefresh: unit -> AsyncLspResult<unit>
  /// A request to show a document. This request might open an
  /// external program depending on the value of the URI to open.
  /// For example a request to open `https://code.visualstudio.com/`
  /// will very likely open the URI in a WEB browser.
  ///
  /// @since 3.16.0
  abstract WindowShowDocument: ShowDocumentParams -> AsyncLspResult<ShowDocumentResult>
  /// @since 3.17.0
  abstract WorkspaceInlineValueRefresh: unit -> AsyncLspResult<unit>
  /// @since 3.17.0
  abstract WorkspaceInlayHintRefresh: unit -> AsyncLspResult<unit>
  /// The diagnostic refresh request definition.
  ///
  /// @since 3.17.0
  abstract WorkspaceDiagnosticRefresh: unit -> AsyncLspResult<unit>
  /// The `client/registerCapability` request is sent from the server to the client to register a new capability
  /// handler on the client side.
  abstract ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>
  /// The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
  /// handler on the client side.
  abstract ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>
  /// The show message request is sent from the server to the client to show a message
  /// and a set of options actions to the user.
  abstract WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<option<MessageActionItem>>
  /// A request to refresh all code actions
  ///
  /// @since 3.16.0
  abstract WorkspaceCodeLensRefresh: unit -> AsyncLspResult<unit>
  /// A request sent from the server to the client to modified certain resources.
  abstract WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResult>

module Mappings =
  type ServerRequestHandling<'server when 'server :> ILspServer> = { Run: 'server -> System.Delegate }

  let routeMappings () =
    let serverRequestHandling run = {
      Run =
        fun server ->
          run server
          |> JsonRpc.Requests.requestHandling
    }

    [
      "textDocument/implementation",
      serverRequestHandling (fun server request -> server.TextDocumentImplementation(request))
      "textDocument/typeDefinition",
      serverRequestHandling (fun server request -> server.TextDocumentTypeDefinition(request))
      "textDocument/documentColor",
      serverRequestHandling (fun server request -> server.TextDocumentDocumentColor(request))
      "textDocument/colorPresentation",
      serverRequestHandling (fun server request -> server.TextDocumentColorPresentation(request))
      "textDocument/foldingRange",
      serverRequestHandling (fun server request -> server.TextDocumentFoldingRange(request))
      "textDocument/declaration", serverRequestHandling (fun server request -> server.TextDocumentDeclaration(request))
      "textDocument/selectionRange",
      serverRequestHandling (fun server request -> server.TextDocumentSelectionRange(request))
      "textDocument/prepareCallHierarchy",
      serverRequestHandling (fun server request -> server.TextDocumentPrepareCallHierarchy(request))
      "callHierarchy/incomingCalls",
      serverRequestHandling (fun server request -> server.CallHierarchyIncomingCalls(request))
      "callHierarchy/outgoingCalls",
      serverRequestHandling (fun server request -> server.CallHierarchyOutgoingCalls(request))
      "textDocument/semanticTokens/full",
      serverRequestHandling (fun server request -> server.TextDocumentSemanticTokensFull(request))
      "textDocument/semanticTokens/full/delta",
      serverRequestHandling (fun server request -> server.TextDocumentSemanticTokensFullDelta(request))
      "textDocument/semanticTokens/range",
      serverRequestHandling (fun server request -> server.TextDocumentSemanticTokensRange(request))
      "textDocument/linkedEditingRange",
      serverRequestHandling (fun server request -> server.TextDocumentLinkedEditingRange(request))
      "workspace/willCreateFiles",
      serverRequestHandling (fun server request -> server.WorkspaceWillCreateFiles(request))
      "workspace/willRenameFiles",
      serverRequestHandling (fun server request -> server.WorkspaceWillRenameFiles(request))
      "workspace/willDeleteFiles",
      serverRequestHandling (fun server request -> server.WorkspaceWillDeleteFiles(request))
      "textDocument/moniker", serverRequestHandling (fun server request -> server.TextDocumentMoniker(request))
      "textDocument/prepareTypeHierarchy",
      serverRequestHandling (fun server request -> server.TextDocumentPrepareTypeHierarchy(request))
      "typeHierarchy/supertypes", serverRequestHandling (fun server request -> server.TypeHierarchySupertypes(request))
      "typeHierarchy/subtypes", serverRequestHandling (fun server request -> server.TypeHierarchySubtypes(request))
      "textDocument/inlineValue", serverRequestHandling (fun server request -> server.TextDocumentInlineValue(request))
      "textDocument/inlayHint", serverRequestHandling (fun server request -> server.TextDocumentInlayHint(request))
      "inlayHint/resolve", serverRequestHandling (fun server request -> server.InlayHintResolve(request))
      "textDocument/diagnostic", serverRequestHandling (fun server request -> server.TextDocumentDiagnostic(request))
      "workspace/diagnostic", serverRequestHandling (fun server request -> server.WorkspaceDiagnostic(request))
      "initialize", serverRequestHandling (fun server request -> server.Initialize(request))
      "shutdown", serverRequestHandling (fun server request -> server.Shutdown())
      "textDocument/willSaveWaitUntil",
      serverRequestHandling (fun server request -> server.TextDocumentWillSaveWaitUntil(request))
      "textDocument/completion", serverRequestHandling (fun server request -> server.TextDocumentCompletion(request))
      "completionItem/resolve", serverRequestHandling (fun server request -> server.CompletionItemResolve(request))
      "textDocument/hover", serverRequestHandling (fun server request -> server.TextDocumentHover(request))
      "textDocument/signatureHelp",
      serverRequestHandling (fun server request -> server.TextDocumentSignatureHelp(request))
      "textDocument/definition", serverRequestHandling (fun server request -> server.TextDocumentDefinition(request))
      "textDocument/references", serverRequestHandling (fun server request -> server.TextDocumentReferences(request))
      "textDocument/documentHighlight",
      serverRequestHandling (fun server request -> server.TextDocumentDocumentHighlight(request))
      "textDocument/documentSymbol",
      serverRequestHandling (fun server request -> server.TextDocumentDocumentSymbol(request))
      "textDocument/codeAction", serverRequestHandling (fun server request -> server.TextDocumentCodeAction(request))
      "codeAction/resolve", serverRequestHandling (fun server request -> server.CodeActionResolve(request))
      "workspace/symbol", serverRequestHandling (fun server request -> server.WorkspaceSymbol(request))
      "workspaceSymbol/resolve", serverRequestHandling (fun server request -> server.WorkspaceSymbolResolve(request))
      "textDocument/codeLens", serverRequestHandling (fun server request -> server.TextDocumentCodeLens(request))
      "codeLens/resolve", serverRequestHandling (fun server request -> server.CodeLensResolve(request))
      "textDocument/documentLink",
      serverRequestHandling (fun server request -> server.TextDocumentDocumentLink(request))
      "documentLink/resolve", serverRequestHandling (fun server request -> server.DocumentLinkResolve(request))
      "textDocument/formatting", serverRequestHandling (fun server request -> server.TextDocumentFormatting(request))
      "textDocument/rangeFormatting",
      serverRequestHandling (fun server request -> server.TextDocumentRangeFormatting(request))
      "textDocument/onTypeFormatting",
      serverRequestHandling (fun server request -> server.TextDocumentOnTypeFormatting(request))
      "textDocument/rename", serverRequestHandling (fun server request -> server.TextDocumentRename(request))
      "textDocument/prepareRename",
      serverRequestHandling (fun server request -> server.TextDocumentPrepareRename(request))
      "workspace/executeCommand", serverRequestHandling (fun server request -> server.WorkspaceExecuteCommand(request))
      "workspace/didChangeWorkspaceFolders",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidChangeWorkspaceFolders(request)
        |> Requests.notificationSuccess
      )
      "window/workDoneProgress/cancel",
      serverRequestHandling (fun server request ->
        server.WindowWorkDoneProgressCancel(request)
        |> Requests.notificationSuccess
      )
      "workspace/didCreateFiles",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidCreateFiles(request)
        |> Requests.notificationSuccess
      )
      "workspace/didRenameFiles",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidRenameFiles(request)
        |> Requests.notificationSuccess
      )
      "workspace/didDeleteFiles",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidDeleteFiles(request)
        |> Requests.notificationSuccess
      )
      "notebookDocument/didOpen",
      serverRequestHandling (fun server request ->
        server.NotebookDocumentDidOpen(request)
        |> Requests.notificationSuccess
      )
      "notebookDocument/didChange",
      serverRequestHandling (fun server request ->
        server.NotebookDocumentDidChange(request)
        |> Requests.notificationSuccess
      )
      "notebookDocument/didSave",
      serverRequestHandling (fun server request ->
        server.NotebookDocumentDidSave(request)
        |> Requests.notificationSuccess
      )
      "notebookDocument/didClose",
      serverRequestHandling (fun server request ->
        server.NotebookDocumentDidClose(request)
        |> Requests.notificationSuccess
      )
      "initialized",
      serverRequestHandling (fun server request ->
        server.Initialized(request)
        |> Requests.notificationSuccess
      )
      "exit",
      serverRequestHandling (fun server request ->
        server.Exit()
        |> Requests.notificationSuccess
      )
      "workspace/didChangeConfiguration",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidChangeConfiguration(request)
        |> Requests.notificationSuccess
      )
      "textDocument/didOpen",
      serverRequestHandling (fun server request ->
        server.TextDocumentDidOpen(request)
        |> Requests.notificationSuccess
      )
      "textDocument/didChange",
      serverRequestHandling (fun server request ->
        server.TextDocumentDidChange(request)
        |> Requests.notificationSuccess
      )
      "textDocument/didClose",
      serverRequestHandling (fun server request ->
        server.TextDocumentDidClose(request)
        |> Requests.notificationSuccess
      )
      "textDocument/didSave",
      serverRequestHandling (fun server request ->
        server.TextDocumentDidSave(request)
        |> Requests.notificationSuccess
      )
      "textDocument/willSave",
      serverRequestHandling (fun server request ->
        server.TextDocumentWillSave(request)
        |> Requests.notificationSuccess
      )
      "workspace/didChangeWatchedFiles",
      serverRequestHandling (fun server request ->
        server.WorkspaceDidChangeWatchedFiles(request)
        |> Requests.notificationSuccess
      )
      "$/setTrace",
      serverRequestHandling (fun server request ->
        server.SetTrace(request)
        |> Requests.notificationSuccess
      )
      "$/cancelRequest",
      serverRequestHandling (fun server request ->
        server.CancelRequest(request)
        |> Requests.notificationSuccess
      )
      "$/progress",
      serverRequestHandling (fun server request ->
        server.Progress(request)
        |> Requests.notificationSuccess
      )
    ]