namespace rec Ionide.LanguageServerProtocol.Types

open System
open System.Runtime.Serialization
open System.Diagnostics
open Newtonsoft.Json
open Newtonsoft.Json.Linq
/// URI's are transferred as strings. The URI's format is defined in https://tools.ietf.org/html/rfc3986
///
/// See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
type URI = string
/// URI's are transferred as strings. The URI's format is defined in https://tools.ietf.org/html/rfc3986
///
/// See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
type DocumentUri = string
/// Regular expressions are transferred as strings.
///
/// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#regExp
type RegExp = string

type IWorkDoneProgressOptions =
  abstract WorkDoneProgress: bool option

type IWorkDoneProgressParams =
  /// An optional token that a server can use to report work done progress.
  abstract WorkDoneToken: ProgressToken option

type IPartialResultParams =
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  abstract PartialResultToken: ProgressToken option

/// A parameter literal used in requests to pass a text document and a position inside that
/// document.
type ITextDocumentPositionParams =
  /// The text document.
  abstract TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  abstract Position: Position

/// General text document registration options.
type ITextDocumentRegistrationOptions =
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  abstract DocumentSelector: DocumentSelector option

type IImplementationOptions =
  inherit IWorkDoneProgressOptions

type ITypeDefinitionOptions =
  inherit IWorkDoneProgressOptions

type IDocumentColorOptions =
  inherit IWorkDoneProgressOptions

type IFoldingRangeOptions =
  inherit IWorkDoneProgressOptions

type IDeclarationOptions =
  inherit IWorkDoneProgressOptions

type ISelectionRangeOptions =
  inherit IWorkDoneProgressOptions

/// Call hierarchy options used during static registration.
///
/// @since 3.16.0
type ICallHierarchyOptions =
  inherit IWorkDoneProgressOptions

/// @since 3.16.0
type ISemanticTokensOptions =
  /// The legend used by the server
  abstract Legend: SemanticTokensLegend
  /// Server supports providing semantic tokens for a specific range
  /// of a document.
  abstract Range: U2<bool, JToken> option
  /// Server supports providing semantic tokens for a full document.
  abstract Full: U2<bool, SemanticTokensOptionsFullC2> option
  inherit IWorkDoneProgressOptions

type ILinkedEditingRangeOptions =
  inherit IWorkDoneProgressOptions

type IMonikerOptions =
  inherit IWorkDoneProgressOptions

/// Type hierarchy options used during static registration.
///
/// @since 3.17.0
type ITypeHierarchyOptions =
  inherit IWorkDoneProgressOptions

/// Inline value options used during static registration.
///
/// @since 3.17.0
type IInlineValueOptions =
  inherit IWorkDoneProgressOptions

/// Inlay hint options used during static registration.
///
/// @since 3.17.0
type IInlayHintOptions =
  /// The server provides support to resolve additional
  /// information for an inlay hint item.
  abstract ResolveProvider: bool option
  inherit IWorkDoneProgressOptions

/// Diagnostic options.
///
/// @since 3.17.0
type IDiagnosticOptions =
  /// An optional identifier under which the diagnostics are
  /// managed by the client.
  abstract Identifier: string option
  /// Whether the language has inter file dependencies meaning that
  /// editing code in one file can result in a different diagnostic
  /// set in another file. Inter file dependencies are common for
  /// most programming languages and typically uncommon for linters.
  abstract InterFileDependencies: bool
  /// The server provides support for workspace diagnostics as well.
  abstract WorkspaceDiagnostics: bool
  inherit IWorkDoneProgressOptions

/// The initialize parameters
type I_InitializeParams =
  /// The process Id of the parent process that started
  /// the server.
  ///
  /// Is `null` if the process has not been started by another process.
  /// If the parent process is not alive then the server should exit.
  abstract ProcessId: int32 option
  /// Information about the client
  ///
  /// @since 3.15.0
  abstract ClientInfo: _InitializeParamsClientInfo option
  /// The locale the client is currently showing the user interface
  /// in. This must not necessarily be the locale of the operating
  /// system.
  ///
  /// Uses IETF language tags as the value's syntax
  /// (See https://en.wikipedia.org/wiki/IETF_language_tag)
  ///
  /// @since 3.16.0
  abstract Locale: string option
  /// The rootPath of the workspace. Is null
  /// if no folder is open.
  ///
  /// @deprecated in favour of rootUri.
  abstract RootPath: string option
  /// The rootUri of the workspace. Is null if no
  /// folder is open. If both `rootPath` and `rootUri` are set
  /// `rootUri` wins.
  ///
  /// @deprecated in favour of workspaceFolders.
  abstract RootUri: DocumentUri option
  /// The capabilities provided by the client (editor or tool)
  abstract Capabilities: ClientCapabilities
  /// User provided initialization options.
  abstract InitializationOptions: LSPAny option
  /// The initial trace setting. If omitted trace is disabled ('off').
  abstract Trace: TraceValues option
  inherit IWorkDoneProgressParams

type IWorkspaceFoldersInitializeParams =
  /// The workspace folders configured in the client when the server starts.
  ///
  /// This property is only available if the client supports workspace folders.
  /// It can be `null` if the client supports workspace folders but none are
  /// configured.
  ///
  /// @since 3.6.0
  abstract WorkspaceFolders: WorkspaceFolder[] option

/// Save options.
type ISaveOptions =
  /// The client is supposed to include the content on save.
  abstract IncludeText: bool option

/// Completion options.
type ICompletionOptions =
  /// Most tools trigger completion request automatically without explicitly requesting
  /// it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
  /// starts to type an identifier. For example if the user types `c` in a JavaScript file
  /// code complete will automatically pop up present `console` besides others as a
  /// completion item. Characters that make up identifiers don't need to be listed here.
  ///
  /// If code complete should automatically be trigger on characters not being valid inside
  /// an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
  abstract TriggerCharacters: string[] option
  /// The list of all possible characters that commit a completion. This field can be used
  /// if clients don't support individual commit characters per completion item. See
  /// `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
  ///
  /// If a server provides both `allCommitCharacters` and commit characters on an individual
  /// completion item the ones on the completion item win.
  ///
  /// @since 3.2.0
  abstract AllCommitCharacters: string[] option
  /// The server provides support to resolve additional
  /// information for a completion item.
  abstract ResolveProvider: bool option
  /// The server supports the following `CompletionItem` specific
  /// capabilities.
  ///
  /// @since 3.17.0
  abstract CompletionItem: CompletionOptionsCompletionItem option
  inherit IWorkDoneProgressOptions

/// Hover options.
type IHoverOptions =
  inherit IWorkDoneProgressOptions

/// Server Capabilities for a {@link SignatureHelpRequest}.
type ISignatureHelpOptions =
  /// List of characters that trigger signature help automatically.
  abstract TriggerCharacters: string[] option
  /// List of characters that re-trigger signature help.
  ///
  /// These trigger characters are only active when signature help is already showing. All trigger characters
  /// are also counted as re-trigger characters.
  ///
  /// @since 3.15.0
  abstract RetriggerCharacters: string[] option
  inherit IWorkDoneProgressOptions

/// Server Capabilities for a {@link DefinitionRequest}.
type IDefinitionOptions =
  inherit IWorkDoneProgressOptions

/// Reference options.
type IReferenceOptions =
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link DocumentHighlightRequest}.
type IDocumentHighlightOptions =
  inherit IWorkDoneProgressOptions

/// A base for all symbol information.
type IBaseSymbolInformation =
  /// The name of this symbol.
  abstract Name: string
  /// The kind of this symbol.
  abstract Kind: SymbolKind
  /// Tags for this symbol.
  ///
  /// @since 3.16.0
  abstract Tags: SymbolTag[] option
  /// The name of the symbol containing this symbol. This information is for
  /// user interface purposes (e.g. to render a qualifier in the user interface
  /// if necessary). It can't be used to re-infer a hierarchy for the document
  /// symbols.
  abstract ContainerName: string option

/// Provider options for a {@link DocumentSymbolRequest}.
type IDocumentSymbolOptions =
  /// A human-readable string that is shown when multiple outlines trees
  /// are shown for the same document.
  ///
  /// @since 3.16.0
  abstract Label: string option
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link CodeActionRequest}.
type ICodeActionOptions =
  /// CodeActionKinds that this server may return.
  ///
  /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  /// may list out every specific kind they provide.
  abstract CodeActionKinds: CodeActionKind[] option
  /// The server provides support to resolve additional
  /// information for a code action.
  ///
  /// @since 3.16.0
  abstract ResolveProvider: bool option
  inherit IWorkDoneProgressOptions

/// Server capabilities for a {@link WorkspaceSymbolRequest}.
type IWorkspaceSymbolOptions =
  /// The server provides support to resolve additional
  /// information for a workspace symbol.
  ///
  /// @since 3.17.0
  abstract ResolveProvider: bool option
  inherit IWorkDoneProgressOptions

/// Code Lens provider options of a {@link CodeLensRequest}.
type ICodeLensOptions =
  /// Code lens has a resolve provider as well.
  abstract ResolveProvider: bool option
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link DocumentLinkRequest}.
type IDocumentLinkOptions =
  /// Document links have a resolve provider as well.
  abstract ResolveProvider: bool option
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link DocumentFormattingRequest}.
type IDocumentFormattingOptions =
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link DocumentRangeFormattingRequest}.
type IDocumentRangeFormattingOptions =
  inherit IWorkDoneProgressOptions

/// Provider options for a {@link DocumentOnTypeFormattingRequest}.
type IDocumentOnTypeFormattingOptions =
  /// A character on which formatting should be triggered, like `{`.
  abstract FirstTriggerCharacter: string
  /// More trigger characters.
  abstract MoreTriggerCharacter: string[] option

/// Provider options for a {@link RenameRequest}.
type IRenameOptions =
  /// Renames should be checked and tested before being executed.
  ///
  /// @since version 3.12.0
  abstract PrepareProvider: bool option
  inherit IWorkDoneProgressOptions

/// The server capabilities of a {@link ExecuteCommandRequest}.
type IExecuteCommandOptions =
  /// The commands to be executed on the server
  abstract Commands: string[]
  inherit IWorkDoneProgressOptions

/// A generic resource operation.
type IResourceOperation =
  /// The resource operation kind.
  abstract Kind: string
  /// An optional annotation identifier describing the operation.
  ///
  /// @since 3.16.0
  abstract AnnotationId: ChangeAnnotationIdentifier option

/// A diagnostic report with a full set of problems.
///
/// @since 3.17.0
type IFullDocumentDiagnosticReport =
  /// A full document diagnostic report.
  abstract Kind: string
  /// An optional result id. If provided it will
  /// be sent on the next diagnostic request for the
  /// same document.
  abstract ResultId: string option
  /// The actual items.
  abstract Items: Diagnostic[]

/// A diagnostic report indicating that the last returned
/// report is still accurate.
///
/// @since 3.17.0
type IUnchangedDocumentDiagnosticReport =
  /// A document diagnostic report indicating
  /// no changes to the last result. A server can
  /// only return `unchanged` if result ids are
  /// provided.
  abstract Kind: string
  /// A result id which will be sent on the next
  /// diagnostic request for the same document.
  abstract ResultId: string

/// A literal to identify a text document in the client.
type ITextDocumentIdentifier =
  /// The text document's uri.
  abstract Uri: DocumentUri

/// A text edit applicable to a text document.
type ITextEdit =
  /// The range of the text document to be manipulated. To insert
  /// text into a document create a range where start === end.
  abstract Range: Range
  /// The string to be inserted. For delete operations use an
  /// empty string.
  abstract NewText: string

/// Options specific to a notebook plus its cells
/// to be synced to the server.
///
/// If a selector provides a notebook document
/// filter but no cell selector all cells of a
/// matching notebook document will be synced.
///
/// If a selector provides no notebook document
/// filter but only a cell selector all notebook
/// document that contain at least one matching
/// cell will be synced.
///
/// @since 3.17.0
type INotebookDocumentSyncOptions =
  /// The notebooks to be synced
  abstract NotebookSelector: NotebookDocumentSyncOptionsNotebookSelector[]
  /// Whether save notification should be forwarded to
  /// the server. Will only be honored if mode === `notebook`.
  abstract Save: bool option

type InitializedParams = obj

type ImplementationParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents a location inside a resource, such as a line
/// inside a text file.
type Location = { Uri: DocumentUri; Range: Range }

type ImplementationRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IImplementationOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

type TypeDefinitionParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

type TypeDefinitionRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ITypeDefinitionOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// A workspace folder inside a client.
type WorkspaceFolder = {
  /// The associated URI for this workspace folder.
  Uri: URI
  /// The name of the workspace folder. Used to refer to this
  /// workspace folder in the user interface.
  Name: string
}

/// The parameters of a `workspace/didChangeWorkspaceFolders` notification.
type DidChangeWorkspaceFoldersParams = {
  /// The actual workspace folder change event.
  Event: WorkspaceFoldersChangeEvent
}

/// The parameters of a configuration request.
type ConfigurationParams = { Items: ConfigurationItem[] }

/// Parameters for a {@link DocumentColorRequest}.
type DocumentColorParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents a color range from a document.
type ColorInformation = {
  /// The range in the document where this color appears.
  Range: Range
  /// The actual color value for this color range.
  Color: Color
}

type DocumentColorRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentColorOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link ColorPresentationRequest}.
type ColorPresentationParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The color to request presentations for.
  Color: Color
  /// The range where the color would be inserted. Serves as a context.
  Range: Range
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

type ColorPresentation = {
  /// The label of this color presentation. It will be shown on the color
  /// picker header. By default this is also the text that is inserted when selecting
  /// this color presentation.
  Label: string
  /// An {@link TextEdit edit} which is applied to a document when selecting
  /// this presentation for the color.  When `falsy` the {@link ColorPresentation.label label}
  /// is used.
  TextEdit: TextEdit option
  /// An optional array of additional {@link TextEdit text edits} that are applied when
  /// selecting this color presentation. Edits must not overlap with the main {@link ColorPresentation.textEdit edit} nor with themselves.
  AdditionalTextEdits: TextEdit[] option
}

type WorkDoneProgressOptions = {
  WorkDoneProgress: bool option
} with

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// General text document registration options.
type TextDocumentRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

/// Parameters for a {@link FoldingRangeRequest}.
type FoldingRangeParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents a folding range. To be valid, start and end line must be bigger than zero and smaller
/// than the number of lines in the document. Clients are free to ignore invalid ranges.
type FoldingRange = {
  /// The zero-based start line of the range to fold. The folded area starts after the line's last character.
  /// To be valid, the end must be zero or larger and smaller than the number of lines in the document.
  StartLine: uint32
  /// The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
  StartCharacter: uint32 option
  /// The zero-based end line of the range to fold. The folded area ends with the line's last character.
  /// To be valid, the end must be zero or larger and smaller than the number of lines in the document.
  EndLine: uint32
  /// The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
  EndCharacter: uint32 option
  /// Describes the kind of the folding range such as `comment' or 'region'. The kind
  /// is used to categorize folding ranges and used by commands like 'Fold all comments'.
  /// See {@link FoldingRangeKind} for an enumeration of standardized kinds.
  Kind: FoldingRangeKind option
  /// The text that the client should show when the specified range is
  /// collapsed. If not defined or not supported by the client, a default
  /// will be chosen by the client.
  ///
  /// @since 3.17.0
  CollapsedText: string option
}

type FoldingRangeRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IFoldingRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

type DeclarationParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

type DeclarationRegistrationOptions = {
  WorkDoneProgress: bool option
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface IDeclarationOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

/// A parameter literal used in selection range requests.
type SelectionRangeParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The positions inside the text document.
  Positions: Position[]
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A selection range represents a part of a selection hierarchy. A selection range
/// may have a parent selection range that contains it.
type SelectionRange = {
  /// The {@link Range range} of this selection range.
  Range: Range
  /// The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
  Parent: SelectionRange option
}

type SelectionRangeRegistrationOptions = {
  WorkDoneProgress: bool option
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ISelectionRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

type WorkDoneProgressCreateParams = {
  /// The token to be used to report progress.
  Token: ProgressToken
}

type WorkDoneProgressCancelParams = {
  /// The token to be used to report progress.
  Token: ProgressToken
}

/// The parameter of a `textDocument/prepareCallHierarchy` request.
///
/// @since 3.16.0
type CallHierarchyPrepareParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Represents programming constructs like functions or constructors in the context
/// of call hierarchy.
///
/// @since 3.16.0
type CallHierarchyItem = {
  /// The name of this item.
  Name: string
  /// The kind of this item.
  Kind: SymbolKind
  /// Tags for this item.
  Tags: SymbolTag[] option
  /// More detail for this item, e.g. the signature of a function.
  Detail: string option
  /// The resource identifier of this item.
  Uri: DocumentUri
  /// The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.
  Range: Range
  /// The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
  /// Must be contained by the {@link CallHierarchyItem.range `range`}.
  SelectionRange: Range
  /// A data entry field that is preserved between a call hierarchy prepare and
  /// incoming calls or outgoing calls requests.
  Data: LSPAny option
}

/// Call hierarchy options used during static or dynamic registration.
///
/// @since 3.16.0
type CallHierarchyRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ICallHierarchyOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameter of a `callHierarchy/incomingCalls` request.
///
/// @since 3.16.0
type CallHierarchyIncomingCallsParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  Item: CallHierarchyItem
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents an incoming call, e.g. a caller of a method or constructor.
///
/// @since 3.16.0
type CallHierarchyIncomingCall = {
  /// The item that makes the call.
  From: CallHierarchyItem
  /// The ranges at which the calls appear. This is relative to the caller
  /// denoted by {@link CallHierarchyIncomingCall.from `this.from`}.
  FromRanges: Range[]
}

/// The parameter of a `callHierarchy/outgoingCalls` request.
///
/// @since 3.16.0
type CallHierarchyOutgoingCallsParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  Item: CallHierarchyItem
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.
///
/// @since 3.16.0
type CallHierarchyOutgoingCall = {
  /// The item that is called.
  To: CallHierarchyItem
  /// The range at which this item is called. This is the range relative to the caller, e.g the item
  /// passed to {@link CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls `provideCallHierarchyOutgoingCalls`}
  /// and not {@link CallHierarchyOutgoingCall.to `this.to`}.
  FromRanges: Range[]
}

/// @since 3.16.0
type SemanticTokensParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// @since 3.16.0
type SemanticTokens = {
  /// An optional result id. If provided and clients support delta updating
  /// the client will include the result id in the next semantic token request.
  /// A server can then instead of computing all semantic tokens again simply
  /// send a delta.
  ResultId: string option
  /// The actual tokens.
  Data: uint32[]
}

/// @since 3.16.0
type SemanticTokensPartialResult = { Data: uint32[] }

type SemanticTokensOptionsFullC2 = {
  /// The server supports deltas for full documents.
  Delta: bool option
}

/// @since 3.16.0
type SemanticTokensRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The legend used by the server
  Legend: SemanticTokensLegend
  /// Server supports providing semantic tokens for a specific range
  /// of a document.
  Range: U2<bool, JToken> option
  /// Server supports providing semantic tokens for a full document.
  Full: U2<bool, SemanticTokensOptionsFullC2> option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ISemanticTokensOptions with
    /// The legend used by the server
    member x.Legend = x.Legend
    /// Server supports providing semantic tokens for a specific range
    /// of a document.
    member x.Range = x.Range
    /// Server supports providing semantic tokens for a full document.
    member x.Full = x.Full

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// @since 3.16.0
type SemanticTokensDeltaParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The result id of a previous response. The result Id can either point to a full response
  /// or a delta response depending on what was received last.
  PreviousResultId: string
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// @since 3.16.0
type SemanticTokensDelta = {
  ResultId: string option
  /// The semantic token edits to transform a previous result into a new result.
  Edits: SemanticTokensEdit[]
}

/// @since 3.16.0
type SemanticTokensDeltaPartialResult = { Edits: SemanticTokensEdit[] }

/// @since 3.16.0
type SemanticTokensRangeParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The range the semantic tokens are requested for.
  Range: Range
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Params to show a resource in the UI.
///
/// @since 3.16.0
type ShowDocumentParams = {
  /// The uri to show.
  Uri: URI
  /// Indicates to show the resource in an external program.
  /// To show, for example, `https://code.visualstudio.com/`
  /// in the default WEB browser set `external` to `true`.
  External: bool option
  /// An optional property to indicate whether the editor
  /// showing the document should take focus or not.
  /// Clients might ignore this property if an external
  /// program is started.
  TakeFocus: bool option
  /// An optional selection range if the document is a text
  /// document. Clients might ignore the property if an
  /// external program is started or the file is not a text
  /// file.
  Selection: Range option
}

/// The result of a showDocument request.
///
/// @since 3.16.0
type ShowDocumentResult = {
  /// A boolean indicating if the show was successful.
  Success: bool
}

type LinkedEditingRangeParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// The result of a linked editing range request.
///
/// @since 3.16.0
type LinkedEditingRanges = {
  /// A list of ranges that can be edited together. The ranges must have
  /// identical length and contain identical text content. The ranges cannot overlap.
  Ranges: Range[]
  /// An optional word pattern (regular expression) that describes valid contents for
  /// the given ranges. If no pattern is provided, the client configuration's word
  /// pattern will be used.
  WordPattern: string option
}

type LinkedEditingRangeRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ILinkedEditingRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters sent in notifications/requests for user-initiated creation of
/// files.
///
/// @since 3.16.0
type CreateFilesParams = {
  /// An array of all files/folders created in this operation.
  Files: FileCreate[]
}

/// A workspace edit represents changes to many resources managed in the workspace. The edit
/// should either provide `changes` or `documentChanges`. If documentChanges are present
/// they are preferred over `changes` if the client can handle versioned document edits.
///
/// Since version 3.13.0 a workspace edit can contain resource operations as well. If resource
/// operations are present clients need to execute the operations in the order in which they
/// are provided. So a workspace edit for example can consist of the following two changes:
/// (1) a create file a.txt and (2) a text document edit which insert text into file a.txt.
///
/// An invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will
/// cause failure of the operation. How the client recovers from the failure is described by
/// the client capability: `workspace.workspaceEdit.failureHandling`
type WorkspaceEdit = {
  /// Holds changes to existing resources.
  Changes: Map<DocumentUri, TextEdit[]> option
  /// Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
  /// are either an array of `TextDocumentEdit`s to express changes to n different text documents
  /// where each text document edit addresses a specific version of a text document. Or it can contain
  /// above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.
  ///
  /// Whether a client supports versioned document edits is expressed via
  /// `workspace.workspaceEdit.documentChanges` client capability.
  ///
  /// If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
  /// only plain `TextEdit`s using the `changes` property are supported.
  DocumentChanges: U4<TextDocumentEdit, CreateFile, RenameFile, DeleteFile>[] option
  /// A map of change annotations that can be referenced in `AnnotatedTextEdit`s or create, rename and
  /// delete file / folder operations.
  ///
  /// Whether clients honor this property depends on the client capability `workspace.changeAnnotationSupport`.
  ///
  /// @since 3.16.0
  ChangeAnnotations: Map<ChangeAnnotationIdentifier, ChangeAnnotation> option
}

/// The options to register for file operations.
///
/// @since 3.16.0
type FileOperationRegistrationOptions = {
  /// The actual filters.
  Filters: FileOperationFilter[]
}

/// The parameters sent in notifications/requests for user-initiated renames of
/// files.
///
/// @since 3.16.0
type RenameFilesParams = {
  /// An array of all files/folders renamed in this operation. When a folder is renamed, only
  /// the folder will be included, and not its children.
  Files: FileRename[]
}

/// The parameters sent in notifications/requests for user-initiated deletes of
/// files.
///
/// @since 3.16.0
type DeleteFilesParams = {
  /// An array of all files/folders deleted in this operation.
  Files: FileDelete[]
}

type MonikerParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Moniker definition to match LSIF 0.5 moniker definition.
///
/// @since 3.16.0
type Moniker = {
  /// The scheme of the moniker. For example tsc or .Net
  Scheme: string
  /// The identifier of the moniker. The value is opaque in LSIF however
  /// schema owners are allowed to define the structure if they want.
  Identifier: string
  /// The scope in which the moniker is unique
  Unique: UniquenessLevel
  /// The moniker kind if known.
  Kind: MonikerKind option
}

type MonikerRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IMonikerOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameter of a `textDocument/prepareTypeHierarchy` request.
///
/// @since 3.17.0
type TypeHierarchyPrepareParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// @since 3.17.0
type TypeHierarchyItem = {
  /// The name of this item.
  Name: string
  /// The kind of this item.
  Kind: SymbolKind
  /// Tags for this item.
  Tags: SymbolTag[] option
  /// More detail for this item, e.g. the signature of a function.
  Detail: string option
  /// The resource identifier of this item.
  Uri: DocumentUri
  /// The range enclosing this symbol not including leading/trailing whitespace
  /// but everything else, e.g. comments and code.
  Range: Range
  /// The range that should be selected and revealed when this symbol is being
  /// picked, e.g. the name of a function. Must be contained by the
  /// {@link TypeHierarchyItem.range `range`}.
  SelectionRange: Range
  /// A data entry field that is preserved between a type hierarchy prepare and
  /// supertypes or subtypes requests. It could also be used to identify the
  /// type hierarchy in the server, helping improve the performance on
  /// resolving supertypes and subtypes.
  Data: LSPAny option
}

/// Type hierarchy options used during static or dynamic registration.
///
/// @since 3.17.0
type TypeHierarchyRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ITypeHierarchyOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameter of a `typeHierarchy/supertypes` request.
///
/// @since 3.17.0
type TypeHierarchySupertypesParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  Item: TypeHierarchyItem
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// The parameter of a `typeHierarchy/subtypes` request.
///
/// @since 3.17.0
type TypeHierarchySubtypesParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  Item: TypeHierarchyItem
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A parameter literal used in inline value requests.
///
/// @since 3.17.0
type InlineValueParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The document range for which inline values should be computed.
  Range: Range
  /// Additional information about the context in which inline values were
  /// requested.
  Context: InlineValueContext
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Inline value options used during static or dynamic registration.
///
/// @since 3.17.0
type InlineValueRegistrationOptions = {
  WorkDoneProgress: bool option
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface IInlineValueOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

/// A parameter literal used in inlay hint requests.
///
/// @since 3.17.0
type InlayHintParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The document range for which inlay hints should be computed.
  Range: Range
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Inlay hint information.
///
/// @since 3.17.0
type InlayHint = {
  /// The position of this hint.
  ///
  /// If multiple hints have the same position, they will be shown in the order
  /// they appear in the response.
  Position: Position
  /// The label of this hint. A human readable string or an array of
  /// InlayHintLabelPart label parts.
  ///
  /// *Note* that neither the string nor the label part can be empty.
  Label: U2<string, InlayHintLabelPart[]>
  /// The kind of this hint. Can be omitted in which case the client
  /// should fall back to a reasonable default.
  Kind: InlayHintKind option
  /// Optional text edits that are performed when accepting this inlay hint.
  ///
  /// *Note* that edits are expected to change the document so that the inlay
  /// hint (or its nearest variant) is now part of the document and the inlay
  /// hint itself is now obsolete.
  TextEdits: TextEdit[] option
  /// The tooltip text when you hover over this item.
  Tooltip: U2<string, MarkupContent> option
  /// Render padding before the hint.
  ///
  /// Note: Padding should use the editor's background color, not the
  /// background color of the hint itself. That means padding can be used
  /// to visually align/separate an inlay hint.
  PaddingLeft: bool option
  /// Render padding after the hint.
  ///
  /// Note: Padding should use the editor's background color, not the
  /// background color of the hint itself. That means padding can be used
  /// to visually align/separate an inlay hint.
  PaddingRight: bool option
  /// A data entry field that is preserved on an inlay hint between
  /// a `textDocument/inlayHint` and a `inlayHint/resolve` request.
  Data: LSPAny option
}

/// Inlay hint options used during static or dynamic registration.
///
/// @since 3.17.0
type InlayHintRegistrationOptions = {
  WorkDoneProgress: bool option
  /// The server provides support to resolve additional
  /// information for an inlay hint item.
  ResolveProvider: bool option
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface IInlayHintOptions with
    /// The server provides support to resolve additional
    /// information for an inlay hint item.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

/// Parameters of the document diagnostic request.
///
/// @since 3.17.0
type DocumentDiagnosticParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The additional identifier  provided during registration.
  Identifier: string option
  /// The result id of a previous response if provided.
  PreviousResultId: string option
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A partial result for a document diagnostic report.
///
/// @since 3.17.0
type DocumentDiagnosticReportPartialResult = {
  RelatedDocuments: Map<DocumentUri, U2<FullDocumentDiagnosticReport, UnchangedDocumentDiagnosticReport>>
}

/// Cancellation data returned from a diagnostic request.
///
/// @since 3.17.0
type DiagnosticServerCancellationData = { RetriggerRequest: bool }

/// Diagnostic registration options.
///
/// @since 3.17.0
type DiagnosticRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// An optional identifier under which the diagnostics are
  /// managed by the client.
  Identifier: string option
  /// Whether the language has inter file dependencies meaning that
  /// editing code in one file can result in a different diagnostic
  /// set in another file. Inter file dependencies are common for
  /// most programming languages and typically uncommon for linters.
  InterFileDependencies: bool
  /// The server provides support for workspace diagnostics as well.
  WorkspaceDiagnostics: bool
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDiagnosticOptions with
    /// An optional identifier under which the diagnostics are
    /// managed by the client.
    member x.Identifier = x.Identifier
    /// Whether the language has inter file dependencies meaning that
    /// editing code in one file can result in a different diagnostic
    /// set in another file. Inter file dependencies are common for
    /// most programming languages and typically uncommon for linters.
    member x.InterFileDependencies = x.InterFileDependencies
    /// The server provides support for workspace diagnostics as well.
    member x.WorkspaceDiagnostics = x.WorkspaceDiagnostics

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters of the workspace diagnostic request.
///
/// @since 3.17.0
type WorkspaceDiagnosticParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The additional identifier provided during registration.
  Identifier: string option
  /// The currently known diagnostic reports with their
  /// previous result ids.
  PreviousResultIds: PreviousResultId[]
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A workspace diagnostic report.
///
/// @since 3.17.0
type WorkspaceDiagnosticReport = { Items: WorkspaceDocumentDiagnosticReport[] }
/// A partial result for a workspace diagnostic report.
///
/// @since 3.17.0
type WorkspaceDiagnosticReportPartialResult = { Items: WorkspaceDocumentDiagnosticReport[] }

/// The params sent in an open notebook document notification.
///
/// @since 3.17.0
type DidOpenNotebookDocumentParams = {
  /// The notebook document that got opened.
  NotebookDocument: NotebookDocument
  /// The text documents that represent the content
  /// of a notebook cell.
  CellTextDocuments: TextDocumentItem[]
}

/// The params sent in a change notebook document notification.
///
/// @since 3.17.0
type DidChangeNotebookDocumentParams = {
  /// The notebook document that did change. The version number points
  /// to the version after all provided changes have been applied. If
  /// only the text document content of a cell changes the notebook version
  /// doesn't necessarily have to change.
  NotebookDocument: VersionedNotebookDocumentIdentifier
  /// The actual changes to the notebook document.
  ///
  /// The changes describe single state changes to the notebook document.
  /// So if there are two changes c1 (at array index 0) and c2 (at array
  /// index 1) for a notebook in state S then c1 moves the notebook from
  /// S to S' and c2 from S' to S''. So c1 is computed on the state S and
  /// c2 is computed on the state S'.
  ///
  /// To mirror the content of a notebook using change events use the following approach:
  /// - start with the same initial content
  /// - apply the 'notebookDocument/didChange' notifications in the order you receive them.
  /// - apply the `NotebookChangeEvent`s in a single notification in the order
  ///   you receive them.
  Change: NotebookDocumentChangeEvent
}

/// The params sent in a save notebook document notification.
///
/// @since 3.17.0
type DidSaveNotebookDocumentParams = {
  /// The notebook document that got saved.
  NotebookDocument: NotebookDocumentIdentifier
}

/// The params sent in a close notebook document notification.
///
/// @since 3.17.0
type DidCloseNotebookDocumentParams = {
  /// The notebook document that got closed.
  NotebookDocument: NotebookDocumentIdentifier
  /// The text documents that represent the content
  /// of a notebook cell that got closed.
  CellTextDocuments: TextDocumentIdentifier[]
}

type RegistrationParams = { Registrations: Registration[] }
type UnregistrationParams = { Unregisterations: Unregistration[] }

type _InitializeParamsClientInfo = {
  /// The name of the client as defined by the client.
  Name: string
  /// The client's version as defined by the client.
  Version: string option
}

type InitializeParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The process Id of the parent process that started
  /// the server.
  ///
  /// Is `null` if the process has not been started by another process.
  /// If the parent process is not alive then the server should exit.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  ProcessId: int32 option
  /// Information about the client
  ///
  /// @since 3.15.0
  ClientInfo: _InitializeParamsClientInfo option
  /// The locale the client is currently showing the user interface
  /// in. This must not necessarily be the locale of the operating
  /// system.
  ///
  /// Uses IETF language tags as the value's syntax
  /// (See https://en.wikipedia.org/wiki/IETF_language_tag)
  ///
  /// @since 3.16.0
  Locale: string option
  /// The rootPath of the workspace. Is null
  /// if no folder is open.
  ///
  /// @deprecated in favour of rootUri.
  RootPath: string option
  /// The rootUri of the workspace. Is null if no
  /// folder is open. If both `rootPath` and `rootUri` are set
  /// `rootUri` wins.
  ///
  /// @deprecated in favour of workspaceFolders.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  RootUri: DocumentUri option
  /// The capabilities provided by the client (editor or tool)
  Capabilities: ClientCapabilities
  /// User provided initialization options.
  InitializationOptions: LSPAny option
  /// The initial trace setting. If omitted trace is disabled ('off').
  Trace: TraceValues option
  /// The workspace folders configured in the client when the server starts.
  ///
  /// This property is only available if the client supports workspace folders.
  /// It can be `null` if the client supports workspace folders but none are
  /// configured.
  ///
  /// @since 3.6.0
  WorkspaceFolders: WorkspaceFolder[] option
} with

  interface I_InitializeParams with
    /// The process Id of the parent process that started
    /// the server.
    ///
    /// Is `null` if the process has not been started by another process.
    /// If the parent process is not alive then the server should exit.
    member x.ProcessId = x.ProcessId
    /// Information about the client
    ///
    /// @since 3.15.0
    member x.ClientInfo = x.ClientInfo
    /// The locale the client is currently showing the user interface
    /// in. This must not necessarily be the locale of the operating
    /// system.
    ///
    /// Uses IETF language tags as the value's syntax
    /// (See https://en.wikipedia.org/wiki/IETF_language_tag)
    ///
    /// @since 3.16.0
    member x.Locale = x.Locale
    /// The rootPath of the workspace. Is null
    /// if no folder is open.
    ///
    /// @deprecated in favour of rootUri.
    member x.RootPath = x.RootPath
    /// The rootUri of the workspace. Is null if no
    /// folder is open. If both `rootPath` and `rootUri` are set
    /// `rootUri` wins.
    ///
    /// @deprecated in favour of workspaceFolders.
    member x.RootUri = x.RootUri
    /// The capabilities provided by the client (editor or tool)
    member x.Capabilities = x.Capabilities
    /// User provided initialization options.
    member x.InitializationOptions = x.InitializationOptions
    /// The initial trace setting. If omitted trace is disabled ('off').
    member x.Trace = x.Trace

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IWorkspaceFoldersInitializeParams with
    /// The workspace folders configured in the client when the server starts.
    ///
    /// This property is only available if the client supports workspace folders.
    /// It can be `null` if the client supports workspace folders but none are
    /// configured.
    ///
    /// @since 3.6.0
    member x.WorkspaceFolders = x.WorkspaceFolders

type InitializeResultServerInfo = {
  /// The name of the server as defined by the server.
  Name: string
  /// The server's version as defined by the server.
  Version: string option
}

/// The result returned from an initialize request.
type InitializeResult = {
  /// The capabilities the language server provides.
  Capabilities: ServerCapabilities
  /// Information about the server.
  ///
  /// @since 3.15.0
  ServerInfo: InitializeResultServerInfo option
}

/// The data type of the ResponseError if the
/// initialize request fails.
type InitializeError = {
  /// Indicates whether the client execute the following retry logic:
  /// (1) show the message provided by the ResponseError to the user
  /// (2) user selects retry or cancel
  /// (3) if user selected retry the initialize method is sent again.
  Retry: bool
}

/// The parameters of a change configuration notification.
type DidChangeConfigurationParams = {
  /// The actual changed settings
  Settings: LSPAny
}

type DidChangeConfigurationRegistrationOptions = { Section: U2<string, string[]> option }

/// The parameters of a notification message.
type ShowMessageParams = {
  /// The message type. See {@link MessageType}
  Type: MessageType
  /// The actual message.
  Message: string
}

type ShowMessageRequestParams = {
  /// The message type. See {@link MessageType}
  Type: MessageType
  /// The actual message.
  Message: string
  /// The message action items to present.
  Actions: MessageActionItem[] option
}

type MessageActionItem = {
  /// A short title like 'Retry', 'Open Log' etc.
  Title: string
}

/// The log message parameters.
type LogMessageParams = {
  /// The message type. See {@link MessageType}
  Type: MessageType
  /// The actual message.
  Message: string
}

/// The parameters sent in an open text document notification
type DidOpenTextDocumentParams = {
  /// The document that was opened.
  TextDocument: TextDocumentItem
}

/// The change text document notification's parameters.
type DidChangeTextDocumentParams = {
  /// The document that did change. The version number points
  /// to the version after all provided content changes have
  /// been applied.
  TextDocument: VersionedTextDocumentIdentifier
  /// The actual content changes. The content changes describe single state changes
  /// to the document. So if there are two content changes c1 (at array index 0) and
  /// c2 (at array index 1) for a document in state S then c1 moves the document from
  /// S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
  /// on the state S'.
  ///
  /// To mirror the content of a document using change events use the following approach:
  /// - start with the same initial content
  /// - apply the 'textDocument/didChange' notifications in the order you receive them.
  /// - apply the `TextDocumentContentChangeEvent`s in a single notification in the order
  ///   you receive them.
  ContentChanges: TextDocumentContentChangeEvent[]
}

/// Describe options to be used when registered for text document change events.
type TextDocumentChangeRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// How documents are synced to the server.
  SyncKind: TextDocumentSyncKind
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

/// The parameters sent in a close text document notification
type DidCloseTextDocumentParams = {
  /// The document that was closed.
  TextDocument: TextDocumentIdentifier
}

/// The parameters sent in a save text document notification
type DidSaveTextDocumentParams = {
  /// The document that was saved.
  TextDocument: TextDocumentIdentifier
  /// Optional the content when saved. Depends on the includeText value
  /// when the save notification was requested.
  Text: string option
}

/// Save registration options.
type TextDocumentSaveRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// The client is supposed to include the content on save.
  IncludeText: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ISaveOptions with
    /// The client is supposed to include the content on save.
    member x.IncludeText = x.IncludeText

/// The parameters sent in a will save text document notification.
type WillSaveTextDocumentParams = {
  /// The document that will be saved.
  TextDocument: TextDocumentIdentifier
  /// The 'TextDocumentSaveReason'.
  Reason: TextDocumentSaveReason
}

/// A text edit applicable to a text document.
type TextEdit = {
  /// The range of the text document to be manipulated. To insert
  /// text into a document create a range where start === end.
  Range: Range
  /// The string to be inserted. For delete operations use an
  /// empty string.
  NewText: string
} with

  interface ITextEdit with
    /// The range of the text document to be manipulated. To insert
    /// text into a document create a range where start === end.
    member x.Range = x.Range
    /// The string to be inserted. For delete operations use an
    /// empty string.
    member x.NewText = x.NewText

/// The watched files change notification's parameters.
type DidChangeWatchedFilesParams = {
  /// The actual file events.
  Changes: FileEvent[]
}

/// Describe options to be used when registered for text document change events.
type DidChangeWatchedFilesRegistrationOptions = {
  /// The watchers to register.
  Watchers: FileSystemWatcher[]
}

/// The publish diagnostic notification's parameters.
type PublishDiagnosticsParams = {
  /// The URI for which diagnostic information is reported.
  Uri: DocumentUri
  /// Optional the version number of the document the diagnostics are published for.
  ///
  /// @since 3.15.0
  Version: int32 option
  /// An array of diagnostic information items.
  Diagnostics: Diagnostic[]
}

/// Completion parameters
type CompletionParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The completion context. This is only available it the client specifies
  /// to send this using the client capability `textDocument.completion.contextSupport === true`
  Context: CompletionContext option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A completion item represents a text snippet that is
/// proposed to complete text that is being typed.
type CompletionItem = {
  /// The label of this completion item.
  ///
  /// The label property is also by default the text that
  /// is inserted when selecting this completion.
  ///
  /// If label details are provided the label itself should
  /// be an unqualified name of the completion item.
  Label: string
  /// Additional details for the label
  ///
  /// @since 3.17.0
  LabelDetails: CompletionItemLabelDetails option
  /// The kind of this completion item. Based of the kind
  /// an icon is chosen by the editor.
  Kind: CompletionItemKind option
  /// Tags for this completion item.
  ///
  /// @since 3.15.0
  Tags: CompletionItemTag[] option
  /// A human-readable string with additional information
  /// about this item, like type or symbol information.
  Detail: string option
  /// A human-readable string that represents a doc-comment.
  Documentation: U2<string, MarkupContent> option
  /// Indicates if this item is deprecated.
  /// @deprecated Use `tags` instead.
  Deprecated: bool option
  /// Select this item when showing.
  ///
  /// *Note* that only one completion item can be selected and that the
  /// tool / client decides which item that is. The rule is that the *first*
  /// item of those that match best is selected.
  Preselect: bool option
  /// A string that should be used when comparing this item
  /// with other items. When `falsy` the {@link CompletionItem.label label}
  /// is used.
  SortText: string option
  /// A string that should be used when filtering a set of
  /// completion items. When `falsy` the {@link CompletionItem.label label}
  /// is used.
  FilterText: string option
  /// A string that should be inserted into a document when selecting
  /// this completion. When `falsy` the {@link CompletionItem.label label}
  /// is used.
  ///
  /// The `insertText` is subject to interpretation by the client side.
  /// Some tools might not take the string literally. For example
  /// VS Code when code complete is requested in this example
  /// `con<cursor position>` and a completion item with an `insertText` of
  /// `console` is provided it will only insert `sole`. Therefore it is
  /// recommended to use `textEdit` instead since it avoids additional client
  /// side interpretation.
  InsertText: string option
  /// The format of the insert text. The format applies to both the
  /// `insertText` property and the `newText` property of a provided
  /// `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
  ///
  /// Please note that the insertTextFormat doesn't apply to
  /// `additionalTextEdits`.
  InsertTextFormat: InsertTextFormat option
  /// How whitespace and indentation is handled during completion
  /// item insertion. If not provided the clients default value depends on
  /// the `textDocument.completion.insertTextMode` client capability.
  ///
  /// @since 3.16.0
  InsertTextMode: InsertTextMode option
  /// An {@link TextEdit edit} which is applied to a document when selecting
  /// this completion. When an edit is provided the value of
  /// {@link CompletionItem.insertText insertText} is ignored.
  ///
  /// Most editors support two different operations when accepting a completion
  /// item. One is to insert a completion text and the other is to replace an
  /// existing text with a completion text. Since this can usually not be
  /// predetermined by a server it can report both ranges. Clients need to
  /// signal support for `InsertReplaceEdits` via the
  /// `textDocument.completion.insertReplaceSupport` client capability
  /// property.
  ///
  /// *Note 1:* The text edit's range as well as both ranges from an insert
  /// replace edit must be a [single line] and they must contain the position
  /// at which completion has been requested.
  /// *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
  /// must be a prefix of the edit's replace range, that means it must be
  /// contained and starting at the same position.
  ///
  /// @since 3.16.0 additional type `InsertReplaceEdit`
  TextEdit: U2<TextEdit, InsertReplaceEdit> option
  /// The edit text used if the completion item is part of a CompletionList and
  /// CompletionList defines an item default for the text edit range.
  ///
  /// Clients will only honor this property if they opt into completion list
  /// item defaults using the capability `completionList.itemDefaults`.
  ///
  /// If not provided and a list's default range is provided the label
  /// property is used as a text.
  ///
  /// @since 3.17.0
  TextEditText: string option
  /// An optional array of additional {@link TextEdit text edits} that are applied when
  /// selecting this completion. Edits must not overlap (including the same insert position)
  /// with the main {@link CompletionItem.textEdit edit} nor with themselves.
  ///
  /// Additional text edits should be used to change text unrelated to the current cursor position
  /// (for example adding an import statement at the top of the file if the completion item will
  /// insert an unqualified type).
  AdditionalTextEdits: TextEdit[] option
  /// An optional set of characters that when pressed while this completion is active will accept it first and
  /// then type that character. *Note* that all commit characters should have `length=1` and that superfluous
  /// characters will be ignored.
  CommitCharacters: string[] option
  /// An optional {@link Command command} that is executed *after* inserting this completion. *Note* that
  /// additional modifications to the current document should be described with the
  /// {@link CompletionItem.additionalTextEdits additionalTextEdits}-property.
  Command: Command option
  /// A data entry field that is preserved on a completion item between a
  /// {@link CompletionRequest} and a {@link CompletionResolveRequest}.
  Data: LSPAny option
}

type CompletionListItemDefaults = {
  /// A default commit character set.
  ///
  /// @since 3.17.0
  CommitCharacters: string[] option
  /// A default edit range.
  ///
  /// @since 3.17.0
  EditRange: U2<Range, CompletionListItemDefaultsEditRangeC2> option
  /// A default insert text format.
  ///
  /// @since 3.17.0
  InsertTextFormat: InsertTextFormat option
  /// A default insert text mode.
  ///
  /// @since 3.17.0
  InsertTextMode: InsertTextMode option
  /// A default data value.
  ///
  /// @since 3.17.0
  Data: LSPAny option
}

type CompletionListItemDefaultsEditRangeC2 = { Insert: Range; Replace: Range }

/// Represents a collection of {@link CompletionItem completion items} to be presented
/// in the editor.
type CompletionList = {
  /// This list it not complete. Further typing results in recomputing this list.
  ///
  /// Recomputed lists have all their items replaced (not appended) in the
  /// incomplete completion sessions.
  IsIncomplete: bool
  /// In many cases the items of an actual completion result share the same
  /// value for properties like `commitCharacters` or the range of a text
  /// edit. A completion list can therefore define item defaults which will
  /// be used if a completion item itself doesn't specify the value.
  ///
  /// If a completion list specifies a default value and a completion item
  /// also specifies a corresponding value the one from the item is used.
  ///
  /// Servers are only allowed to return default values if the client
  /// signals support for this via the `completionList.itemDefaults`
  /// capability.
  ///
  /// @since 3.17.0
  ItemDefaults: CompletionListItemDefaults option
  /// The completion items.
  Items: CompletionItem[]
}

type CompletionOptionsCompletionItem = {
  /// The server has support for completion item label
  /// details (see also `CompletionItemLabelDetails`) when
  /// receiving a completion item in a resolve call.
  ///
  /// @since 3.17.0
  LabelDetailsSupport: bool option
}

/// Registration options for a {@link CompletionRequest}.
type CompletionRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// Most tools trigger completion request automatically without explicitly requesting
  /// it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
  /// starts to type an identifier. For example if the user types `c` in a JavaScript file
  /// code complete will automatically pop up present `console` besides others as a
  /// completion item. Characters that make up identifiers don't need to be listed here.
  ///
  /// If code complete should automatically be trigger on characters not being valid inside
  /// an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
  TriggerCharacters: string[] option
  /// The list of all possible characters that commit a completion. This field can be used
  /// if clients don't support individual commit characters per completion item. See
  /// `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
  ///
  /// If a server provides both `allCommitCharacters` and commit characters on an individual
  /// completion item the ones on the completion item win.
  ///
  /// @since 3.2.0
  AllCommitCharacters: string[] option
  /// The server provides support to resolve additional
  /// information for a completion item.
  ResolveProvider: bool option
  /// The server supports the following `CompletionItem` specific
  /// capabilities.
  ///
  /// @since 3.17.0
  CompletionItem: CompletionOptionsCompletionItem option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ICompletionOptions with
    /// Most tools trigger completion request automatically without explicitly requesting
    /// it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
    /// starts to type an identifier. For example if the user types `c` in a JavaScript file
    /// code complete will automatically pop up present `console` besides others as a
    /// completion item. Characters that make up identifiers don't need to be listed here.
    ///
    /// If code complete should automatically be trigger on characters not being valid inside
    /// an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
    member x.TriggerCharacters = x.TriggerCharacters
    /// The list of all possible characters that commit a completion. This field can be used
    /// if clients don't support individual commit characters per completion item. See
    /// `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
    ///
    /// If a server provides both `allCommitCharacters` and commit characters on an individual
    /// completion item the ones on the completion item win.
    ///
    /// @since 3.2.0
    member x.AllCommitCharacters = x.AllCommitCharacters
    /// The server provides support to resolve additional
    /// information for a completion item.
    member x.ResolveProvider = x.ResolveProvider
    /// The server supports the following `CompletionItem` specific
    /// capabilities.
    ///
    /// @since 3.17.0
    member x.CompletionItem = x.CompletionItem

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link HoverRequest}.
type HoverParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// The result of a hover request.
type Hover = {
  /// The hover's content
  Contents: U3<MarkupContent, MarkedString, MarkedString[]>
  /// An optional range inside the text document that is used to
  /// visualize the hover, e.g. by changing the background color.
  Range: Range option
}

/// Registration options for a {@link HoverRequest}.
type HoverRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IHoverOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link SignatureHelpRequest}.
type SignatureHelpParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The signature help context. This is only available if the client specifies
  /// to send this using the client capability `textDocument.signatureHelp.contextSupport === true`
  ///
  /// @since 3.15.0
  Context: SignatureHelpContext option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Signature help represents the signature of something
/// callable. There can be multiple signature but only one
/// active and only one active parameter.
type SignatureHelp = {
  /// One or more signatures.
  Signatures: SignatureInformation[]
  /// The active signature. If omitted or the value lies outside the
  /// range of `signatures` the value defaults to zero or is ignored if
  /// the `SignatureHelp` has no signatures.
  ///
  /// Whenever possible implementors should make an active decision about
  /// the active signature and shouldn't rely on a default value.
  ///
  /// In future version of the protocol this property might become
  /// mandatory to better express this.
  ActiveSignature: uint32 option
  /// The active parameter of the active signature. If omitted or the value
  /// lies outside the range of `signatures[activeSignature].parameters`
  /// defaults to 0 if the active signature has parameters. If
  /// the active signature has no parameters it is ignored.
  /// In future version of the protocol this property might become
  /// mandatory to better express the active parameter if the
  /// active signature does have any.
  ActiveParameter: uint32 option
}

/// Registration options for a {@link SignatureHelpRequest}.
type SignatureHelpRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// List of characters that trigger signature help automatically.
  TriggerCharacters: string[] option
  /// List of characters that re-trigger signature help.
  ///
  /// These trigger characters are only active when signature help is already showing. All trigger characters
  /// are also counted as re-trigger characters.
  ///
  /// @since 3.15.0
  RetriggerCharacters: string[] option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ISignatureHelpOptions with
    /// List of characters that trigger signature help automatically.
    member x.TriggerCharacters = x.TriggerCharacters
    /// List of characters that re-trigger signature help.
    ///
    /// These trigger characters are only active when signature help is already showing. All trigger characters
    /// are also counted as re-trigger characters.
    ///
    /// @since 3.15.0
    member x.RetriggerCharacters = x.RetriggerCharacters

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link DefinitionRequest}.
type DefinitionParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Registration options for a {@link DefinitionRequest}.
type DefinitionRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDefinitionOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link ReferencesRequest}.
type ReferenceParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  Context: ReferenceContext
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Registration options for a {@link ReferencesRequest}.
type ReferenceRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IReferenceOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link DocumentHighlightRequest}.
type DocumentHighlightParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A document highlight is a range inside a text document which deserves
/// special attention. Usually a document highlight is visualized by changing
/// the background color of its range.
type DocumentHighlight = {
  /// The range this highlight applies to.
  Range: Range
  /// The highlight kind, default is {@link DocumentHighlightKind.Text text}.
  Kind: DocumentHighlightKind option
}

/// Registration options for a {@link DocumentHighlightRequest}.
type DocumentHighlightRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentHighlightOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Parameters for a {@link DocumentSymbolRequest}.
type DocumentSymbolParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The text document.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents information about programming constructs like variables, classes,
/// interfaces etc.
type SymbolInformation = {
  /// The name of this symbol.
  Name: string
  /// The kind of this symbol.
  Kind: SymbolKind
  /// Tags for this symbol.
  ///
  /// @since 3.16.0
  Tags: SymbolTag[] option
  /// The name of the symbol containing this symbol. This information is for
  /// user interface purposes (e.g. to render a qualifier in the user interface
  /// if necessary). It can't be used to re-infer a hierarchy for the document
  /// symbols.
  ContainerName: string option
  /// Indicates if this symbol is deprecated.
  ///
  /// @deprecated Use tags instead
  Deprecated: bool option
  /// The location of this symbol. The location's range is used by a tool
  /// to reveal the location in the editor. If the symbol is selected in the
  /// tool the range's start information is used to position the cursor. So
  /// the range usually spans more than the actual symbol's name and does
  /// normally include things like visibility modifiers.
  ///
  /// The range doesn't have to denote a node range in the sense of an abstract
  /// syntax tree. It can therefore not be used to re-construct a hierarchy of
  /// the symbols.
  Location: Location
} with

  interface IBaseSymbolInformation with
    /// The name of this symbol.
    member x.Name = x.Name
    /// The kind of this symbol.
    member x.Kind = x.Kind
    /// Tags for this symbol.
    ///
    /// @since 3.16.0
    member x.Tags = x.Tags
    /// The name of the symbol containing this symbol. This information is for
    /// user interface purposes (e.g. to render a qualifier in the user interface
    /// if necessary). It can't be used to re-infer a hierarchy for the document
    /// symbols.
    member x.ContainerName = x.ContainerName

/// Represents programming constructs like variables, classes, interfaces etc.
/// that appear in a document. Document symbols can be hierarchical and they
/// have two ranges: one that encloses its definition and one that points to
/// its most interesting range, e.g. the range of an identifier.
type DocumentSymbol = {
  /// The name of this symbol. Will be displayed in the user interface and therefore must not be
  /// an empty string or a string only consisting of white spaces.
  Name: string
  /// More detail for this symbol, e.g the signature of a function.
  Detail: string option
  /// The kind of this symbol.
  Kind: SymbolKind
  /// Tags for this document symbol.
  ///
  /// @since 3.16.0
  Tags: SymbolTag[] option
  /// Indicates if this symbol is deprecated.
  ///
  /// @deprecated Use tags instead
  Deprecated: bool option
  /// The range enclosing this symbol not including leading/trailing whitespace but everything else
  /// like comments. This information is typically used to determine if the clients cursor is
  /// inside the symbol to reveal in the symbol in the UI.
  Range: Range
  /// The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
  /// Must be contained by the `range`.
  SelectionRange: Range
  /// Children of this symbol, e.g. properties of a class.
  Children: DocumentSymbol[] option
}

/// Registration options for a {@link DocumentSymbolRequest}.
type DocumentSymbolRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// A human-readable string that is shown when multiple outlines trees
  /// are shown for the same document.
  ///
  /// @since 3.16.0
  Label: string option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentSymbolOptions with
    /// A human-readable string that is shown when multiple outlines trees
    /// are shown for the same document.
    ///
    /// @since 3.16.0
    member x.Label = x.Label

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link CodeActionRequest}.
type CodeActionParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The document in which the command was invoked.
  TextDocument: TextDocumentIdentifier
  /// The range for which the command was invoked.
  Range: Range
  /// Context carrying additional information.
  Context: CodeActionContext
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents a reference to a command. Provides a title which
/// will be used to represent a command in the UI and, optionally,
/// an array of arguments which will be passed to the command handler
/// function when invoked.
type Command = {
  /// Title of the command, like `save`.
  Title: string
  /// The identifier of the actual command handler.
  Command: string
  /// Arguments that the command handler should be
  /// invoked with.
  Arguments: LSPAny[] option
}

type CodeActionDisabled = {
  /// Human readable description of why the code action is currently disabled.
  ///
  /// This is displayed in the code actions UI.
  Reason: string
}

/// A code action represents a change that can be performed in code, e.g. to fix a problem or
/// to refactor code.
///
/// A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
type CodeAction = {
  /// A short, human-readable, title for this code action.
  Title: string
  /// The kind of the code action.
  ///
  /// Used to filter code actions.
  Kind: CodeActionKind option
  /// The diagnostics that this code action resolves.
  Diagnostics: Diagnostic[] option
  /// Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
  /// by keybindings.
  ///
  /// A quick fix should be marked preferred if it properly addresses the underlying error.
  /// A refactoring should be marked preferred if it is the most reasonable choice of actions to take.
  ///
  /// @since 3.15.0
  IsPreferred: bool option
  /// Marks that the code action cannot currently be applied.
  ///
  /// Clients should follow the following guidelines regarding disabled code actions:
  ///
  ///   - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)
  ///     code action menus.
  ///
  ///   - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type
  ///     of code action, such as refactorings.
  ///
  ///   - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)
  ///     that auto applies a code action and only disabled code actions are returned, the client should show the user an
  ///     error message with `reason` in the editor.
  ///
  /// @since 3.16.0
  Disabled: CodeActionDisabled option
  /// The workspace edit this code action performs.
  Edit: WorkspaceEdit option
  /// A command this code action executes. If a code action
  /// provides an edit and a command, first the edit is
  /// executed and then the command.
  Command: Command option
  /// A data entry field that is preserved on a code action between
  /// a `textDocument/codeAction` and a `codeAction/resolve` request.
  ///
  /// @since 3.16.0
  Data: LSPAny option
}

/// Registration options for a {@link CodeActionRequest}.
type CodeActionRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// CodeActionKinds that this server may return.
  ///
  /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  /// may list out every specific kind they provide.
  CodeActionKinds: CodeActionKind[] option
  /// The server provides support to resolve additional
  /// information for a code action.
  ///
  /// @since 3.16.0
  ResolveProvider: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ICodeActionOptions with
    /// CodeActionKinds that this server may return.
    ///
    /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    /// may list out every specific kind they provide.
    member x.CodeActionKinds = x.CodeActionKinds
    /// The server provides support to resolve additional
    /// information for a code action.
    ///
    /// @since 3.16.0
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link WorkspaceSymbolRequest}.
type WorkspaceSymbolParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// A query string to filter symbols by. Clients may send an empty
  /// string here to request all symbols.
  Query: string
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

type WorkspaceSymbolLocationC2 = { Uri: DocumentUri }

/// A special workspace symbol that supports locations without a range.
///
/// See also SymbolInformation.
///
/// @since 3.17.0
type WorkspaceSymbol = {
  /// The name of this symbol.
  Name: string
  /// The kind of this symbol.
  Kind: SymbolKind
  /// Tags for this symbol.
  ///
  /// @since 3.16.0
  Tags: SymbolTag[] option
  /// The name of the symbol containing this symbol. This information is for
  /// user interface purposes (e.g. to render a qualifier in the user interface
  /// if necessary). It can't be used to re-infer a hierarchy for the document
  /// symbols.
  ContainerName: string option
  /// The location of the symbol. Whether a server is allowed to
  /// return a location without a range depends on the client
  /// capability `workspace.symbol.resolveSupport`.
  ///
  /// See SymbolInformation#location for more details.
  Location: U2<Location, WorkspaceSymbolLocationC2>
  /// A data entry field that is preserved on a workspace symbol between a
  /// workspace symbol request and a workspace symbol resolve request.
  Data: LSPAny option
} with

  interface IBaseSymbolInformation with
    /// The name of this symbol.
    member x.Name = x.Name
    /// The kind of this symbol.
    member x.Kind = x.Kind
    /// Tags for this symbol.
    ///
    /// @since 3.16.0
    member x.Tags = x.Tags
    /// The name of the symbol containing this symbol. This information is for
    /// user interface purposes (e.g. to render a qualifier in the user interface
    /// if necessary). It can't be used to re-infer a hierarchy for the document
    /// symbols.
    member x.ContainerName = x.ContainerName

/// Registration options for a {@link WorkspaceSymbolRequest}.
type WorkspaceSymbolRegistrationOptions = {
  WorkDoneProgress: bool option
  /// The server provides support to resolve additional
  /// information for a workspace symbol.
  ///
  /// @since 3.17.0
  ResolveProvider: bool option
} with

  interface IWorkspaceSymbolOptions with
    /// The server provides support to resolve additional
    /// information for a workspace symbol.
    ///
    /// @since 3.17.0
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link CodeLensRequest}.
type CodeLensParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The document to request code lens for.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A code lens represents a {@link Command command} that should be shown along with
/// source text, like the number of references, a way to run tests, etc.
///
/// A code lens is _unresolved_ when no command is associated to it. For performance
/// reasons the creation of a code lens and resolving should be done in two stages.
type CodeLens = {
  /// The range in which this code lens is valid. Should only span a single line.
  Range: Range
  /// The command this code lens represents.
  Command: Command option
  /// A data entry field that is preserved on a code lens item between
  /// a {@link CodeLensRequest} and a {@link CodeLensResolveRequest}
  Data: LSPAny option
}

/// Registration options for a {@link CodeLensRequest}.
type CodeLensRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// Code lens has a resolve provider as well.
  ResolveProvider: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface ICodeLensOptions with
    /// Code lens has a resolve provider as well.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link DocumentLinkRequest}.
type DocumentLinkParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
  /// The document to provide document links for.
  TextDocument: TextDocumentIdentifier
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// A document link is a range in a text document that links to an internal or external resource, like another
/// text document or a web site.
type DocumentLink = {
  /// The range this link applies to.
  Range: Range
  /// The uri this link points to. If missing a resolve request is sent later.
  Target: URI option
  /// The tooltip text when you hover over this link.
  ///
  /// If a tooltip is provided, is will be displayed in a string that includes instructions on how to
  /// trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
  /// user settings, and localization.
  ///
  /// @since 3.15.0
  Tooltip: string option
  /// A data entry field that is preserved on a document link between a
  /// DocumentLinkRequest and a DocumentLinkResolveRequest.
  Data: LSPAny option
}

/// Registration options for a {@link DocumentLinkRequest}.
type DocumentLinkRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// Document links have a resolve provider as well.
  ResolveProvider: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentLinkOptions with
    /// Document links have a resolve provider as well.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link DocumentFormattingRequest}.
type DocumentFormattingParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The document to format.
  TextDocument: TextDocumentIdentifier
  /// The format options.
  Options: FormattingOptions
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Registration options for a {@link DocumentFormattingRequest}.
type DocumentFormattingRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentFormattingOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link DocumentRangeFormattingRequest}.
type DocumentRangeFormattingParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The document to format.
  TextDocument: TextDocumentIdentifier
  /// The range to format
  Range: Range
  /// The format options
  Options: FormattingOptions
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Registration options for a {@link DocumentRangeFormattingRequest}.
type DocumentRangeFormattingRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentRangeFormattingOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters of a {@link DocumentOnTypeFormattingRequest}.
type DocumentOnTypeFormattingParams = {
  /// The document to format.
  TextDocument: TextDocumentIdentifier
  /// The position around which the on type formatting should happen.
  /// This is not necessarily the exact position where the character denoted
  /// by the property `ch` got typed.
  Position: Position
  /// The character that has been typed that triggered the formatting
  /// on type request. That is not necessarily the last character that
  /// got inserted into the document since the client could auto insert
  /// characters as well (e.g. like automatic brace completion).
  Ch: string
  /// The formatting options.
  Options: FormattingOptions
}

/// Registration options for a {@link DocumentOnTypeFormattingRequest}.
type DocumentOnTypeFormattingRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  /// A character on which formatting should be triggered, like `{`.
  FirstTriggerCharacter: string
  /// More trigger characters.
  MoreTriggerCharacter: string[] option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IDocumentOnTypeFormattingOptions with
    /// A character on which formatting should be triggered, like `{`.
    member x.FirstTriggerCharacter = x.FirstTriggerCharacter
    /// More trigger characters.
    member x.MoreTriggerCharacter = x.MoreTriggerCharacter

/// The parameters of a {@link RenameRequest}.
type RenameParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The document to rename.
  TextDocument: TextDocumentIdentifier
  /// The position at which this request was sent.
  Position: Position
  /// The new name of the symbol. If the given name is not valid the
  /// request must return a {@link ResponseError} with an
  /// appropriate message set.
  NewName: string
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Registration options for a {@link RenameRequest}.
type RenameRegistrationOptions = {
  /// A document selector to identify the scope of the registration. If set to null
  /// the document selector provided on the client side will be used.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  DocumentSelector: DocumentSelector option
  WorkDoneProgress: bool option
  /// Renames should be checked and tested before being executed.
  ///
  /// @since version 3.12.0
  PrepareProvider: bool option
} with

  interface ITextDocumentRegistrationOptions with
    /// A document selector to identify the scope of the registration. If set to null
    /// the document selector provided on the client side will be used.
    member x.DocumentSelector = x.DocumentSelector

  interface IRenameOptions with
    /// Renames should be checked and tested before being executed.
    ///
    /// @since version 3.12.0
    member x.PrepareProvider = x.PrepareProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

type PrepareRenameParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// The parameters of a {@link ExecuteCommandRequest}.
type ExecuteCommandParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The identifier of the actual command handler.
  Command: string
  /// Arguments that the command should be invoked with.
  Arguments: LSPAny[] option
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

/// Registration options for a {@link ExecuteCommandRequest}.
type ExecuteCommandRegistrationOptions = {
  WorkDoneProgress: bool option
  /// The commands to be executed on the server
  Commands: string[]
} with

  interface IExecuteCommandOptions with
    /// The commands to be executed on the server
    member x.Commands = x.Commands

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The parameters passed via an apply workspace edit request.
type ApplyWorkspaceEditParams = {
  /// An optional label of the workspace edit. This label is
  /// presented in the user interface for example on an undo
  /// stack to undo the workspace edit.
  Label: string option
  /// The edits to apply.
  Edit: WorkspaceEdit
}

/// The result returned from the apply workspace edit request.
///
/// @since 3.17 renamed from ApplyWorkspaceEditResponse
type ApplyWorkspaceEditResult = {
  /// Indicates whether the edit was applied or not.
  Applied: bool
  /// An optional textual description for why the edit was not applied.
  /// This may be used by the server for diagnostic logging or to provide
  /// a suitable error for a request that triggered the edit.
  FailureReason: string option
  /// Depending on the client's failure handling strategy `failedChange` might
  /// contain the index of the change that failed. This property is only available
  /// if the client signals a `failureHandlingStrategy` in its client capabilities.
  FailedChange: uint32 option
}

type WorkDoneProgressBegin = {
  [<UnionKindAttribute("begin")>]
  Kind: string
  /// Mandatory title of the progress operation. Used to briefly inform about
  /// the kind of operation being performed.
  ///
  /// Examples: "Indexing" or "Linking dependencies".
  Title: string
  /// Controls if a cancel button should show to allow the user to cancel the
  /// long running operation. Clients that don't support cancellation are allowed
  /// to ignore the setting.
  Cancellable: bool option
  /// Optional, more detailed associated progress message. Contains
  /// complementary information to the `title`.
  ///
  /// Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
  /// If unset, the previous progress message (if any) is still valid.
  Message: string option
  /// Optional progress percentage to display (value 100 is considered 100%).
  /// If not provided infinite progress is assumed and clients are allowed
  /// to ignore the `percentage` value in subsequent in report notifications.
  ///
  /// The value should be steadily rising. Clients are free to ignore values
  /// that are not following this rule. The value range is [0, 100].
  Percentage: uint32 option
}

type WorkDoneProgressReport = {
  [<UnionKindAttribute("report")>]
  Kind: string
  /// Controls enablement state of a cancel button.
  ///
  /// Clients that don't support cancellation or don't support controlling the button's
  /// enablement state are allowed to ignore the property.
  Cancellable: bool option
  /// Optional, more detailed associated progress message. Contains
  /// complementary information to the `title`.
  ///
  /// Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
  /// If unset, the previous progress message (if any) is still valid.
  Message: string option
  /// Optional progress percentage to display (value 100 is considered 100%).
  /// If not provided infinite progress is assumed and clients are allowed
  /// to ignore the `percentage` value in subsequent in report notifications.
  ///
  /// The value should be steadily rising. Clients are free to ignore values
  /// that are not following this rule. The value range is [0, 100]
  Percentage: uint32 option
}

type WorkDoneProgressEnd = {
  [<UnionKindAttribute("end")>]
  Kind: string
  /// Optional, a final message indicating to for example indicate the outcome
  /// of the operation.
  Message: string option
}

type SetTraceParams = { Value: TraceValues }
type LogTraceParams = { Message: string; Verbose: string option }

type CancelParams = {
  /// The request id to cancel.
  Id: U2<int32, string>
}

type ProgressParams = {
  /// The progress token provided by the client or server.
  Token: ProgressToken
  /// The progress data.
  Value: LSPAny
}

/// A parameter literal used in requests to pass a text document and a position inside that
/// document.
type TextDocumentPositionParams = {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The position inside the text document.
  Position: Position
} with

  interface ITextDocumentPositionParams with
    /// The text document.
    member x.TextDocument = x.TextDocument
    /// The position inside the text document.
    member x.Position = x.Position

type WorkDoneProgressParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
} with

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

type PartialResultParams = {
  /// An optional token that a server can use to report partial results (e.g. streaming) to
  /// the client.
  PartialResultToken: ProgressToken option
} with

  interface IPartialResultParams with
    /// An optional token that a server can use to report partial results (e.g. streaming) to
    /// the client.
    member x.PartialResultToken = x.PartialResultToken

/// Represents the connection of two locations. Provides additional metadata over normal {@link Location locations},
/// including an origin range.
type LocationLink = {
  /// Span of the origin of this link.
  ///
  /// Used as the underlined span for mouse interaction. Defaults to the word range at
  /// the definition position.
  OriginSelectionRange: Range option
  /// The target resource identifier of this link.
  TargetUri: DocumentUri
  /// The full target range of this link. If the target for example is a symbol then target range is the
  /// range enclosing this symbol not including leading/trailing whitespace but everything else
  /// like comments. This information is typically used to highlight the range in the editor.
  TargetRange: Range
  /// The range that should be selected and revealed when this link is being followed, e.g the name of a function.
  /// Must be contained by the `targetRange`. See also `DocumentSymbol#range`
  TargetSelectionRange: Range
}

/// A range in a text document expressed as (zero-based) start and end positions.
///
/// If you want to specify a range that contains a line including the line ending
/// character(s) then use an end position denoting the start of the next line.
/// For example:
/// ```ts
/// {
///     start: { line: 5, character: 23 }
///     end : { line 6, character : 0 }
/// }
/// ```
[<DebuggerDisplay("{DebuggerDisplay}")>]
type Range = {
  /// The range's start position.
  Start: Position
  /// The range's end position.
  End: Position
} with

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay = $"{x.Start.DebuggerDisplay}-{x.End.DebuggerDisplay}"

type ImplementationOptions = {
  WorkDoneProgress: bool option
} with

  interface IImplementationOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Static registration options to be returned in the initialize
/// request.
type StaticRegistrationOptions = {
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
}

type TypeDefinitionOptions = {
  WorkDoneProgress: bool option
} with

  interface ITypeDefinitionOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The workspace folder change event.
type WorkspaceFoldersChangeEvent = {
  /// The array of added workspace folders
  Added: WorkspaceFolder[]
  /// The array of the removed workspace folders
  Removed: WorkspaceFolder[]
}

type ConfigurationItem = {
  /// The scope to get the configuration section for.
  ScopeUri: URI option
  /// The configuration section asked for.
  Section: string option
}

/// A literal to identify a text document in the client.
type TextDocumentIdentifier = {
  /// The text document's uri.
  Uri: DocumentUri
} with

  interface ITextDocumentIdentifier with
    /// The text document's uri.
    member x.Uri = x.Uri

/// Represents a color in RGBA space.
type Color = {
  /// The red component of this color in the range [0-1].
  Red: decimal
  /// The green component of this color in the range [0-1].
  Green: decimal
  /// The blue component of this color in the range [0-1].
  Blue: decimal
  /// The alpha component of this color in the range [0-1].
  Alpha: decimal
}

type DocumentColorOptions = {
  WorkDoneProgress: bool option
} with

  interface IDocumentColorOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

type FoldingRangeOptions = {
  WorkDoneProgress: bool option
} with

  interface IFoldingRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

type DeclarationOptions = {
  WorkDoneProgress: bool option
} with

  interface IDeclarationOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Position in a text document expressed as zero-based line and character
/// offset. Prior to 3.17 the offsets were always based on a UTF-16 string
/// representation. So a string of the form `a𐐀b` the character offset of the
/// character `a` is 0, the character offset of `𐐀` is 1 and the character
/// offset of b is 3 since `𐐀` is represented using two code units in UTF-16.
/// Since 3.17 clients and servers can agree on a different string encoding
/// representation (e.g. UTF-8). The client announces it's supported encoding
/// via the client capability [`general.positionEncodings`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#clientCapabilities).
/// The value is an array of position encodings the client supports, with
/// decreasing preference (e.g. the encoding at index `0` is the most preferred
/// one). To stay backwards compatible the only mandatory encoding is UTF-16
/// represented via the string `utf-16`. The server can pick one of the
/// encodings offered by the client and signals that encoding back to the
/// client via the initialize result's property
/// [`capabilities.positionEncoding`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#serverCapabilities). If the string value
/// `utf-16` is missing from the client's capability `general.positionEncodings`
/// servers can safely assume that the client supports UTF-16. If the server
/// omits the position encoding in its initialize result the encoding defaults
/// to the string value `utf-16`. Implementation considerations: since the
/// conversion from one encoding into another requires the content of the
/// file / line the conversion is best done where the file is read which is
/// usually on the server side.
///
/// Positions are line end character agnostic. So you can not specify a position
/// that denotes `\r|\n` or `\n|` where `|` represents the character offset.
///
/// @since 3.17.0 - support for negotiated position encoding.
[<DebuggerDisplay("{DebuggerDisplay}")>]
type Position = {
  /// Line position in a document (zero-based).
  ///
  /// If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
  /// If a line number is negative, it defaults to 0.
  Line: uint32
  /// Character offset on a line in a document (zero-based).
  ///
  /// The meaning of this offset is determined by the negotiated
  /// `PositionEncodingKind`.
  ///
  /// If the character value is greater than the line length it defaults back to the
  /// line length.
  Character: uint32
} with

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay = $"({x.Line},{x.Character})"

type SelectionRangeOptions = {
  WorkDoneProgress: bool option
} with

  interface ISelectionRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Call hierarchy options used during static registration.
///
/// @since 3.16.0
type CallHierarchyOptions = {
  WorkDoneProgress: bool option
} with

  interface ICallHierarchyOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// @since 3.16.0
type SemanticTokensOptions = {
  WorkDoneProgress: bool option
  /// The legend used by the server
  Legend: SemanticTokensLegend
  /// Server supports providing semantic tokens for a specific range
  /// of a document.
  Range: U2<bool, JToken> option
  /// Server supports providing semantic tokens for a full document.
  Full: U2<bool, SemanticTokensOptionsFullC2> option
} with

  interface ISemanticTokensOptions with
    /// The legend used by the server
    member x.Legend = x.Legend
    /// Server supports providing semantic tokens for a specific range
    /// of a document.
    member x.Range = x.Range
    /// Server supports providing semantic tokens for a full document.
    member x.Full = x.Full

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// @since 3.16.0
type SemanticTokensEdit = {
  /// The start offset of the edit.
  Start: uint32
  /// The count of elements to remove.
  DeleteCount: uint32
  /// The elements to insert.
  Data: uint32[] option
}

type LinkedEditingRangeOptions = {
  WorkDoneProgress: bool option
} with

  interface ILinkedEditingRangeOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Represents information on a file/folder create.
///
/// @since 3.16.0
type FileCreate = {
  /// A file:// URI for the location of the file/folder being created.
  Uri: string
}

/// Describes textual changes on a text document. A TextDocumentEdit describes all changes
/// on a document version Si and after they are applied move the document to version Si+1.
/// So the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any
/// kind of ordering. However the edits must be non overlapping.
type TextDocumentEdit = {
  /// The text document to change.
  TextDocument: OptionalVersionedTextDocumentIdentifier
  /// The edits to be applied.
  ///
  /// @since 3.16.0 - support for AnnotatedTextEdit. This is guarded using a
  /// client capability.
  Edits: U2<TextEdit, AnnotatedTextEdit>[]
}

/// Create file operation.
type CreateFile = {
  /// A create
  [<UnionKindAttribute("create")>]
  Kind: string
  /// An optional annotation identifier describing the operation.
  ///
  /// @since 3.16.0
  AnnotationId: ChangeAnnotationIdentifier option
  /// The resource to create.
  Uri: DocumentUri
  /// Additional options
  Options: CreateFileOptions option
} with

  interface IResourceOperation with
    /// The resource operation kind.
    member x.Kind = x.Kind
    /// An optional annotation identifier describing the operation.
    ///
    /// @since 3.16.0
    member x.AnnotationId = x.AnnotationId

/// Rename file operation
type RenameFile = {
  /// A rename
  [<UnionKindAttribute("rename")>]
  Kind: string
  /// An optional annotation identifier describing the operation.
  ///
  /// @since 3.16.0
  AnnotationId: ChangeAnnotationIdentifier option
  /// The old (existing) location.
  OldUri: DocumentUri
  /// The new location.
  NewUri: DocumentUri
  /// Rename options.
  Options: RenameFileOptions option
} with

  interface IResourceOperation with
    /// The resource operation kind.
    member x.Kind = x.Kind
    /// An optional annotation identifier describing the operation.
    ///
    /// @since 3.16.0
    member x.AnnotationId = x.AnnotationId

/// Delete file operation
type DeleteFile = {
  /// A delete
  [<UnionKindAttribute("delete")>]
  Kind: string
  /// An optional annotation identifier describing the operation.
  ///
  /// @since 3.16.0
  AnnotationId: ChangeAnnotationIdentifier option
  /// The file to delete.
  Uri: DocumentUri
  /// Delete options.
  Options: DeleteFileOptions option
} with

  interface IResourceOperation with
    /// The resource operation kind.
    member x.Kind = x.Kind
    /// An optional annotation identifier describing the operation.
    ///
    /// @since 3.16.0
    member x.AnnotationId = x.AnnotationId

/// Additional information that describes document changes.
///
/// @since 3.16.0
type ChangeAnnotation = {
  /// A human-readable string describing the actual change. The string
  /// is rendered prominent in the user interface.
  Label: string
  /// A flag which indicates that user confirmation is needed
  /// before applying the change.
  NeedsConfirmation: bool option
  /// A human-readable string which is rendered less prominent in
  /// the user interface.
  Description: string option
}

/// A filter to describe in which file operation requests or notifications
/// the server is interested in receiving.
///
/// @since 3.16.0
type FileOperationFilter = {
  /// A Uri scheme like `file` or `untitled`.
  Scheme: string option
  /// The actual file operation pattern.
  Pattern: FileOperationPattern
}

/// Represents information on a file/folder rename.
///
/// @since 3.16.0
type FileRename = {
  /// A file:// URI for the original location of the file/folder being renamed.
  OldUri: string
  /// A file:// URI for the new location of the file/folder being renamed.
  NewUri: string
}

/// Represents information on a file/folder delete.
///
/// @since 3.16.0
type FileDelete = {
  /// A file:// URI for the location of the file/folder being deleted.
  Uri: string
}

type MonikerOptions = {
  WorkDoneProgress: bool option
} with

  interface IMonikerOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Type hierarchy options used during static registration.
///
/// @since 3.17.0
type TypeHierarchyOptions = {
  WorkDoneProgress: bool option
} with

  interface ITypeHierarchyOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// @since 3.17.0
type InlineValueContext = {
  /// The stack frame (as a DAP Id) where the execution has stopped.
  FrameId: int32
  /// The document range where execution has stopped.
  /// Typically the end position of the range denotes the line where the inline values are shown.
  StoppedLocation: Range
}

/// Provide inline value as text.
///
/// @since 3.17.0
type InlineValueText = {
  /// The document range for which the inline value applies.
  Range: Range
  /// The text of the inline value.
  Text: string
}

/// Provide inline value through a variable lookup.
/// If only a range is specified, the variable name will be extracted from the underlying document.
/// An optional variable name can be used to override the extracted name.
///
/// @since 3.17.0
type InlineValueVariableLookup = {
  /// The document range for which the inline value applies.
  /// The range is used to extract the variable name from the underlying document.
  Range: Range
  /// If specified the name of the variable to look up.
  VariableName: string option
  /// How to perform the lookup.
  CaseSensitiveLookup: bool
}

/// Provide an inline value through an expression evaluation.
/// If only a range is specified, the expression will be extracted from the underlying document.
/// An optional expression can be used to override the extracted expression.
///
/// @since 3.17.0
type InlineValueEvaluatableExpression = {
  /// The document range for which the inline value applies.
  /// The range is used to extract the evaluatable expression from the underlying document.
  Range: Range
  /// If specified the expression overrides the extracted expression.
  Expression: string option
}

/// Inline value options used during static registration.
///
/// @since 3.17.0
type InlineValueOptions = {
  WorkDoneProgress: bool option
} with

  interface IInlineValueOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// An inlay hint label part allows for interactive and composite labels
/// of inlay hints.
///
/// @since 3.17.0
type InlayHintLabelPart = {
  /// The value of this label part.
  Value: string
  /// The tooltip text when you hover over this label part. Depending on
  /// the client capability `inlayHint.resolveSupport` clients might resolve
  /// this property late using the resolve request.
  Tooltip: U2<string, MarkupContent> option
  /// An optional source code location that represents this
  /// label part.
  ///
  /// The editor will use this location for the hover and for code navigation
  /// features: This part will become a clickable link that resolves to the
  /// definition of the symbol at the given location (not necessarily the
  /// location itself), it shows the hover that shows at the given location,
  /// and it shows a context menu with further code navigation commands.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  Location: Location option
  /// An optional command for this label part.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  Command: Command option
}

/// A `MarkupContent` literal represents a string value which content is interpreted base on its
/// kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
///
/// If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
/// See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
///
/// Here is an example how such a string can be constructed using JavaScript / TypeScript:
/// ```ts
/// let markdown: MarkdownContent = {
///  kind: MarkupKind.Markdown,
///  value: [
///    '# Header',
///    'Some text',
///    '```typescript',
///    'someCode();',
///    '```'
///  ].join('\n')
/// };
/// ```
///
/// *Please Note* that clients might sanitize the return markdown. A client could decide to
/// remove HTML from the markdown to avoid script execution.
type MarkupContent = {
  /// The type of the Markup
  Kind: MarkupKind
  /// The content itself
  Value: string
}

/// Inlay hint options used during static registration.
///
/// @since 3.17.0
type InlayHintOptions = {
  WorkDoneProgress: bool option
  /// The server provides support to resolve additional
  /// information for an inlay hint item.
  ResolveProvider: bool option
} with

  interface IInlayHintOptions with
    /// The server provides support to resolve additional
    /// information for an inlay hint item.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// A full diagnostic report with a set of related documents.
///
/// @since 3.17.0
type RelatedFullDocumentDiagnosticReport = {
  /// A full document diagnostic report.
  [<UnionKindAttribute("full")>]
  Kind: string
  /// An optional result id. If provided it will
  /// be sent on the next diagnostic request for the
  /// same document.
  ResultId: string option
  /// The actual items.
  Items: Diagnostic[]
  /// Diagnostics of related documents. This information is useful
  /// in programming languages where code in a file A can generate
  /// diagnostics in a file B which A depends on. An example of
  /// such a language is C/C++ where marco definitions in a file
  /// a.cpp and result in errors in a header file b.hpp.
  ///
  /// @since 3.17.0
  RelatedDocuments: Map<DocumentUri, U2<FullDocumentDiagnosticReport, UnchangedDocumentDiagnosticReport>> option
} with

  interface IFullDocumentDiagnosticReport with
    /// A full document diagnostic report.
    member x.Kind = x.Kind
    /// An optional result id. If provided it will
    /// be sent on the next diagnostic request for the
    /// same document.
    member x.ResultId = x.ResultId
    /// The actual items.
    member x.Items = x.Items

/// An unchanged diagnostic report with a set of related documents.
///
/// @since 3.17.0
type RelatedUnchangedDocumentDiagnosticReport = {
  /// A document diagnostic report indicating
  /// no changes to the last result. A server can
  /// only return `unchanged` if result ids are
  /// provided.
  [<UnionKindAttribute("unchanged")>]
  Kind: string
  /// A result id which will be sent on the next
  /// diagnostic request for the same document.
  ResultId: string
  /// Diagnostics of related documents. This information is useful
  /// in programming languages where code in a file A can generate
  /// diagnostics in a file B which A depends on. An example of
  /// such a language is C/C++ where marco definitions in a file
  /// a.cpp and result in errors in a header file b.hpp.
  ///
  /// @since 3.17.0
  RelatedDocuments: Map<DocumentUri, U2<FullDocumentDiagnosticReport, UnchangedDocumentDiagnosticReport>> option
} with

  interface IUnchangedDocumentDiagnosticReport with
    /// A document diagnostic report indicating
    /// no changes to the last result. A server can
    /// only return `unchanged` if result ids are
    /// provided.
    member x.Kind = x.Kind
    /// A result id which will be sent on the next
    /// diagnostic request for the same document.
    member x.ResultId = x.ResultId

/// A diagnostic report with a full set of problems.
///
/// @since 3.17.0
type FullDocumentDiagnosticReport = {
  /// A full document diagnostic report.
  [<UnionKindAttribute("full")>]
  Kind: string
  /// An optional result id. If provided it will
  /// be sent on the next diagnostic request for the
  /// same document.
  ResultId: string option
  /// The actual items.
  Items: Diagnostic[]
} with

  interface IFullDocumentDiagnosticReport with
    /// A full document diagnostic report.
    member x.Kind = x.Kind
    /// An optional result id. If provided it will
    /// be sent on the next diagnostic request for the
    /// same document.
    member x.ResultId = x.ResultId
    /// The actual items.
    member x.Items = x.Items

/// A diagnostic report indicating that the last returned
/// report is still accurate.
///
/// @since 3.17.0
type UnchangedDocumentDiagnosticReport = {
  /// A document diagnostic report indicating
  /// no changes to the last result. A server can
  /// only return `unchanged` if result ids are
  /// provided.
  [<UnionKindAttribute("unchanged")>]
  Kind: string
  /// A result id which will be sent on the next
  /// diagnostic request for the same document.
  ResultId: string
} with

  interface IUnchangedDocumentDiagnosticReport with
    /// A document diagnostic report indicating
    /// no changes to the last result. A server can
    /// only return `unchanged` if result ids are
    /// provided.
    member x.Kind = x.Kind
    /// A result id which will be sent on the next
    /// diagnostic request for the same document.
    member x.ResultId = x.ResultId

/// Diagnostic options.
///
/// @since 3.17.0
type DiagnosticOptions = {
  WorkDoneProgress: bool option
  /// An optional identifier under which the diagnostics are
  /// managed by the client.
  Identifier: string option
  /// Whether the language has inter file dependencies meaning that
  /// editing code in one file can result in a different diagnostic
  /// set in another file. Inter file dependencies are common for
  /// most programming languages and typically uncommon for linters.
  InterFileDependencies: bool
  /// The server provides support for workspace diagnostics as well.
  WorkspaceDiagnostics: bool
} with

  interface IDiagnosticOptions with
    /// An optional identifier under which the diagnostics are
    /// managed by the client.
    member x.Identifier = x.Identifier
    /// Whether the language has inter file dependencies meaning that
    /// editing code in one file can result in a different diagnostic
    /// set in another file. Inter file dependencies are common for
    /// most programming languages and typically uncommon for linters.
    member x.InterFileDependencies = x.InterFileDependencies
    /// The server provides support for workspace diagnostics as well.
    member x.WorkspaceDiagnostics = x.WorkspaceDiagnostics

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// A previous result id in a workspace pull request.
///
/// @since 3.17.0
type PreviousResultId = {
  /// The URI for which the client knowns a
  /// result id.
  Uri: DocumentUri
  /// The value of the previous result id.
  Value: string
}

/// A notebook document.
///
/// @since 3.17.0
type NotebookDocument = {
  /// The notebook document's uri.
  Uri: URI
  /// The type of the notebook.
  NotebookType: string
  /// The version number of this document (it will increase after each
  /// change, including undo/redo).
  Version: int32
  /// Additional metadata stored with the notebook
  /// document.
  ///
  /// Note: should always be an object literal (e.g. LSPObject)
  Metadata: LSPObject option
  /// The cells of a notebook.
  Cells: NotebookCell[]
}

/// An item to transfer a text document from the client to the
/// server.
type TextDocumentItem = {
  /// The text document's uri.
  Uri: DocumentUri
  /// The text document's language identifier.
  LanguageId: string
  /// The version number of this document (it will increase after each
  /// change, including undo/redo).
  Version: int32
  /// The content of the opened text document.
  Text: string
}

/// A versioned notebook document identifier.
///
/// @since 3.17.0
type VersionedNotebookDocumentIdentifier = {
  /// The version number of this notebook document.
  Version: int32
  /// The notebook document's uri.
  Uri: URI
}

type NotebookDocumentChangeEventCells = {
  /// Changes to the cell structure to add or
  /// remove cells.
  Structure: NotebookDocumentChangeEventCellsStructure option
  /// Changes to notebook cells properties like its
  /// kind, execution summary or metadata.
  Data: NotebookCell[] option
  /// Changes to the text content of notebook cells.
  TextContent: NotebookDocumentChangeEventCellsTextContent[] option
}

type NotebookDocumentChangeEventCellsStructure = {
  /// The change to the cell array.
  Array: NotebookCellArrayChange
  /// Additional opened cell text documents.
  DidOpen: TextDocumentItem[] option
  /// Additional closed cell text documents.
  DidClose: TextDocumentIdentifier[] option
}

type NotebookDocumentChangeEventCellsTextContent = {
  Document: VersionedTextDocumentIdentifier
  Changes: TextDocumentContentChangeEvent[]
}

/// A change event for a notebook document.
///
/// @since 3.17.0
type NotebookDocumentChangeEvent = {
  /// The changed meta data if any.
  ///
  /// Note: should always be an object literal (e.g. LSPObject)
  Metadata: LSPObject option
  /// Changes to cells
  Cells: NotebookDocumentChangeEventCells option
}

/// A literal to identify a notebook document in the client.
///
/// @since 3.17.0
type NotebookDocumentIdentifier = {
  /// The notebook document's uri.
  Uri: URI
}

/// General parameters to register for a notification or to register a provider.
type Registration = {
  /// The id used to register the request. The id can be used to deregister
  /// the request again.
  Id: string
  /// The method / capability to register for.
  Method: string
  /// Options necessary for the registration.
  RegisterOptions: LSPAny option
}

/// General parameters to unregister a request or notification.
type Unregistration = {
  /// The id used to unregister the request or notification. Usually an id
  /// provided during the register request.
  Id: string
  /// The method to unregister for.
  Method: string
}

/// The initialize parameters
type _InitializeParams = {
  /// An optional token that a server can use to report work done progress.
  WorkDoneToken: ProgressToken option
  /// The process Id of the parent process that started
  /// the server.
  ///
  /// Is `null` if the process has not been started by another process.
  /// If the parent process is not alive then the server should exit.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  ProcessId: int32 option
  /// Information about the client
  ///
  /// @since 3.15.0
  ClientInfo: _InitializeParamsClientInfo option
  /// The locale the client is currently showing the user interface
  /// in. This must not necessarily be the locale of the operating
  /// system.
  ///
  /// Uses IETF language tags as the value's syntax
  /// (See https://en.wikipedia.org/wiki/IETF_language_tag)
  ///
  /// @since 3.16.0
  Locale: string option
  /// The rootPath of the workspace. Is null
  /// if no folder is open.
  ///
  /// @deprecated in favour of rootUri.
  RootPath: string option
  /// The rootUri of the workspace. Is null if no
  /// folder is open. If both `rootPath` and `rootUri` are set
  /// `rootUri` wins.
  ///
  /// @deprecated in favour of workspaceFolders.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  RootUri: DocumentUri option
  /// The capabilities provided by the client (editor or tool)
  Capabilities: ClientCapabilities
  /// User provided initialization options.
  InitializationOptions: LSPAny option
  /// The initial trace setting. If omitted trace is disabled ('off').
  Trace: TraceValues option
} with

  interface I_InitializeParams with
    /// The process Id of the parent process that started
    /// the server.
    ///
    /// Is `null` if the process has not been started by another process.
    /// If the parent process is not alive then the server should exit.
    member x.ProcessId = x.ProcessId
    /// Information about the client
    ///
    /// @since 3.15.0
    member x.ClientInfo = x.ClientInfo
    /// The locale the client is currently showing the user interface
    /// in. This must not necessarily be the locale of the operating
    /// system.
    ///
    /// Uses IETF language tags as the value's syntax
    /// (See https://en.wikipedia.org/wiki/IETF_language_tag)
    ///
    /// @since 3.16.0
    member x.Locale = x.Locale
    /// The rootPath of the workspace. Is null
    /// if no folder is open.
    ///
    /// @deprecated in favour of rootUri.
    member x.RootPath = x.RootPath
    /// The rootUri of the workspace. Is null if no
    /// folder is open. If both `rootPath` and `rootUri` are set
    /// `rootUri` wins.
    ///
    /// @deprecated in favour of workspaceFolders.
    member x.RootUri = x.RootUri
    /// The capabilities provided by the client (editor or tool)
    member x.Capabilities = x.Capabilities
    /// User provided initialization options.
    member x.InitializationOptions = x.InitializationOptions
    /// The initial trace setting. If omitted trace is disabled ('off').
    member x.Trace = x.Trace

  interface IWorkDoneProgressParams with
    /// An optional token that a server can use to report work done progress.
    member x.WorkDoneToken = x.WorkDoneToken

type WorkspaceFoldersInitializeParams = {
  /// The workspace folders configured in the client when the server starts.
  ///
  /// This property is only available if the client supports workspace folders.
  /// It can be `null` if the client supports workspace folders but none are
  /// configured.
  ///
  /// @since 3.6.0
  WorkspaceFolders: WorkspaceFolder[] option
} with

  interface IWorkspaceFoldersInitializeParams with
    /// The workspace folders configured in the client when the server starts.
    ///
    /// This property is only available if the client supports workspace folders.
    /// It can be `null` if the client supports workspace folders but none are
    /// configured.
    ///
    /// @since 3.6.0
    member x.WorkspaceFolders = x.WorkspaceFolders

type ServerCapabilitiesWorkspace = {
  /// The server supports workspace folder.
  ///
  /// @since 3.6.0
  WorkspaceFolders: WorkspaceFoldersServerCapabilities option
  /// The server is interested in notifications/requests for operations on files.
  ///
  /// @since 3.16.0
  FileOperations: FileOperationOptions option
}

/// Defines the capabilities provided by a language
/// server.
type ServerCapabilities = {
  /// The position encoding the server picked from the encodings offered
  /// by the client via the client capability `general.positionEncodings`.
  ///
  /// If the client didn't provide any position encodings the only valid
  /// value that a server can return is 'utf-16'.
  ///
  /// If omitted it defaults to 'utf-16'.
  ///
  /// @since 3.17.0
  PositionEncoding: PositionEncodingKind option
  /// Defines how text documents are synced. Is either a detailed structure
  /// defining each notification or for backwards compatibility the
  /// TextDocumentSyncKind number.
  TextDocumentSync: U2<TextDocumentSyncOptions, TextDocumentSyncKind> option
  /// Defines how notebook documents are synced.
  ///
  /// @since 3.17.0
  NotebookDocumentSync: U2<NotebookDocumentSyncOptions, NotebookDocumentSyncRegistrationOptions> option
  /// The server provides completion support.
  CompletionProvider: CompletionOptions option
  /// The server provides hover support.
  HoverProvider: U2<bool, HoverOptions> option
  /// The server provides signature help support.
  SignatureHelpProvider: SignatureHelpOptions option
  /// The server provides Goto Declaration support.
  DeclarationProvider: U3<bool, DeclarationOptions, DeclarationRegistrationOptions> option
  /// The server provides goto definition support.
  DefinitionProvider: U2<bool, DefinitionOptions> option
  /// The server provides Goto Type Definition support.
  TypeDefinitionProvider: U3<bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions> option
  /// The server provides Goto Implementation support.
  ImplementationProvider: U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option
  /// The server provides find references support.
  ReferencesProvider: U2<bool, ReferenceOptions> option
  /// The server provides document highlight support.
  DocumentHighlightProvider: U2<bool, DocumentHighlightOptions> option
  /// The server provides document symbol support.
  DocumentSymbolProvider: U2<bool, DocumentSymbolOptions> option
  /// The server provides code actions. CodeActionOptions may only be
  /// specified if the client states that it supports
  /// `codeActionLiteralSupport` in its initial `initialize` request.
  CodeActionProvider: U2<bool, CodeActionOptions> option
  /// The server provides code lens.
  CodeLensProvider: CodeLensOptions option
  /// The server provides document link support.
  DocumentLinkProvider: DocumentLinkOptions option
  /// The server provides color provider support.
  ColorProvider: U3<bool, DocumentColorOptions, DocumentColorRegistrationOptions> option
  /// The server provides workspace symbol support.
  WorkspaceSymbolProvider: U2<bool, WorkspaceSymbolOptions> option
  /// The server provides document formatting.
  DocumentFormattingProvider: U2<bool, DocumentFormattingOptions> option
  /// The server provides document range formatting.
  DocumentRangeFormattingProvider: U2<bool, DocumentRangeFormattingOptions> option
  /// The server provides document formatting on typing.
  DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option
  /// The server provides rename support. RenameOptions may only be
  /// specified if the client states that it supports
  /// `prepareSupport` in its initial `initialize` request.
  RenameProvider: U2<bool, RenameOptions> option
  /// The server provides folding provider support.
  FoldingRangeProvider: U3<bool, FoldingRangeOptions, FoldingRangeRegistrationOptions> option
  /// The server provides selection range support.
  SelectionRangeProvider: U3<bool, SelectionRangeOptions, SelectionRangeRegistrationOptions> option
  /// The server provides execute command support.
  ExecuteCommandProvider: ExecuteCommandOptions option
  /// The server provides call hierarchy support.
  ///
  /// @since 3.16.0
  CallHierarchyProvider: U3<bool, CallHierarchyOptions, CallHierarchyRegistrationOptions> option
  /// The server provides linked editing range support.
  ///
  /// @since 3.16.0
  LinkedEditingRangeProvider: U3<bool, LinkedEditingRangeOptions, LinkedEditingRangeRegistrationOptions> option
  /// The server provides semantic tokens support.
  ///
  /// @since 3.16.0
  SemanticTokensProvider: U2<SemanticTokensOptions, SemanticTokensRegistrationOptions> option
  /// The server provides moniker support.
  ///
  /// @since 3.16.0
  MonikerProvider: U3<bool, MonikerOptions, MonikerRegistrationOptions> option
  /// The server provides type hierarchy support.
  ///
  /// @since 3.17.0
  TypeHierarchyProvider: U3<bool, TypeHierarchyOptions, TypeHierarchyRegistrationOptions> option
  /// The server provides inline values.
  ///
  /// @since 3.17.0
  InlineValueProvider: U3<bool, InlineValueOptions, InlineValueRegistrationOptions> option
  /// The server provides inlay hints.
  ///
  /// @since 3.17.0
  InlayHintProvider: U3<bool, InlayHintOptions, InlayHintRegistrationOptions> option
  /// The server has support for pull model diagnostics.
  ///
  /// @since 3.17.0
  DiagnosticProvider: U2<DiagnosticOptions, DiagnosticRegistrationOptions> option
  /// Workspace specific server capabilities.
  Workspace: ServerCapabilitiesWorkspace option
  /// Experimental server capabilities.
  Experimental: LSPAny option
}

/// A text document identifier to denote a specific version of a text document.
type VersionedTextDocumentIdentifier = {
  /// The text document's uri.
  Uri: DocumentUri
  /// The version number of this document.
  Version: int32
} with

  interface ITextDocumentIdentifier with
    /// The text document's uri.
    member x.Uri = x.Uri

/// Save options.
type SaveOptions = {
  /// The client is supposed to include the content on save.
  IncludeText: bool option
} with

  interface ISaveOptions with
    /// The client is supposed to include the content on save.
    member x.IncludeText = x.IncludeText

/// An event describing a file change.
type FileEvent = {
  /// The file's uri.
  Uri: DocumentUri
  /// The change type.
  Type: FileChangeType
}

type FileSystemWatcher = {
  /// The glob pattern to watch. See {@link GlobPattern glob pattern} for more detail.
  ///
  /// @since 3.17.0 support for relative patterns.
  GlobPattern: GlobPattern
  /// The kind of events of interest. If omitted it defaults
  /// to WatchKind.Create | WatchKind.Change | WatchKind.Delete
  /// which is 7.
  Kind: WatchKind option
}

/// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
/// are only valid in the scope of a resource.
[<DebuggerDisplay("{DebuggerDisplay}")>]
type Diagnostic = {
  /// The range at which the message applies
  Range: Range
  /// The diagnostic's severity. Can be omitted. If omitted it is up to the
  /// client to interpret diagnostics as error, warning, info or hint.
  Severity: DiagnosticSeverity option
  /// The diagnostic's code, which usually appear in the user interface.
  Code: U2<int32, string> option
  /// An optional property to describe the error code.
  /// Requires the code field (above) to be present/not null.
  ///
  /// @since 3.16.0
  CodeDescription: CodeDescription option
  /// A human-readable string describing the source of this
  /// diagnostic, e.g. 'typescript' or 'super lint'. It usually
  /// appears in the user interface.
  Source: string option
  /// The diagnostic's message. It usually appears in the user interface
  Message: string
  /// Additional metadata about the diagnostic.
  ///
  /// @since 3.15.0
  Tags: DiagnosticTag[] option
  /// An array of related diagnostic information, e.g. when symbol-names within
  /// a scope collide all definitions can be marked via this property.
  RelatedInformation: DiagnosticRelatedInformation[] option
  /// A data entry field that is preserved between a `textDocument/publishDiagnostics`
  /// notification and `textDocument/codeAction` request.
  ///
  /// @since 3.16.0
  Data: LSPAny option
} with

  [<DebuggerBrowsable(DebuggerBrowsableState.Never); JsonIgnore>]
  member x.DebuggerDisplay =
    $"[{defaultArg x.Severity DiagnosticSeverity.Error}] ({x.Range.DebuggerDisplay}) {x.Message} ({Option.map string x.Code |> Option.defaultValue String.Empty})"

/// Contains additional information about the context in which a completion request is triggered.
type CompletionContext = {
  /// How the completion was triggered.
  TriggerKind: CompletionTriggerKind
  /// The trigger character (a single character) that has trigger code complete.
  /// Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
  TriggerCharacter: string option
}

/// Additional details for a completion item label.
///
/// @since 3.17.0
type CompletionItemLabelDetails = {
  /// An optional string which is rendered less prominently directly after {@link CompletionItem.label label},
  /// without any spacing. Should be used for function signatures and type annotations.
  Detail: string option
  /// An optional string which is rendered less prominently after {@link CompletionItem.detail}. Should be used
  /// for fully qualified names and file paths.
  Description: string option
}

/// A special text edit to provide an insert and a replace operation.
///
/// @since 3.16.0
type InsertReplaceEdit = {
  /// The string to be inserted.
  NewText: string
  /// The range if the insert is requested
  Insert: Range
  /// The range if the replace is requested.
  Replace: Range
}

/// Completion options.
type CompletionOptions = {
  WorkDoneProgress: bool option
  /// Most tools trigger completion request automatically without explicitly requesting
  /// it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
  /// starts to type an identifier. For example if the user types `c` in a JavaScript file
  /// code complete will automatically pop up present `console` besides others as a
  /// completion item. Characters that make up identifiers don't need to be listed here.
  ///
  /// If code complete should automatically be trigger on characters not being valid inside
  /// an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
  TriggerCharacters: string[] option
  /// The list of all possible characters that commit a completion. This field can be used
  /// if clients don't support individual commit characters per completion item. See
  /// `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
  ///
  /// If a server provides both `allCommitCharacters` and commit characters on an individual
  /// completion item the ones on the completion item win.
  ///
  /// @since 3.2.0
  AllCommitCharacters: string[] option
  /// The server provides support to resolve additional
  /// information for a completion item.
  ResolveProvider: bool option
  /// The server supports the following `CompletionItem` specific
  /// capabilities.
  ///
  /// @since 3.17.0
  CompletionItem: CompletionOptionsCompletionItem option
} with

  interface ICompletionOptions with
    /// Most tools trigger completion request automatically without explicitly requesting
    /// it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
    /// starts to type an identifier. For example if the user types `c` in a JavaScript file
    /// code complete will automatically pop up present `console` besides others as a
    /// completion item. Characters that make up identifiers don't need to be listed here.
    ///
    /// If code complete should automatically be trigger on characters not being valid inside
    /// an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
    member x.TriggerCharacters = x.TriggerCharacters
    /// The list of all possible characters that commit a completion. This field can be used
    /// if clients don't support individual commit characters per completion item. See
    /// `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
    ///
    /// If a server provides both `allCommitCharacters` and commit characters on an individual
    /// completion item the ones on the completion item win.
    ///
    /// @since 3.2.0
    member x.AllCommitCharacters = x.AllCommitCharacters
    /// The server provides support to resolve additional
    /// information for a completion item.
    member x.ResolveProvider = x.ResolveProvider
    /// The server supports the following `CompletionItem` specific
    /// capabilities.
    ///
    /// @since 3.17.0
    member x.CompletionItem = x.CompletionItem

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Hover options.
type HoverOptions = {
  WorkDoneProgress: bool option
} with

  interface IHoverOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Additional information about the context in which a signature help request was triggered.
///
/// @since 3.15.0
type SignatureHelpContext = {
  /// Action that caused signature help to be triggered.
  TriggerKind: SignatureHelpTriggerKind
  /// Character that caused signature help to be triggered.
  ///
  /// This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`
  TriggerCharacter: string option
  /// `true` if signature help was already showing when it was triggered.
  ///
  /// Retriggers occurs when the signature help is already active and can be caused by actions such as
  /// typing a trigger character, a cursor move, or document content changes.
  IsRetrigger: bool
  /// The currently active `SignatureHelp`.
  ///
  /// The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
  /// the user navigating through available signatures.
  ActiveSignatureHelp: SignatureHelp option
}

/// Represents the signature of something callable. A signature
/// can have a label, like a function-name, a doc-comment, and
/// a set of parameters.
type SignatureInformation = {
  /// The label of this signature. Will be shown in
  /// the UI.
  Label: string
  /// The human-readable doc-comment of this signature. Will be shown
  /// in the UI but can be omitted.
  Documentation: U2<string, MarkupContent> option
  /// The parameters of this signature.
  Parameters: ParameterInformation[] option
  /// The index of the active parameter.
  ///
  /// If provided, this is used in place of `SignatureHelp.activeParameter`.
  ///
  /// @since 3.16.0
  ActiveParameter: uint32 option
}

/// Server Capabilities for a {@link SignatureHelpRequest}.
type SignatureHelpOptions = {
  WorkDoneProgress: bool option
  /// List of characters that trigger signature help automatically.
  TriggerCharacters: string[] option
  /// List of characters that re-trigger signature help.
  ///
  /// These trigger characters are only active when signature help is already showing. All trigger characters
  /// are also counted as re-trigger characters.
  ///
  /// @since 3.15.0
  RetriggerCharacters: string[] option
} with

  interface ISignatureHelpOptions with
    /// List of characters that trigger signature help automatically.
    member x.TriggerCharacters = x.TriggerCharacters
    /// List of characters that re-trigger signature help.
    ///
    /// These trigger characters are only active when signature help is already showing. All trigger characters
    /// are also counted as re-trigger characters.
    ///
    /// @since 3.15.0
    member x.RetriggerCharacters = x.RetriggerCharacters

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Server Capabilities for a {@link DefinitionRequest}.
type DefinitionOptions = {
  WorkDoneProgress: bool option
} with

  interface IDefinitionOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Value-object that contains additional information when
/// requesting references.
type ReferenceContext = {
  /// Include the declaration of the current symbol.
  IncludeDeclaration: bool
}

/// Reference options.
type ReferenceOptions = {
  WorkDoneProgress: bool option
} with

  interface IReferenceOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Provider options for a {@link DocumentHighlightRequest}.
type DocumentHighlightOptions = {
  WorkDoneProgress: bool option
} with

  interface IDocumentHighlightOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// A base for all symbol information.
type BaseSymbolInformation = {
  /// The name of this symbol.
  Name: string
  /// The kind of this symbol.
  Kind: SymbolKind
  /// Tags for this symbol.
  ///
  /// @since 3.16.0
  Tags: SymbolTag[] option
  /// The name of the symbol containing this symbol. This information is for
  /// user interface purposes (e.g. to render a qualifier in the user interface
  /// if necessary). It can't be used to re-infer a hierarchy for the document
  /// symbols.
  ContainerName: string option
} with

  interface IBaseSymbolInformation with
    /// The name of this symbol.
    member x.Name = x.Name
    /// The kind of this symbol.
    member x.Kind = x.Kind
    /// Tags for this symbol.
    ///
    /// @since 3.16.0
    member x.Tags = x.Tags
    /// The name of the symbol containing this symbol. This information is for
    /// user interface purposes (e.g. to render a qualifier in the user interface
    /// if necessary). It can't be used to re-infer a hierarchy for the document
    /// symbols.
    member x.ContainerName = x.ContainerName

/// Provider options for a {@link DocumentSymbolRequest}.
type DocumentSymbolOptions = {
  WorkDoneProgress: bool option
  /// A human-readable string that is shown when multiple outlines trees
  /// are shown for the same document.
  ///
  /// @since 3.16.0
  Label: string option
} with

  interface IDocumentSymbolOptions with
    /// A human-readable string that is shown when multiple outlines trees
    /// are shown for the same document.
    ///
    /// @since 3.16.0
    member x.Label = x.Label

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Contains additional diagnostic information about the context in which
/// a {@link CodeActionProvider.provideCodeActions code action} is run.
type CodeActionContext = {
  /// An array of diagnostics known on the client side overlapping the range provided to the
  /// `textDocument/codeAction` request. They are provided so that the server knows which
  /// errors are currently presented to the user for the given range. There is no guarantee
  /// that these accurately reflect the error state of the resource. The primary parameter
  /// to compute code actions is the provided range.
  Diagnostics: Diagnostic[]
  /// Requested kind of actions to return.
  ///
  /// Actions not of this kind are filtered out by the client before being shown. So servers
  /// can omit computing them.
  Only: CodeActionKind[] option
  /// The reason why code actions were requested.
  ///
  /// @since 3.17.0
  TriggerKind: CodeActionTriggerKind option
}

/// Provider options for a {@link CodeActionRequest}.
type CodeActionOptions = {
  WorkDoneProgress: bool option
  /// CodeActionKinds that this server may return.
  ///
  /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  /// may list out every specific kind they provide.
  CodeActionKinds: CodeActionKind[] option
  /// The server provides support to resolve additional
  /// information for a code action.
  ///
  /// @since 3.16.0
  ResolveProvider: bool option
} with

  interface ICodeActionOptions with
    /// CodeActionKinds that this server may return.
    ///
    /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    /// may list out every specific kind they provide.
    member x.CodeActionKinds = x.CodeActionKinds
    /// The server provides support to resolve additional
    /// information for a code action.
    ///
    /// @since 3.16.0
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Server capabilities for a {@link WorkspaceSymbolRequest}.
type WorkspaceSymbolOptions = {
  WorkDoneProgress: bool option
  /// The server provides support to resolve additional
  /// information for a workspace symbol.
  ///
  /// @since 3.17.0
  ResolveProvider: bool option
} with

  interface IWorkspaceSymbolOptions with
    /// The server provides support to resolve additional
    /// information for a workspace symbol.
    ///
    /// @since 3.17.0
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Code Lens provider options of a {@link CodeLensRequest}.
type CodeLensOptions = {
  WorkDoneProgress: bool option
  /// Code lens has a resolve provider as well.
  ResolveProvider: bool option
} with

  interface ICodeLensOptions with
    /// Code lens has a resolve provider as well.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Provider options for a {@link DocumentLinkRequest}.
type DocumentLinkOptions = {
  WorkDoneProgress: bool option
  /// Document links have a resolve provider as well.
  ResolveProvider: bool option
} with

  interface IDocumentLinkOptions with
    /// Document links have a resolve provider as well.
    member x.ResolveProvider = x.ResolveProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Value-object describing what options formatting should use.
type FormattingOptions = {
  /// Size of a tab in spaces.
  TabSize: uint32
  /// Prefer spaces over tabs.
  InsertSpaces: bool
  /// Trim trailing whitespace on a line.
  ///
  /// @since 3.15.0
  TrimTrailingWhitespace: bool option
  /// Insert a newline character at the end of the file if one does not exist.
  ///
  /// @since 3.15.0
  InsertFinalNewline: bool option
  /// Trim all newlines after the final newline at the end of the file.
  ///
  /// @since 3.15.0
  TrimFinalNewlines: bool option
}

/// Provider options for a {@link DocumentFormattingRequest}.
type DocumentFormattingOptions = {
  WorkDoneProgress: bool option
} with

  interface IDocumentFormattingOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Provider options for a {@link DocumentRangeFormattingRequest}.
type DocumentRangeFormattingOptions = {
  WorkDoneProgress: bool option
} with

  interface IDocumentRangeFormattingOptions

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// Provider options for a {@link DocumentOnTypeFormattingRequest}.
type DocumentOnTypeFormattingOptions = {
  /// A character on which formatting should be triggered, like `{`.
  FirstTriggerCharacter: string
  /// More trigger characters.
  MoreTriggerCharacter: string[] option
} with

  interface IDocumentOnTypeFormattingOptions with
    /// A character on which formatting should be triggered, like `{`.
    member x.FirstTriggerCharacter = x.FirstTriggerCharacter
    /// More trigger characters.
    member x.MoreTriggerCharacter = x.MoreTriggerCharacter

/// Provider options for a {@link RenameRequest}.
type RenameOptions = {
  WorkDoneProgress: bool option
  /// Renames should be checked and tested before being executed.
  ///
  /// @since version 3.12.0
  PrepareProvider: bool option
} with

  interface IRenameOptions with
    /// Renames should be checked and tested before being executed.
    ///
    /// @since version 3.12.0
    member x.PrepareProvider = x.PrepareProvider

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// The server capabilities of a {@link ExecuteCommandRequest}.
type ExecuteCommandOptions = {
  WorkDoneProgress: bool option
  /// The commands to be executed on the server
  Commands: string[]
} with

  interface IExecuteCommandOptions with
    /// The commands to be executed on the server
    member x.Commands = x.Commands

  interface IWorkDoneProgressOptions with
    member x.WorkDoneProgress = x.WorkDoneProgress

/// @since 3.16.0
type SemanticTokensLegend = {
  /// The token types a server uses.
  TokenTypes: string[]
  /// The token modifiers a server uses.
  TokenModifiers: string[]
}

/// A text document identifier to optionally denote a specific version of a text document.
type OptionalVersionedTextDocumentIdentifier = {
  /// The text document's uri.
  Uri: DocumentUri
  /// The version number of this document. If a versioned text document identifier
  /// is sent from the server to the client and the file is not open in the editor
  /// (the server has not received an open notification before) the server can send
  /// `null` to indicate that the version is unknown and the content on disk is the
  /// truth (as specified with document content ownership).
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  Version: int32 option
} with

  interface ITextDocumentIdentifier with
    /// The text document's uri.
    member x.Uri = x.Uri

/// A special text edit with an additional change annotation.
///
/// @since 3.16.0.
type AnnotatedTextEdit = {
  /// The range of the text document to be manipulated. To insert
  /// text into a document create a range where start === end.
  Range: Range
  /// The string to be inserted. For delete operations use an
  /// empty string.
  NewText: string
  /// The actual identifier of the change annotation
  AnnotationId: ChangeAnnotationIdentifier
} with

  interface ITextEdit with
    /// The range of the text document to be manipulated. To insert
    /// text into a document create a range where start === end.
    member x.Range = x.Range
    /// The string to be inserted. For delete operations use an
    /// empty string.
    member x.NewText = x.NewText

/// A generic resource operation.
type ResourceOperation = {
  /// The resource operation kind.
  Kind: string
  /// An optional annotation identifier describing the operation.
  ///
  /// @since 3.16.0
  AnnotationId: ChangeAnnotationIdentifier option
} with

  interface IResourceOperation with
    /// The resource operation kind.
    member x.Kind = x.Kind
    /// An optional annotation identifier describing the operation.
    ///
    /// @since 3.16.0
    member x.AnnotationId = x.AnnotationId

/// Options to create a file.
type CreateFileOptions = {
  /// Overwrite existing file. Overwrite wins over `ignoreIfExists`
  Overwrite: bool option
  /// Ignore if exists.
  IgnoreIfExists: bool option
}

/// Rename file options
type RenameFileOptions = {
  /// Overwrite target if existing. Overwrite wins over `ignoreIfExists`
  Overwrite: bool option
  /// Ignores if target exists.
  IgnoreIfExists: bool option
}

/// Delete file options
type DeleteFileOptions = {
  /// Delete the content recursively if a folder is denoted.
  Recursive: bool option
  /// Ignore the operation if the file doesn't exist.
  IgnoreIfNotExists: bool option
}

/// A pattern to describe in which file operation requests or notifications
/// the server is interested in receiving.
///
/// @since 3.16.0
type FileOperationPattern = {
  /// The glob pattern to match. Glob patterns can have the following syntax:
  /// - `*` to match one or more characters in a path segment
  /// - `?` to match on one character in a path segment
  /// - `**` to match any number of path segments, including none
  /// - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
  /// - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
  /// - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
  Glob: string
  /// Whether to match files or folders with this pattern.
  ///
  /// Matches both if undefined.
  Matches: FileOperationPatternKind option
  /// Additional options used during matching.
  Options: FileOperationPatternOptions option
}

/// A full document diagnostic report for a workspace diagnostic result.
///
/// @since 3.17.0
type WorkspaceFullDocumentDiagnosticReport = {
  /// A full document diagnostic report.
  [<UnionKindAttribute("full")>]
  Kind: string
  /// An optional result id. If provided it will
  /// be sent on the next diagnostic request for the
  /// same document.
  ResultId: string option
  /// The actual items.
  Items: Diagnostic[]
  /// The URI for which diagnostic information is reported.
  Uri: DocumentUri
  /// The version number for which the diagnostics are reported.
  /// If the document is not marked as open `null` can be provided.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  Version: int32 option
} with

  interface IFullDocumentDiagnosticReport with
    /// A full document diagnostic report.
    member x.Kind = x.Kind
    /// An optional result id. If provided it will
    /// be sent on the next diagnostic request for the
    /// same document.
    member x.ResultId = x.ResultId
    /// The actual items.
    member x.Items = x.Items

/// An unchanged document diagnostic report for a workspace diagnostic result.
///
/// @since 3.17.0
type WorkspaceUnchangedDocumentDiagnosticReport = {
  /// A document diagnostic report indicating
  /// no changes to the last result. A server can
  /// only return `unchanged` if result ids are
  /// provided.
  [<UnionKindAttribute("unchanged")>]
  Kind: string
  /// A result id which will be sent on the next
  /// diagnostic request for the same document.
  ResultId: string
  /// The URI for which diagnostic information is reported.
  Uri: DocumentUri
  /// The version number for which the diagnostics are reported.
  /// If the document is not marked as open `null` can be provided.
  [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
  Version: int32 option
} with

  interface IUnchangedDocumentDiagnosticReport with
    /// A document diagnostic report indicating
    /// no changes to the last result. A server can
    /// only return `unchanged` if result ids are
    /// provided.
    member x.Kind = x.Kind
    /// A result id which will be sent on the next
    /// diagnostic request for the same document.
    member x.ResultId = x.ResultId

/// A notebook cell.
///
/// A cell's document URI must be unique across ALL notebook
/// cells and can therefore be used to uniquely identify a
/// notebook cell or the cell's text document.
///
/// @since 3.17.0
type NotebookCell = {
  /// The cell's kind
  Kind: NotebookCellKind
  /// The URI of the cell's text document
  /// content.
  Document: DocumentUri
  /// Additional metadata stored with the cell.
  ///
  /// Note: should always be an object literal (e.g. LSPObject)
  Metadata: LSPObject option
  /// Additional execution summary information
  /// if supported by the client.
  ExecutionSummary: ExecutionSummary option
}

/// A change describing how to move a `NotebookCell`
/// array from state S to S'.
///
/// @since 3.17.0
type NotebookCellArrayChange = {
  /// The start oftest of the cell that changed.
  Start: uint32
  /// The deleted cells
  DeleteCount: uint32
  /// The new cells, if any
  Cells: NotebookCell[] option
}

/// Defines the capabilities provided by the client.
type ClientCapabilities = {
  /// Workspace specific client capabilities.
  Workspace: WorkspaceClientCapabilities option
  /// Text document specific client capabilities.
  TextDocument: TextDocumentClientCapabilities option
  /// Capabilities specific to the notebook document support.
  ///
  /// @since 3.17.0
  NotebookDocument: NotebookDocumentClientCapabilities option
  /// Window specific client capabilities.
  Window: WindowClientCapabilities option
  /// General client capabilities.
  ///
  /// @since 3.16.0
  General: GeneralClientCapabilities option
  /// Experimental client capabilities.
  Experimental: LSPAny option
}

type TextDocumentSyncOptions = {
  /// Open and close notifications are sent to the server. If omitted open close notification should not
  /// be sent.
  OpenClose: bool option
  /// Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
  /// and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.
  Change: TextDocumentSyncKind option
  /// If present will save notifications are sent to the server. If omitted the notification should not be
  /// sent.
  WillSave: bool option
  /// If present will save wait until requests are sent to the server. If omitted the request should not be
  /// sent.
  WillSaveWaitUntil: bool option
  /// If present save notifications are sent to the server. If omitted the notification should not be
  /// sent.
  Save: U2<bool, SaveOptions> option
}

type NotebookDocumentSyncOptionsNotebookSelector = {
  /// The notebook to be synced If a string
  /// value is provided it matches against the
  /// notebook type. '*' matches every notebook.
  Notebook: U2<string, NotebookDocumentFilter> option
  /// The cells of the matching notebook to be synced.
  Cells: NotebookDocumentSyncOptionsNotebookSelectorCells[] option
}

type NotebookDocumentSyncOptionsNotebookSelectorCells = { Language: string }

/// Options specific to a notebook plus its cells
/// to be synced to the server.
///
/// If a selector provides a notebook document
/// filter but no cell selector all cells of a
/// matching notebook document will be synced.
///
/// If a selector provides no notebook document
/// filter but only a cell selector all notebook
/// document that contain at least one matching
/// cell will be synced.
///
/// @since 3.17.0
type NotebookDocumentSyncOptions = {
  /// The notebooks to be synced
  NotebookSelector: NotebookDocumentSyncOptionsNotebookSelector[]
  /// Whether save notification should be forwarded to
  /// the server. Will only be honored if mode === `notebook`.
  Save: bool option
} with

  interface INotebookDocumentSyncOptions with
    /// The notebooks to be synced
    member x.NotebookSelector = x.NotebookSelector
    /// Whether save notification should be forwarded to
    /// the server. Will only be honored if mode === `notebook`.
    member x.Save = x.Save

/// Registration options specific to a notebook.
///
/// @since 3.17.0
type NotebookDocumentSyncRegistrationOptions = {
  /// The notebooks to be synced
  NotebookSelector: NotebookDocumentSyncOptionsNotebookSelector[]
  /// Whether save notification should be forwarded to
  /// the server. Will only be honored if mode === `notebook`.
  Save: bool option
  /// The id used to register the request. The id can be used to deregister
  /// the request again. See also Registration#id.
  Id: string option
} with

  interface INotebookDocumentSyncOptions with
    /// The notebooks to be synced
    member x.NotebookSelector = x.NotebookSelector
    /// Whether save notification should be forwarded to
    /// the server. Will only be honored if mode === `notebook`.
    member x.Save = x.Save

type WorkspaceFoldersServerCapabilities = {
  /// The server has support for workspace folders
  Supported: bool option
  /// Whether the server wants to receive workspace folder
  /// change notifications.
  ///
  /// If a string is provided the string is treated as an ID
  /// under which the notification is registered on the client
  /// side. The ID can be used to unregister for these events
  /// using the `client/unregisterCapability` request.
  ChangeNotifications: U2<string, bool> option
}

/// Options for notifications/requests for user operations on files.
///
/// @since 3.16.0
type FileOperationOptions = {
  /// The server is interested in receiving didCreateFiles notifications.
  DidCreate: FileOperationRegistrationOptions option
  /// The server is interested in receiving willCreateFiles requests.
  WillCreate: FileOperationRegistrationOptions option
  /// The server is interested in receiving didRenameFiles notifications.
  DidRename: FileOperationRegistrationOptions option
  /// The server is interested in receiving willRenameFiles requests.
  WillRename: FileOperationRegistrationOptions option
  /// The server is interested in receiving didDeleteFiles file notifications.
  DidDelete: FileOperationRegistrationOptions option
  /// The server is interested in receiving willDeleteFiles file requests.
  WillDelete: FileOperationRegistrationOptions option
}

/// Structure to capture a description for an error code.
///
/// @since 3.16.0
type CodeDescription = {
  /// An URI to open with more information about the diagnostic error.
  Href: URI
}

/// Represents a related message and source code location for a diagnostic. This should be
/// used to point to code locations that cause or related to a diagnostics, e.g when duplicating
/// a symbol in a scope.
type DiagnosticRelatedInformation = {
  /// The location of this related diagnostic information.
  Location: Location
  /// The message of this related diagnostic information.
  Message: string
}

/// Represents a parameter of a callable-signature. A parameter can
/// have a label and a doc-comment.
type ParameterInformation = {
  /// The label of this parameter information.
  ///
  /// Either a string or an inclusive start and exclusive end offsets within its containing
  /// signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
  /// string representation as `Position` and `Range` does.
  ///
  /// *Note*: a label of type string should be a substring of its containing signature label.
  /// Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.
  Label: U2<string, uint32 * uint32>
  /// The human-readable doc-comment of this parameter. Will be shown
  /// in the UI but can be omitted.
  Documentation: U2<string, MarkupContent> option
}

/// A notebook cell text document filter denotes a cell text
/// document by different properties.
///
/// @since 3.17.0
type NotebookCellTextDocumentFilter = {
  /// A filter that matches against the notebook
  /// containing the notebook cell. If a string
  /// value is provided it matches against the
  /// notebook type. '*' matches every notebook.
  Notebook: U2<string, NotebookDocumentFilter>
  /// A language id like `python`.
  ///
  /// Will be matched against the language id of the
  /// notebook cell document. '*' matches every language.
  Language: string option
}

/// Matching options for the file operation pattern.
///
/// @since 3.16.0
type FileOperationPatternOptions = {
  /// The pattern should be matched ignoring casing.
  IgnoreCase: bool option
}

type ExecutionSummary = {
  /// A strict monotonically increasing value
  /// indicating the execution order of a cell
  /// inside a notebook.
  ExecutionOrder: uint32
  /// Whether the execution was successful or
  /// not if known by the client.
  Success: bool option
}

/// Workspace specific client capabilities.
type WorkspaceClientCapabilities = {
  /// The client supports applying batch edits
  /// to the workspace by supporting the request
  /// 'workspace/applyEdit'
  ApplyEdit: bool option
  /// Capabilities specific to `WorkspaceEdit`s.
  WorkspaceEdit: WorkspaceEditClientCapabilities option
  /// Capabilities specific to the `workspace/didChangeConfiguration` notification.
  DidChangeConfiguration: DidChangeConfigurationClientCapabilities option
  /// Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
  DidChangeWatchedFiles: DidChangeWatchedFilesClientCapabilities option
  /// Capabilities specific to the `workspace/symbol` request.
  Symbol: WorkspaceSymbolClientCapabilities option
  /// Capabilities specific to the `workspace/executeCommand` request.
  ExecuteCommand: ExecuteCommandClientCapabilities option
  /// The client has support for workspace folders.
  ///
  /// @since 3.6.0
  WorkspaceFolders: bool option
  /// The client supports `workspace/configuration` requests.
  ///
  /// @since 3.6.0
  Configuration: bool option
  /// Capabilities specific to the semantic token requests scoped to the
  /// workspace.
  ///
  /// @since 3.16.0.
  SemanticTokens: SemanticTokensWorkspaceClientCapabilities option
  /// Capabilities specific to the code lens requests scoped to the
  /// workspace.
  ///
  /// @since 3.16.0.
  CodeLens: CodeLensWorkspaceClientCapabilities option
  /// The client has support for file notifications/requests for user operations on files.
  ///
  /// Since 3.16.0
  FileOperations: FileOperationClientCapabilities option
  /// Capabilities specific to the inline values requests scoped to the
  /// workspace.
  ///
  /// @since 3.17.0.
  InlineValue: InlineValueWorkspaceClientCapabilities option
  /// Capabilities specific to the inlay hint requests scoped to the
  /// workspace.
  ///
  /// @since 3.17.0.
  InlayHint: InlayHintWorkspaceClientCapabilities option
  /// Capabilities specific to the diagnostic requests scoped to the
  /// workspace.
  ///
  /// @since 3.17.0.
  Diagnostics: DiagnosticWorkspaceClientCapabilities option
}

/// Text document specific client capabilities.
type TextDocumentClientCapabilities = {
  /// Defines which synchronization capabilities the client supports.
  Synchronization: TextDocumentSyncClientCapabilities option
  /// Capabilities specific to the `textDocument/completion` request.
  Completion: CompletionClientCapabilities option
  /// Capabilities specific to the `textDocument/hover` request.
  Hover: HoverClientCapabilities option
  /// Capabilities specific to the `textDocument/signatureHelp` request.
  SignatureHelp: SignatureHelpClientCapabilities option
  /// Capabilities specific to the `textDocument/declaration` request.
  ///
  /// @since 3.14.0
  Declaration: DeclarationClientCapabilities option
  /// Capabilities specific to the `textDocument/definition` request.
  Definition: DefinitionClientCapabilities option
  /// Capabilities specific to the `textDocument/typeDefinition` request.
  ///
  /// @since 3.6.0
  TypeDefinition: TypeDefinitionClientCapabilities option
  /// Capabilities specific to the `textDocument/implementation` request.
  ///
  /// @since 3.6.0
  Implementation: ImplementationClientCapabilities option
  /// Capabilities specific to the `textDocument/references` request.
  References: ReferenceClientCapabilities option
  /// Capabilities specific to the `textDocument/documentHighlight` request.
  DocumentHighlight: DocumentHighlightClientCapabilities option
  /// Capabilities specific to the `textDocument/documentSymbol` request.
  DocumentSymbol: DocumentSymbolClientCapabilities option
  /// Capabilities specific to the `textDocument/codeAction` request.
  CodeAction: CodeActionClientCapabilities option
  /// Capabilities specific to the `textDocument/codeLens` request.
  CodeLens: CodeLensClientCapabilities option
  /// Capabilities specific to the `textDocument/documentLink` request.
  DocumentLink: DocumentLinkClientCapabilities option
  /// Capabilities specific to the `textDocument/documentColor` and the
  /// `textDocument/colorPresentation` request.
  ///
  /// @since 3.6.0
  ColorProvider: DocumentColorClientCapabilities option
  /// Capabilities specific to the `textDocument/formatting` request.
  Formatting: DocumentFormattingClientCapabilities option
  /// Capabilities specific to the `textDocument/rangeFormatting` request.
  RangeFormatting: DocumentRangeFormattingClientCapabilities option
  /// Capabilities specific to the `textDocument/onTypeFormatting` request.
  OnTypeFormatting: DocumentOnTypeFormattingClientCapabilities option
  /// Capabilities specific to the `textDocument/rename` request.
  Rename: RenameClientCapabilities option
  /// Capabilities specific to the `textDocument/foldingRange` request.
  ///
  /// @since 3.10.0
  FoldingRange: FoldingRangeClientCapabilities option
  /// Capabilities specific to the `textDocument/selectionRange` request.
  ///
  /// @since 3.15.0
  SelectionRange: SelectionRangeClientCapabilities option
  /// Capabilities specific to the `textDocument/publishDiagnostics` notification.
  PublishDiagnostics: PublishDiagnosticsClientCapabilities option
  /// Capabilities specific to the various call hierarchy requests.
  ///
  /// @since 3.16.0
  CallHierarchy: CallHierarchyClientCapabilities option
  /// Capabilities specific to the various semantic token request.
  ///
  /// @since 3.16.0
  SemanticTokens: SemanticTokensClientCapabilities option
  /// Capabilities specific to the `textDocument/linkedEditingRange` request.
  ///
  /// @since 3.16.0
  LinkedEditingRange: LinkedEditingRangeClientCapabilities option
  /// Client capabilities specific to the `textDocument/moniker` request.
  ///
  /// @since 3.16.0
  Moniker: MonikerClientCapabilities option
  /// Capabilities specific to the various type hierarchy requests.
  ///
  /// @since 3.17.0
  TypeHierarchy: TypeHierarchyClientCapabilities option
  /// Capabilities specific to the `textDocument/inlineValue` request.
  ///
  /// @since 3.17.0
  InlineValue: InlineValueClientCapabilities option
  /// Capabilities specific to the `textDocument/inlayHint` request.
  ///
  /// @since 3.17.0
  InlayHint: InlayHintClientCapabilities option
  /// Capabilities specific to the diagnostic pull model.
  ///
  /// @since 3.17.0
  Diagnostic: DiagnosticClientCapabilities option
}

/// Capabilities specific to the notebook document support.
///
/// @since 3.17.0
type NotebookDocumentClientCapabilities = {
  /// Capabilities specific to notebook document synchronization
  ///
  /// @since 3.17.0
  Synchronization: NotebookDocumentSyncClientCapabilities
}

type WindowClientCapabilities = {
  /// It indicates whether the client supports server initiated
  /// progress using the `window/workDoneProgress/create` request.
  ///
  /// The capability also controls Whether client supports handling
  /// of progress notifications. If set servers are allowed to report a
  /// `workDoneProgress` property in the request specific server
  /// capabilities.
  ///
  /// @since 3.15.0
  WorkDoneProgress: bool option
  /// Capabilities specific to the showMessage request.
  ///
  /// @since 3.16.0
  ShowMessage: ShowMessageRequestClientCapabilities option
  /// Capabilities specific to the showDocument request.
  ///
  /// @since 3.16.0
  ShowDocument: ShowDocumentClientCapabilities option
}

type GeneralClientCapabilitiesStaleRequestSupport = {
  /// The client will actively cancel the request.
  Cancel: bool
  /// The list of requests for which the client
  /// will retry the request if it receives a
  /// response with error code `ContentModified`
  RetryOnContentModified: string[]
}

/// General client capabilities.
///
/// @since 3.16.0
type GeneralClientCapabilities = {
  /// Client capability that signals how the client
  /// handles stale requests (e.g. a request
  /// for which the client will not process the response
  /// anymore since the information is outdated).
  ///
  /// @since 3.17.0
  StaleRequestSupport: GeneralClientCapabilitiesStaleRequestSupport option
  /// Client capabilities specific to regular expressions.
  ///
  /// @since 3.16.0
  RegularExpressions: RegularExpressionsClientCapabilities option
  /// Client capabilities specific to the client's markdown parser.
  ///
  /// @since 3.16.0
  Markdown: MarkdownClientCapabilities option
  /// The position encodings supported by the client. Client and server
  /// have to agree on the same position encoding to ensure that offsets
  /// (e.g. character position in a line) are interpreted the same on both
  /// sides.
  ///
  /// To keep the protocol backwards compatible the following applies: if
  /// the value 'utf-16' is missing from the array of position encodings
  /// servers can assume that the client supports UTF-16. UTF-16 is
  /// therefore a mandatory encoding.
  ///
  /// If omitted it defaults to ['utf-16'].
  ///
  /// Implementation considerations: since the conversion from one encoding
  /// into another requires the content of the file / line the conversion
  /// is best done where the file is read which is usually on the server
  /// side.
  ///
  /// @since 3.17.0
  PositionEncodings: PositionEncodingKind[] option
}

/// A relative pattern is a helper to construct glob patterns that are matched
/// relatively to a base URI. The common value for a `baseUri` is a workspace
/// folder root, but it can be another absolute URI as well.
///
/// @since 3.17.0
type RelativePattern = {
  /// A workspace folder or a base URI to which this pattern will be matched
  /// against relatively.
  BaseUri: U2<WorkspaceFolder, URI>
  /// The actual glob pattern;
  Pattern: Pattern
}

type WorkspaceEditClientCapabilitiesChangeAnnotationSupport = {
  /// Whether the client groups edits with equal labels into tree nodes,
  /// for instance all edits labelled with "Changes in Strings" would
  /// be a tree node.
  GroupsOnLabel: bool option
}

type WorkspaceEditClientCapabilities = {
  /// The client supports versioned document changes in `WorkspaceEdit`s
  DocumentChanges: bool option
  /// The resource operations the client supports. Clients should at least
  /// support 'create', 'rename' and 'delete' files and folders.
  ///
  /// @since 3.13.0
  ResourceOperations: ResourceOperationKind[] option
  /// The failure handling strategy of a client if applying the workspace edit
  /// fails.
  ///
  /// @since 3.13.0
  FailureHandling: FailureHandlingKind option
  /// Whether the client normalizes line endings to the client specific
  /// setting.
  /// If set to `true` the client will normalize line ending characters
  /// in a workspace edit to the client-specified new line
  /// character.
  ///
  /// @since 3.16.0
  NormalizesLineEndings: bool option
  /// Whether the client in general supports change annotations on text edits,
  /// create file, rename file and delete file changes.
  ///
  /// @since 3.16.0
  ChangeAnnotationSupport: WorkspaceEditClientCapabilitiesChangeAnnotationSupport option
}

type DidChangeConfigurationClientCapabilities = {
  /// Did change configuration notification supports dynamic registration.
  DynamicRegistration: bool option
}

type DidChangeWatchedFilesClientCapabilities = {
  /// Did change watched files notification supports dynamic registration. Please note
  /// that the current protocol doesn't support static configuration for file changes
  /// from the server side.
  DynamicRegistration: bool option
  /// Whether the client has support for {@link  RelativePattern relative pattern}
  /// or not.
  ///
  /// @since 3.17.0
  RelativePatternSupport: bool option
}

type WorkspaceSymbolClientCapabilitiesSymbolKind = {
  /// The symbol kind values the client supports. When this
  /// property exists the client also guarantees that it will
  /// handle values outside its set gracefully and falls back
  /// to a default value when unknown.
  ///
  /// If this property is not present the client only supports
  /// the symbol kinds from `File` to `Array` as defined in
  /// the initial version of the protocol.
  ValueSet: SymbolKind[] option
}

type WorkspaceSymbolClientCapabilitiesTagSupport = {
  /// The tags supported by the client.
  ValueSet: SymbolTag[]
}

type WorkspaceSymbolClientCapabilitiesResolveSupport = {
  /// The properties that a client can resolve lazily. Usually
  /// `location.range`
  Properties: string[]
}

/// Client capabilities for a {@link WorkspaceSymbolRequest}.
type WorkspaceSymbolClientCapabilities = {
  /// Symbol request supports dynamic registration.
  DynamicRegistration: bool option
  /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
  SymbolKind: WorkspaceSymbolClientCapabilitiesSymbolKind option
  /// The client supports tags on `SymbolInformation`.
  /// Clients supporting tags have to handle unknown tags gracefully.
  ///
  /// @since 3.16.0
  TagSupport: WorkspaceSymbolClientCapabilitiesTagSupport option
  /// The client support partial workspace symbols. The client will send the
  /// request `workspaceSymbol/resolve` to the server to resolve additional
  /// properties.
  ///
  /// @since 3.17.0
  ResolveSupport: WorkspaceSymbolClientCapabilitiesResolveSupport option
}

/// The client capabilities of a {@link ExecuteCommandRequest}.
type ExecuteCommandClientCapabilities = {
  /// Execute command supports dynamic registration.
  DynamicRegistration: bool option
}

/// @since 3.16.0
type SemanticTokensWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from
  /// the server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// semantic tokens currently shown. It should be used with absolute care
  /// and is useful for situation where a server for example detects a project
  /// wide change that requires such a calculation.
  RefreshSupport: bool option
}

/// @since 3.16.0
type CodeLensWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from the
  /// server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// code lenses currently shown. It should be used with absolute care and is
  /// useful for situation where a server for example detect a project wide
  /// change that requires such a calculation.
  RefreshSupport: bool option
}

/// Capabilities relating to events from file operations by the user in the client.
///
/// These events do not come from the file system, they come from user operations
/// like renaming a file in the UI.
///
/// @since 3.16.0
type FileOperationClientCapabilities = {
  /// Whether the client supports dynamic registration for file requests/notifications.
  DynamicRegistration: bool option
  /// The client has support for sending didCreateFiles notifications.
  DidCreate: bool option
  /// The client has support for sending willCreateFiles requests.
  WillCreate: bool option
  /// The client has support for sending didRenameFiles notifications.
  DidRename: bool option
  /// The client has support for sending willRenameFiles requests.
  WillRename: bool option
  /// The client has support for sending didDeleteFiles notifications.
  DidDelete: bool option
  /// The client has support for sending willDeleteFiles requests.
  WillDelete: bool option
}

/// Client workspace capabilities specific to inline values.
///
/// @since 3.17.0
type InlineValueWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from the
  /// server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// inline values currently shown. It should be used with absolute care and is
  /// useful for situation where a server for example detects a project wide
  /// change that requires such a calculation.
  RefreshSupport: bool option
}

/// Client workspace capabilities specific to inlay hints.
///
/// @since 3.17.0
type InlayHintWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from
  /// the server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// inlay hints currently shown. It should be used with absolute care and
  /// is useful for situation where a server for example detects a project wide
  /// change that requires such a calculation.
  RefreshSupport: bool option
}

/// Workspace client capabilities specific to diagnostic pull requests.
///
/// @since 3.17.0
type DiagnosticWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from
  /// the server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// pulled diagnostics currently shown. It should be used with absolute care and
  /// is useful for situation where a server for example detects a project wide
  /// change that requires such a calculation.
  RefreshSupport: bool option
}

type TextDocumentSyncClientCapabilities = {
  /// Whether text document synchronization supports dynamic registration.
  DynamicRegistration: bool option
  /// The client supports sending will save notifications.
  WillSave: bool option
  /// The client supports sending a will save request and
  /// waits for a response providing text edits which will
  /// be applied to the document before it is saved.
  WillSaveWaitUntil: bool option
  /// The client supports did save notifications.
  DidSave: bool option
}

type CompletionClientCapabilitiesCompletionItem = {
  /// Client supports snippets as insert text.
  ///
  /// A snippet can define tab stops and placeholders with `$1`, `$2`
  /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  /// the end of the snippet. Placeholders with equal identifiers are linked,
  /// that is typing in one will update others too.
  SnippetSupport: bool option
  /// Client supports commit characters on a completion item.
  CommitCharactersSupport: bool option
  /// Client supports the following content formats for the documentation
  /// property. The order describes the preferred format of the client.
  DocumentationFormat: MarkupKind[] option
  /// Client supports the deprecated property on a completion item.
  DeprecatedSupport: bool option
  /// Client supports the preselect property on a completion item.
  PreselectSupport: bool option
  /// Client supports the tag property on a completion item. Clients supporting
  /// tags have to handle unknown tags gracefully. Clients especially need to
  /// preserve unknown tags when sending a completion item back to the server in
  /// a resolve call.
  ///
  /// @since 3.15.0
  TagSupport: CompletionClientCapabilitiesCompletionItemTagSupport option
  /// Client support insert replace edit to control different behavior if a
  /// completion item is inserted in the text or should replace text.
  ///
  /// @since 3.16.0
  InsertReplaceSupport: bool option
  /// Indicates which properties a client can resolve lazily on a completion
  /// item. Before version 3.16.0 only the predefined properties `documentation`
  /// and `details` could be resolved lazily.
  ///
  /// @since 3.16.0
  ResolveSupport: CompletionClientCapabilitiesCompletionItemResolveSupport option
  /// The client supports the `insertTextMode` property on
  /// a completion item to override the whitespace handling mode
  /// as defined by the client (see `insertTextMode`).
  ///
  /// @since 3.16.0
  InsertTextModeSupport: CompletionClientCapabilitiesCompletionItemInsertTextModeSupport option
  /// The client has support for completion item label
  /// details (see also `CompletionItemLabelDetails`).
  ///
  /// @since 3.17.0
  LabelDetailsSupport: bool option
}

type CompletionClientCapabilitiesCompletionItemTagSupport = {
  /// The tags supported by the client.
  ValueSet: CompletionItemTag[]
}

type CompletionClientCapabilitiesCompletionItemResolveSupport = {
  /// The properties that a client can resolve lazily.
  Properties: string[]
}

type CompletionClientCapabilitiesCompletionItemInsertTextModeSupport = { ValueSet: InsertTextMode[] }

type CompletionClientCapabilitiesCompletionItemKind = {
  /// The completion item kind values the client supports. When this
  /// property exists the client also guarantees that it will
  /// handle values outside its set gracefully and falls back
  /// to a default value when unknown.
  ///
  /// If this property is not present the client only supports
  /// the completion items kinds from `Text` to `Reference` as defined in
  /// the initial version of the protocol.
  ValueSet: CompletionItemKind[] option
}

type CompletionClientCapabilitiesCompletionList = {
  /// The client supports the following itemDefaults on
  /// a completion list.
  ///
  /// The value lists the supported property names of the
  /// `CompletionList.itemDefaults` object. If omitted
  /// no properties are supported.
  ///
  /// @since 3.17.0
  ItemDefaults: string[] option
}

/// Completion client capabilities
type CompletionClientCapabilities = {
  /// Whether completion supports dynamic registration.
  DynamicRegistration: bool option
  /// The client supports the following `CompletionItem` specific
  /// capabilities.
  CompletionItem: CompletionClientCapabilitiesCompletionItem option
  CompletionItemKind: CompletionClientCapabilitiesCompletionItemKind option
  /// Defines how the client handles whitespace and indentation
  /// when accepting a completion item that uses multi line
  /// text in either `insertText` or `textEdit`.
  ///
  /// @since 3.17.0
  InsertTextMode: InsertTextMode option
  /// The client supports to send additional context information for a
  /// `textDocument/completion` request.
  ContextSupport: bool option
  /// The client supports the following `CompletionList` specific
  /// capabilities.
  ///
  /// @since 3.17.0
  CompletionList: CompletionClientCapabilitiesCompletionList option
}

type HoverClientCapabilities = {
  /// Whether hover supports dynamic registration.
  DynamicRegistration: bool option
  /// Client supports the following content formats for the content
  /// property. The order describes the preferred format of the client.
  ContentFormat: MarkupKind[] option
}

type SignatureHelpClientCapabilitiesSignatureInformation = {
  /// Client supports the following content formats for the documentation
  /// property. The order describes the preferred format of the client.
  DocumentationFormat: MarkupKind[] option
  /// Client capabilities specific to parameter information.
  ParameterInformation: SignatureHelpClientCapabilitiesSignatureInformationParameterInformation option
  /// The client supports the `activeParameter` property on `SignatureInformation`
  /// literal.
  ///
  /// @since 3.16.0
  ActiveParameterSupport: bool option
}

type SignatureHelpClientCapabilitiesSignatureInformationParameterInformation = {
  /// The client supports processing label offsets instead of a
  /// simple label string.
  ///
  /// @since 3.14.0
  LabelOffsetSupport: bool option
}

/// Client Capabilities for a {@link SignatureHelpRequest}.
type SignatureHelpClientCapabilities = {
  /// Whether signature help supports dynamic registration.
  DynamicRegistration: bool option
  /// The client supports the following `SignatureInformation`
  /// specific properties.
  SignatureInformation: SignatureHelpClientCapabilitiesSignatureInformation option
  /// The client supports to send additional context information for a
  /// `textDocument/signatureHelp` request. A client that opts into
  /// contextSupport will also support the `retriggerCharacters` on
  /// `SignatureHelpOptions`.
  ///
  /// @since 3.15.0
  ContextSupport: bool option
}

/// @since 3.14.0
type DeclarationClientCapabilities = {
  /// Whether declaration supports dynamic registration. If this is set to `true`
  /// the client supports the new `DeclarationRegistrationOptions` return value
  /// for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// The client supports additional metadata in the form of declaration links.
  LinkSupport: bool option
}

/// Client Capabilities for a {@link DefinitionRequest}.
type DefinitionClientCapabilities = {
  /// Whether definition supports dynamic registration.
  DynamicRegistration: bool option
  /// The client supports additional metadata in the form of definition links.
  ///
  /// @since 3.14.0
  LinkSupport: bool option
}

/// Since 3.6.0
type TypeDefinitionClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `TypeDefinitionRegistrationOptions` return value
  /// for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// The client supports additional metadata in the form of definition links.
  ///
  /// Since 3.14.0
  LinkSupport: bool option
}

/// @since 3.6.0
type ImplementationClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `ImplementationRegistrationOptions` return value
  /// for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// The client supports additional metadata in the form of definition links.
  ///
  /// @since 3.14.0
  LinkSupport: bool option
}

/// Client Capabilities for a {@link ReferencesRequest}.
type ReferenceClientCapabilities = {
  /// Whether references supports dynamic registration.
  DynamicRegistration: bool option
}

/// Client Capabilities for a {@link DocumentHighlightRequest}.
type DocumentHighlightClientCapabilities = {
  /// Whether document highlight supports dynamic registration.
  DynamicRegistration: bool option
}

type DocumentSymbolClientCapabilitiesSymbolKind = {
  /// The symbol kind values the client supports. When this
  /// property exists the client also guarantees that it will
  /// handle values outside its set gracefully and falls back
  /// to a default value when unknown.
  ///
  /// If this property is not present the client only supports
  /// the symbol kinds from `File` to `Array` as defined in
  /// the initial version of the protocol.
  ValueSet: SymbolKind[] option
}

type DocumentSymbolClientCapabilitiesTagSupport = {
  /// The tags supported by the client.
  ValueSet: SymbolTag[]
}

/// Client Capabilities for a {@link DocumentSymbolRequest}.
type DocumentSymbolClientCapabilities = {
  /// Whether document symbol supports dynamic registration.
  DynamicRegistration: bool option
  /// Specific capabilities for the `SymbolKind` in the
  /// `textDocument/documentSymbol` request.
  SymbolKind: DocumentSymbolClientCapabilitiesSymbolKind option
  /// The client supports hierarchical document symbols.
  HierarchicalDocumentSymbolSupport: bool option
  /// The client supports tags on `SymbolInformation`. Tags are supported on
  /// `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
  /// Clients supporting tags have to handle unknown tags gracefully.
  ///
  /// @since 3.16.0
  TagSupport: DocumentSymbolClientCapabilitiesTagSupport option
  /// The client supports an additional label presented in the UI when
  /// registering a document symbol provider.
  ///
  /// @since 3.16.0
  LabelSupport: bool option
}

type CodeActionClientCapabilitiesCodeActionLiteralSupport = {
  /// The code action kind is support with the following value
  /// set.
  CodeActionKind: CodeActionClientCapabilitiesCodeActionLiteralSupportCodeActionKind
}

type CodeActionClientCapabilitiesCodeActionLiteralSupportCodeActionKind = {
  /// The code action kind values the client supports. When this
  /// property exists the client also guarantees that it will
  /// handle values outside its set gracefully and falls back
  /// to a default value when unknown.
  ValueSet: CodeActionKind[]
}

type CodeActionClientCapabilitiesResolveSupport = {
  /// The properties that a client can resolve lazily.
  Properties: string[]
}

/// The Client Capabilities of a {@link CodeActionRequest}.
type CodeActionClientCapabilities = {
  /// Whether code action supports dynamic registration.
  DynamicRegistration: bool option
  /// The client support code action literals of type `CodeAction` as a valid
  /// response of the `textDocument/codeAction` request. If the property is not
  /// set the request can only return `Command` literals.
  ///
  /// @since 3.8.0
  CodeActionLiteralSupport: CodeActionClientCapabilitiesCodeActionLiteralSupport option
  /// Whether code action supports the `isPreferred` property.
  ///
  /// @since 3.15.0
  IsPreferredSupport: bool option
  /// Whether code action supports the `disabled` property.
  ///
  /// @since 3.16.0
  DisabledSupport: bool option
  /// Whether code action supports the `data` property which is
  /// preserved between a `textDocument/codeAction` and a
  /// `codeAction/resolve` request.
  ///
  /// @since 3.16.0
  DataSupport: bool option
  /// Whether the client supports resolving additional code action
  /// properties via a separate `codeAction/resolve` request.
  ///
  /// @since 3.16.0
  ResolveSupport: CodeActionClientCapabilitiesResolveSupport option
  /// Whether the client honors the change annotations in
  /// text edits and resource operations returned via the
  /// `CodeAction#edit` property by for example presenting
  /// the workspace edit in the user interface and asking
  /// for confirmation.
  ///
  /// @since 3.16.0
  HonorsChangeAnnotations: bool option
}

/// The client capabilities  of a {@link CodeLensRequest}.
type CodeLensClientCapabilities = {
  /// Whether code lens supports dynamic registration.
  DynamicRegistration: bool option
}

/// The client capabilities of a {@link DocumentLinkRequest}.
type DocumentLinkClientCapabilities = {
  /// Whether document link supports dynamic registration.
  DynamicRegistration: bool option
  /// Whether the client supports the `tooltip` property on `DocumentLink`.
  ///
  /// @since 3.15.0
  TooltipSupport: bool option
}

type DocumentColorClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `DocumentColorRegistrationOptions` return value
  /// for the corresponding server capability as well.
  DynamicRegistration: bool option
}

/// Client capabilities of a {@link DocumentFormattingRequest}.
type DocumentFormattingClientCapabilities = {
  /// Whether formatting supports dynamic registration.
  DynamicRegistration: bool option
}

/// Client capabilities of a {@link DocumentRangeFormattingRequest}.
type DocumentRangeFormattingClientCapabilities = {
  /// Whether range formatting supports dynamic registration.
  DynamicRegistration: bool option
}

/// Client capabilities of a {@link DocumentOnTypeFormattingRequest}.
type DocumentOnTypeFormattingClientCapabilities = {
  /// Whether on type formatting supports dynamic registration.
  DynamicRegistration: bool option
}

type RenameClientCapabilities = {
  /// Whether rename supports dynamic registration.
  DynamicRegistration: bool option
  /// Client supports testing for validity of rename operations
  /// before execution.
  ///
  /// @since 3.12.0
  PrepareSupport: bool option
  /// Client supports the default behavior result.
  ///
  /// The value indicates the default behavior used by the
  /// client.
  ///
  /// @since 3.16.0
  PrepareSupportDefaultBehavior: PrepareSupportDefaultBehavior option
  /// Whether the client honors the change annotations in
  /// text edits and resource operations returned via the
  /// rename request's workspace edit by for example presenting
  /// the workspace edit in the user interface and asking
  /// for confirmation.
  ///
  /// @since 3.16.0
  HonorsChangeAnnotations: bool option
}

type FoldingRangeClientCapabilitiesFoldingRangeKind = {
  /// The folding range kind values the client supports. When this
  /// property exists the client also guarantees that it will
  /// handle values outside its set gracefully and falls back
  /// to a default value when unknown.
  ValueSet: FoldingRangeKind[] option
}

type FoldingRangeClientCapabilitiesFoldingRange = {
  /// If set, the client signals that it supports setting collapsedText on
  /// folding ranges to display custom labels instead of the default text.
  ///
  /// @since 3.17.0
  CollapsedText: bool option
}

type FoldingRangeClientCapabilities = {
  /// Whether implementation supports dynamic registration for folding range
  /// providers. If this is set to `true` the client supports the new
  /// `FoldingRangeRegistrationOptions` return value for the corresponding
  /// server capability as well.
  DynamicRegistration: bool option
  /// The maximum number of folding ranges that the client prefers to receive
  /// per document. The value serves as a hint, servers are free to follow the
  /// limit.
  RangeLimit: uint32 option
  /// If set, the client signals that it only supports folding complete lines.
  /// If set, client will ignore specified `startCharacter` and `endCharacter`
  /// properties in a FoldingRange.
  LineFoldingOnly: bool option
  /// Specific options for the folding range kind.
  ///
  /// @since 3.17.0
  FoldingRangeKind: FoldingRangeClientCapabilitiesFoldingRangeKind option
  /// Specific options for the folding range.
  ///
  /// @since 3.17.0
  FoldingRange: FoldingRangeClientCapabilitiesFoldingRange option
}

type SelectionRangeClientCapabilities = {
  /// Whether implementation supports dynamic registration for selection range providers. If this is set to `true`
  /// the client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server
  /// capability as well.
  DynamicRegistration: bool option
}

type PublishDiagnosticsClientCapabilitiesTagSupport = {
  /// The tags supported by the client.
  ValueSet: DiagnosticTag[]
}

/// The publish diagnostic client capabilities.
type PublishDiagnosticsClientCapabilities = {
  /// Whether the clients accepts diagnostics with related information.
  RelatedInformation: bool option
  /// Client supports the tag property to provide meta data about a diagnostic.
  /// Clients supporting tags have to handle unknown tags gracefully.
  ///
  /// @since 3.15.0
  TagSupport: PublishDiagnosticsClientCapabilitiesTagSupport option
  /// Whether the client interprets the version property of the
  /// `textDocument/publishDiagnostics` notification's parameter.
  ///
  /// @since 3.15.0
  VersionSupport: bool option
  /// Client supports a codeDescription property
  ///
  /// @since 3.16.0
  CodeDescriptionSupport: bool option
  /// Whether code action supports the `data` property which is
  /// preserved between a `textDocument/publishDiagnostics` and
  /// `textDocument/codeAction` request.
  ///
  /// @since 3.16.0
  DataSupport: bool option
}

/// @since 3.16.0
type CallHierarchyClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
}

type SemanticTokensClientCapabilitiesRequests = {
  /// The client will send the `textDocument/semanticTokens/range` request if
  /// the server provides a corresponding handler.
  Range: U2<bool, JToken> option
  /// The client will send the `textDocument/semanticTokens/full` request if
  /// the server provides a corresponding handler.
  Full: U2<bool, SemanticTokensClientCapabilitiesRequestsFullC2> option
}

type SemanticTokensClientCapabilitiesRequestsFullC2 = {
  /// The client will send the `textDocument/semanticTokens/full/delta` request if
  /// the server provides a corresponding handler.
  Delta: bool option
}

/// @since 3.16.0
type SemanticTokensClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// Which requests the client supports and might send to the server
  /// depending on the server's capability. Please note that clients might not
  /// show semantic tokens or degrade some of the user experience if a range
  /// or full request is advertised by the client but not provided by the
  /// server. If for example the client capability `requests.full` and
  /// `request.range` are both set to true but the server only provides a
  /// range provider the client might not render a minimap correctly or might
  /// even decide to not show any semantic tokens at all.
  Requests: SemanticTokensClientCapabilitiesRequests
  /// The token types that the client supports.
  TokenTypes: string[]
  /// The token modifiers that the client supports.
  TokenModifiers: string[]
  /// The token formats the clients supports.
  Formats: TokenFormat[]
  /// Whether the client supports tokens that can overlap each other.
  OverlappingTokenSupport: bool option
  /// Whether the client supports tokens that can span multiple lines.
  MultilineTokenSupport: bool option
  /// Whether the client allows the server to actively cancel a
  /// semantic token request, e.g. supports returning
  /// LSPErrorCodes.ServerCancelled. If a server does the client
  /// needs to retrigger the request.
  ///
  /// @since 3.17.0
  ServerCancelSupport: bool option
  /// Whether the client uses semantic tokens to augment existing
  /// syntax tokens. If set to `true` client side created syntax
  /// tokens and semantic tokens are both used for colorization. If
  /// set to `false` the client only uses the returned semantic tokens
  /// for colorization.
  ///
  /// If the value is `undefined` then the client behavior is not
  /// specified.
  ///
  /// @since 3.17.0
  AugmentsSyntaxTokens: bool option
}

/// Client capabilities for the linked editing range request.
///
/// @since 3.16.0
type LinkedEditingRangeClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
}

/// Client capabilities specific to the moniker request.
///
/// @since 3.16.0
type MonikerClientCapabilities = {
  /// Whether moniker supports dynamic registration. If this is set to `true`
  /// the client supports the new `MonikerRegistrationOptions` return value
  /// for the corresponding server capability as well.
  DynamicRegistration: bool option
}

/// @since 3.17.0
type TypeHierarchyClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
}

/// Client capabilities specific to inline values.
///
/// @since 3.17.0
type InlineValueClientCapabilities = {
  /// Whether implementation supports dynamic registration for inline value providers.
  DynamicRegistration: bool option
}

type InlayHintClientCapabilitiesResolveSupport = {
  /// The properties that a client can resolve lazily.
  Properties: string[]
}

/// Inlay hint client capabilities.
///
/// @since 3.17.0
type InlayHintClientCapabilities = {
  /// Whether inlay hints support dynamic registration.
  DynamicRegistration: bool option
  /// Indicates which properties a client can resolve lazily on an inlay
  /// hint.
  ResolveSupport: InlayHintClientCapabilitiesResolveSupport option
}

/// Client capabilities specific to diagnostic pull requests.
///
/// @since 3.17.0
type DiagnosticClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is set to `true`
  /// the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// Whether the clients supports related documents for document diagnostic pulls.
  RelatedDocumentSupport: bool option
}

/// Notebook specific client capabilities.
///
/// @since 3.17.0
type NotebookDocumentSyncClientCapabilities = {
  /// Whether implementation supports dynamic registration. If this is
  /// set to `true` the client supports the new
  /// `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  /// return value for the corresponding server capability as well.
  DynamicRegistration: bool option
  /// The client supports sending execution summary data per cell.
  ExecutionSummarySupport: bool option
}

type ShowMessageRequestClientCapabilitiesMessageActionItem = {
  /// Whether the client supports additional attributes which
  /// are preserved and send back to the server in the
  /// request's response.
  AdditionalPropertiesSupport: bool option
}

/// Show message request client capabilities
type ShowMessageRequestClientCapabilities = {
  /// Capabilities specific to the `MessageActionItem` type.
  MessageActionItem: ShowMessageRequestClientCapabilitiesMessageActionItem option
}

/// Client capabilities for the showDocument request.
///
/// @since 3.16.0
type ShowDocumentClientCapabilities = {
  /// The client has support for the showDocument
  /// request.
  Support: bool
}

/// Client capabilities specific to regular expressions.
///
/// @since 3.16.0
type RegularExpressionsClientCapabilities = {
  /// The engine's name.
  Engine: string
  /// The engine's version.
  Version: string option
}

/// Client capabilities specific to the used markdown parser.
///
/// @since 3.16.0
type MarkdownClientCapabilities = {
  /// The name of the parser.
  Parser: string
  /// The version of the parser.
  Version: string option
  /// A list of HTML tags that the client allows / supports in
  /// Markdown.
  ///
  /// @since 3.17.0
  AllowedTags: string[] option
}

/// The definition of a symbol represented as one or many {@link Location locations}.
/// For most programming languages there is only one location at which a symbol is
/// defined.
///
/// Servers should prefer returning `DefinitionLink` over `Definition` if supported
/// by the client.
type Definition = U2<Location, Location[]>
/// Information about where a symbol is defined.
///
/// Provides additional metadata over normal {@link Location location} definitions, including the range of
/// the defining symbol
type DefinitionLink = LocationLink
/// LSP arrays.
/// @since 3.17.0
type LSPArray = LSPAny[]
/// The declaration of a symbol representation as one or many {@link Location locations}.
type Declaration = U2<Location, Location[]>
/// Information about where a symbol is declared.
///
/// Provides additional metadata over normal {@link Location location} declarations, including the range of
/// the declaring symbol.
///
/// Servers should prefer returning `DeclarationLink` over `Declaration` if supported
/// by the client.
type DeclarationLink = LocationLink
/// Inline value information can be provided by different means:
/// - directly as a text value (class InlineValueText).
/// - as a name to use for a variable lookup (class InlineValueVariableLookup)
/// - as an evaluatable expression (class InlineValueEvaluatableExpression)
/// The InlineValue types combines all inline value types into one type.
///
/// @since 3.17.0
type InlineValue = U3<InlineValueText, InlineValueVariableLookup, InlineValueEvaluatableExpression>
/// The result of a document diagnostic pull request. A report can
/// either be a full report containing all diagnostics for the
/// requested document or an unchanged report indicating that nothing
/// has changed in terms of diagnostics in comparison to the last
/// pull request.
///
/// @since 3.17.0
type DocumentDiagnosticReport = U2<RelatedFullDocumentDiagnosticReport, RelatedUnchangedDocumentDiagnosticReport>
type PrepareRenameResult = U3<Range, PrepareRenameResultC2, PrepareRenameResultC3>
type PrepareRenameResultC2 = { Range: Range; Placeholder: string }
type PrepareRenameResultC3 = { DefaultBehavior: bool }
/// A document selector is the combination of one or many document filters.
///
/// @sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;
///
/// The use of a string as a document filter is deprecated @since 3.16.0.
type DocumentSelector = DocumentFilter[]
type ProgressToken = U2<int32, string>
/// An identifier to refer to a change annotation stored with a workspace edit.
type ChangeAnnotationIdentifier = string

/// A workspace diagnostic document report.
///
/// @since 3.17.0
type WorkspaceDocumentDiagnosticReport =
  U2<WorkspaceFullDocumentDiagnosticReport, WorkspaceUnchangedDocumentDiagnosticReport>

/// An event describing a change to a text document. If only a text is provided
/// it is considered to be the full content of the document.
type TextDocumentContentChangeEvent = U2<TextDocumentContentChangeEventC1, TextDocumentContentChangeEventC2>

type TextDocumentContentChangeEventC1 = {
  /// The range of the document that changed.
  Range: Range
  /// The optional length of the range that got replaced.
  ///
  /// @deprecated use range instead.
  RangeLength: uint32 option
  /// The new text for the provided range.
  Text: string
}

type TextDocumentContentChangeEventC2 = {
  /// The new text of the whole document.
  Text: string
}

/// MarkedString can be used to render human readable text. It is either a markdown string
/// or a code-block that provides a language and a code snippet. The language identifier
/// is semantically equal to the optional language identifier in fenced code blocks in GitHub
/// issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
///
/// The pair of a language and a value is an equivalent to markdown:
/// ```${language}
/// ${value}
/// ```
///
/// Note that markdown strings will be sanitized - that means html will be escaped.
/// @deprecated use MarkupContent instead.
type MarkedString = U2<string, MarkedStringC2>
type MarkedStringC2 = { Language: string; Value: string }
/// A document filter describes a top level text document or
/// a notebook cell document.
///
/// @since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.
type DocumentFilter = U2<TextDocumentFilter, NotebookCellTextDocumentFilter>
/// LSP object definition.
/// @since 3.17.0
type LSPObject = Map<string, LSPAny>
/// The glob pattern. Either a string pattern or a relative pattern.
///
/// @since 3.17.0
type GlobPattern = U2<Pattern, RelativePattern>

/// A document filter denotes a document by different properties like
/// the {@link TextDocument.languageId language}, the {@link Uri.scheme scheme} of
/// its resource, or a glob-pattern that is applied to the {@link TextDocument.fileName path}.
///
/// Glob patterns can have the following syntax:
/// - `*` to match one or more characters in a path segment
/// - `?` to match on one character in a path segment
/// - `**` to match any number of path segments, including none
/// - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
/// - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
/// - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
///
/// @sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
/// @sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`
///
/// @since 3.17.0
type TextDocumentFilter = {
  /// A language id, like `typescript`.
  Language: string option
  /// A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.
  Scheme: string option
  /// A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.
  Pattern: string option
}

/// A notebook document filter denotes a notebook document by
/// different properties. The properties will be match
/// against the notebook's URI (same as with documents)
///
/// @since 3.17.0
type NotebookDocumentFilter = {
  /// The type of the enclosing notebook.
  NotebookType: string option
  /// A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.
  Scheme: string option
  /// A glob pattern.
  Pattern: string option
}

/// The glob pattern to watch relative to the base path. Glob patterns can have the following syntax:
/// - `*` to match one or more characters in a path segment
/// - `?` to match on one character in a path segment
/// - `**` to match any number of path segments, including none
/// - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
/// - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
/// - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
///
/// @since 3.17.0
type Pattern = string
/// A set of predefined token types. This set is not fixed
/// an clients can specify additional token types via the
/// corresponding client capabilities.
///
/// @since 3.16.0
type SemanticTokenTypes = string

module SemanticTokenTypes =
  [<Literal>]
  let ``namespace``: SemanticTokenTypes = "namespace"

  /// Represents a generic type. Acts as a fallback for types which can't be mapped to
  /// a specific type like class or enum.
  [<Literal>]
  let ``type``: SemanticTokenTypes = "type"

  [<Literal>]
  let ``class``: SemanticTokenTypes = "class"

  [<Literal>]
  let enum: SemanticTokenTypes = "enum"

  [<Literal>]
  let ``interface``: SemanticTokenTypes = "interface"

  [<Literal>]
  let ``struct``: SemanticTokenTypes = "struct"

  [<Literal>]
  let typeParameter: SemanticTokenTypes = "typeParameter"

  [<Literal>]
  let parameter: SemanticTokenTypes = "parameter"

  [<Literal>]
  let variable: SemanticTokenTypes = "variable"

  [<Literal>]
  let property: SemanticTokenTypes = "property"

  [<Literal>]
  let enumMember: SemanticTokenTypes = "enumMember"

  [<Literal>]
  let event: SemanticTokenTypes = "event"

  [<Literal>]
  let ``function``: SemanticTokenTypes = "function"

  [<Literal>]
  let method: SemanticTokenTypes = "method"

  [<Literal>]
  let macro: SemanticTokenTypes = "macro"

  [<Literal>]
  let keyword: SemanticTokenTypes = "keyword"

  [<Literal>]
  let modifier: SemanticTokenTypes = "modifier"

  [<Literal>]
  let comment: SemanticTokenTypes = "comment"

  [<Literal>]
  let string: SemanticTokenTypes = "string"

  [<Literal>]
  let number: SemanticTokenTypes = "number"

  [<Literal>]
  let regexp: SemanticTokenTypes = "regexp"

  [<Literal>]
  let operator: SemanticTokenTypes = "operator"

  /// @since 3.17.0
  [<Literal>]
  let decorator: SemanticTokenTypes = "decorator"

/// A set of predefined token modifiers. This set is not fixed
/// an clients can specify additional token types via the
/// corresponding client capabilities.
///
/// @since 3.16.0
type SemanticTokenModifiers = string

module SemanticTokenModifiers =
  [<Literal>]
  let declaration: SemanticTokenModifiers = "declaration"

  [<Literal>]
  let definition: SemanticTokenModifiers = "definition"

  [<Literal>]
  let readonly: SemanticTokenModifiers = "readonly"

  [<Literal>]
  let ``static``: SemanticTokenModifiers = "static"

  [<Literal>]
  let deprecated: SemanticTokenModifiers = "deprecated"

  [<Literal>]
  let ``abstract``: SemanticTokenModifiers = "abstract"

  [<Literal>]
  let async: SemanticTokenModifiers = "async"

  [<Literal>]
  let modification: SemanticTokenModifiers = "modification"

  [<Literal>]
  let documentation: SemanticTokenModifiers = "documentation"

  [<Literal>]
  let defaultLibrary: SemanticTokenModifiers = "defaultLibrary"

/// The document diagnostic report kinds.
///
/// @since 3.17.0
[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type DocumentDiagnosticReportKind =
  /// A diagnostic report with a full
  /// set of problems.
  | [<EnumMember(Value = "full")>] Full = 0
  /// A report indicating that the last
  /// returned report is still accurate.
  | [<EnumMember(Value = "unchanged")>] Unchanged = 1

/// Predefined error codes.
type ErrorCodes =
  | ParseError = -32700
  | InvalidRequest = -32600
  | MethodNotFound = -32601
  | InvalidParams = -32602
  | InternalError = -32603
  /// Error code indicating that a server received a notification or
  /// request before the server has received the `initialize` request.
  | ServerNotInitialized = -32002
  | UnknownErrorCode = -32001

type LSPErrorCodes =
  /// A request failed but it was syntactically correct, e.g the
  /// method name was known and the parameters were valid. The error
  /// message should contain human readable information about why
  /// the request failed.
  ///
  /// @since 3.17.0
  | RequestFailed = -32803
  /// The server cancelled the request. This error code should
  /// only be used for requests that explicitly support being
  /// server cancellable.
  ///
  /// @since 3.17.0
  | ServerCancelled = -32802
  /// The server detected that the content of a document got
  /// modified outside normal conditions. A server should
  /// NOT send this error code if it detects a content change
  /// in it unprocessed messages. The result even computed
  /// on an older state might still be useful for the client.
  ///
  /// If a client decides that a result is not of any use anymore
  /// the client should cancel the request.
  | ContentModified = -32801
  /// The client has canceled a request and a server has detected
  /// the cancel.
  | RequestCancelled = -32800

/// A set of predefined range kinds.
type FoldingRangeKind = string

module FoldingRangeKind =
  /// Folding range for a comment
  [<Literal>]
  let Comment: FoldingRangeKind = "comment"

  /// Folding range for an import or include
  [<Literal>]
  let Imports: FoldingRangeKind = "imports"

  /// Folding range for a region (e.g. `#region`)
  [<Literal>]
  let Region: FoldingRangeKind = "region"

/// A symbol kind.
type SymbolKind =
  | File = 1
  | Module = 2
  | Namespace = 3
  | Package = 4
  | Class = 5
  | Method = 6
  | Property = 7
  | Field = 8
  | Constructor = 9
  | Enum = 10
  | Interface = 11
  | Function = 12
  | Variable = 13
  | Constant = 14
  | String = 15
  | Number = 16
  | Boolean = 17
  | Array = 18
  | Object = 19
  | Key = 20
  | Null = 21
  | EnumMember = 22
  | Struct = 23
  | Event = 24
  | Operator = 25
  | TypeParameter = 26

/// Symbol tags are extra annotations that tweak the rendering of a symbol.
///
/// @since 3.16
type SymbolTag =
  /// Render a symbol as obsolete, usually using a strike-out.
  | Deprecated = 1

/// Moniker uniqueness level to define scope of the moniker.
///
/// @since 3.16.0
[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type UniquenessLevel =
  /// The moniker is only unique inside a document
  | [<EnumMember(Value = "document")>] document = 0
  /// The moniker is unique inside a project for which a dump got created
  | [<EnumMember(Value = "project")>] project = 1
  /// The moniker is unique inside the group to which a project belongs
  | [<EnumMember(Value = "group")>] group = 2
  /// The moniker is unique inside the moniker scheme.
  | [<EnumMember(Value = "scheme")>] scheme = 3
  /// The moniker is globally unique
  | [<EnumMember(Value = "global")>] ``global`` = 4

/// The moniker kind.
///
/// @since 3.16.0
[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type MonikerKind =
  /// The moniker represent a symbol that is imported into a project
  | [<EnumMember(Value = "import")>] import = 0
  /// The moniker represents a symbol that is exported from a project
  | [<EnumMember(Value = "export")>] export = 1
  /// The moniker represents a symbol that is local to a project (e.g. a local
  /// variable of a function, a class not visible outside the project, ...)
  | [<EnumMember(Value = "local")>] local = 2

/// Inlay hint kinds.
///
/// @since 3.17.0
type InlayHintKind =
  /// An inlay hint that for a type annotation.
  | Type = 1
  /// An inlay hint that is for a parameter.
  | Parameter = 2

/// The message type
type MessageType =
  /// An error message.
  | Error = 1
  /// A warning message.
  | Warning = 2
  /// An information message.
  | Info = 3
  /// A log message.
  | Log = 4
  /// A debug message.
  ///
  /// @since 3.18.0
  | Debug = 5

/// Defines how the host (editor) should sync
/// document changes to the language server.
type TextDocumentSyncKind =
  /// Documents should not be synced at all.
  | None = 0
  /// Documents are synced by always sending the full content
  /// of the document.
  | Full = 1
  /// Documents are synced by sending the full content on open.
  /// After that only incremental updates to the document are
  /// send.
  | Incremental = 2

/// Represents reasons why a text document is saved.
type TextDocumentSaveReason =
  /// Manually triggered, e.g. by the user pressing save, by starting debugging,
  /// or by an API call.
  | Manual = 1
  /// Automatic after a delay.
  | AfterDelay = 2
  /// When the editor lost focus.
  | FocusOut = 3

/// The kind of a completion entry.
type CompletionItemKind =
  | Text = 1
  | Method = 2
  | Function = 3
  | Constructor = 4
  | Field = 5
  | Variable = 6
  | Class = 7
  | Interface = 8
  | Module = 9
  | Property = 10
  | Unit = 11
  | Value = 12
  | Enum = 13
  | Keyword = 14
  | Snippet = 15
  | Color = 16
  | File = 17
  | Reference = 18
  | Folder = 19
  | EnumMember = 20
  | Constant = 21
  | Struct = 22
  | Event = 23
  | Operator = 24
  | TypeParameter = 25

/// Completion item tags are extra annotations that tweak the rendering of a completion
/// item.
///
/// @since 3.15.0
type CompletionItemTag =
  /// Render a completion as obsolete, usually using a strike-out.
  | Deprecated = 1

/// Defines whether the insert text in a completion item should be interpreted as
/// plain text or a snippet.
type InsertTextFormat =
  /// The primary text to be inserted is treated as a plain string.
  | PlainText = 1
  /// The primary text to be inserted is treated as a snippet.
  ///
  /// A snippet can define tab stops and placeholders with `$1`, `$2`
  /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  /// the end of the snippet. Placeholders with equal identifiers are linked,
  /// that is typing in one will update others too.
  ///
  /// See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax
  | Snippet = 2

/// How whitespace and indentation is handled during completion
/// item insertion.
///
/// @since 3.16.0
type InsertTextMode =
  /// The insertion or replace strings is taken as it is. If the
  /// value is multi line the lines below the cursor will be
  /// inserted using the indentation defined in the string value.
  /// The client will not apply any kind of adjustments to the
  /// string.
  | AsIs = 1
  /// The editor adjusts leading whitespace of new lines so that
  /// they match the indentation up to the cursor of the line for
  /// which the item is accepted.
  ///
  /// Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
  /// multi line completion item is indented using 2 tabs and all
  /// following lines inserted will be indented using 2 tabs as well.
  | AdjustIndentation = 2

/// A document highlight kind.
type DocumentHighlightKind =
  /// A textual occurrence.
  | Text = 1
  /// Read-access of a symbol, like reading a variable.
  | Read = 2
  /// Write-access of a symbol, like writing to a variable.
  | Write = 3

/// A set of predefined code action kinds
type CodeActionKind = string

module CodeActionKind =
  /// Empty kind.
  [<Literal>]
  let Empty: CodeActionKind = ""

  /// Base kind for quickfix actions: 'quickfix'
  [<Literal>]
  let QuickFix: CodeActionKind = "quickfix"

  /// Base kind for refactoring actions: 'refactor'
  [<Literal>]
  let Refactor: CodeActionKind = "refactor"

  /// Base kind for refactoring extraction actions: 'refactor.extract'
  ///
  /// Example extract actions:
  ///
  /// - Extract method
  /// - Extract function
  /// - Extract variable
  /// - Extract interface from class
  /// - ...
  [<Literal>]
  let RefactorExtract: CodeActionKind = "refactor.extract"

  /// Base kind for refactoring inline actions: 'refactor.inline'
  ///
  /// Example inline actions:
  ///
  /// - Inline function
  /// - Inline variable
  /// - Inline constant
  /// - ...
  [<Literal>]
  let RefactorInline: CodeActionKind = "refactor.inline"

  /// Base kind for refactoring rewrite actions: 'refactor.rewrite'
  ///
  /// Example rewrite actions:
  ///
  /// - Convert JavaScript function to class
  /// - Add or remove parameter
  /// - Encapsulate field
  /// - Make method static
  /// - Move method to base class
  /// - ...
  [<Literal>]
  let RefactorRewrite: CodeActionKind = "refactor.rewrite"

  /// Base kind for source actions: `source`
  ///
  /// Source code actions apply to the entire file.
  [<Literal>]
  let Source: CodeActionKind = "source"

  /// Base kind for an organize imports source action: `source.organizeImports`
  [<Literal>]
  let SourceOrganizeImports: CodeActionKind = "source.organizeImports"

  /// Base kind for auto-fix source actions: `source.fixAll`.
  ///
  /// Fix all actions automatically fix errors that have a clear fix that do not require user input.
  /// They should not suppress errors or perform unsafe fixes such as generating new types or classes.
  ///
  /// @since 3.15.0
  [<Literal>]
  let SourceFixAll: CodeActionKind = "source.fixAll"

[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type TraceValues =
  /// Turn tracing off.
  | [<EnumMember(Value = "off")>] Off = 0
  /// Trace messages only.
  | [<EnumMember(Value = "messages")>] Messages = 1
  /// Verbose message tracing.
  | [<EnumMember(Value = "verbose")>] Verbose = 2

/// Describes the content type that a client supports in various
/// result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
///
/// Please note that `MarkupKinds` must not start with a `$`. This kinds
/// are reserved for internal usage.
[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type MarkupKind =
  /// Plain text is supported as a content format
  | [<EnumMember(Value = "plaintext")>] PlainText = 0
  /// Markdown is supported as a content format
  | [<EnumMember(Value = "markdown")>] Markdown = 1

/// A set of predefined position encoding kinds.
///
/// @since 3.17.0
type PositionEncodingKind = string

module PositionEncodingKind =
  /// Character offsets count UTF-8 code units (e.g. bytes).
  [<Literal>]
  let UTF8: PositionEncodingKind = "utf-8"

  /// Character offsets count UTF-16 code units.
  ///
  /// This is the default and must always be supported
  /// by servers
  [<Literal>]
  let UTF16: PositionEncodingKind = "utf-16"

  /// Character offsets count UTF-32 code units.
  ///
  /// Implementation note: these are the same as Unicode codepoints,
  /// so this `PositionEncodingKind` may also be used for an
  /// encoding-agnostic representation of character offsets.
  [<Literal>]
  let UTF32: PositionEncodingKind = "utf-32"

/// The file event type
type FileChangeType =
  /// The file got created.
  | Created = 1
  /// The file got changed.
  | Changed = 2
  /// The file got deleted.
  | Deleted = 3

type WatchKind =
  /// Interested in create events.
  | Create = 1
  /// Interested in change events
  | Change = 2
  /// Interested in delete events
  | Delete = 4

/// The diagnostic's severity.
type DiagnosticSeverity =
  /// Reports an error.
  | Error = 1
  /// Reports a warning.
  | Warning = 2
  /// Reports an information.
  | Information = 3
  /// Reports a hint.
  | Hint = 4

/// The diagnostic tags.
///
/// @since 3.15.0
type DiagnosticTag =
  /// Unused or unnecessary code.
  ///
  /// Clients are allowed to render diagnostics with this tag faded out instead of having
  /// an error squiggle.
  | Unnecessary = 1
  /// Deprecated or obsolete code.
  ///
  /// Clients are allowed to rendered diagnostics with this tag strike through.
  | Deprecated = 2

/// How a completion was triggered
type CompletionTriggerKind =
  /// Completion was triggered by typing an identifier (24x7 code
  /// complete), manual invocation (e.g Ctrl+Space) or via API.
  | Invoked = 1
  /// Completion was triggered by a trigger character specified by
  /// the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
  | TriggerCharacter = 2
  /// Completion was re-triggered as current completion list is incomplete
  | TriggerForIncompleteCompletions = 3

/// How a signature help was triggered.
///
/// @since 3.15.0
type SignatureHelpTriggerKind =
  /// Signature help was invoked manually by the user or by a command.
  | Invoked = 1
  /// Signature help was triggered by a trigger character.
  | TriggerCharacter = 2
  /// Signature help was triggered by the cursor moving or by the document content changing.
  | ContentChange = 3

/// The reason why code actions were requested.
///
/// @since 3.17.0
type CodeActionTriggerKind =
  /// Code actions were explicitly requested by the user or by an extension.
  | Invoked = 1
  /// Code actions were requested automatically.
  ///
  /// This typically happens when current selection in a file changes, but can
  /// also be triggered when file content changes.
  | Automatic = 2

/// A pattern kind describing if a glob pattern matches a file a folder or
/// both.
///
/// @since 3.16.0
[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type FileOperationPatternKind =
  /// The pattern matches a file only.
  | [<EnumMember(Value = "file")>] file = 0
  /// The pattern matches a folder only.
  | [<EnumMember(Value = "folder")>] folder = 1

/// A notebook cell kind.
///
/// @since 3.17.0
type NotebookCellKind =
  /// A markup-cell is formatted source that is used for display.
  | Markup = 1
  /// A code-cell is source code.
  | Code = 2

[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type ResourceOperationKind =
  /// Supports creating new files and folders.
  | [<EnumMember(Value = "create")>] Create = 0
  /// Supports renaming existing files and folders.
  | [<EnumMember(Value = "rename")>] Rename = 1
  /// Supports deleting existing files and folders.
  | [<EnumMember(Value = "delete")>] Delete = 2

[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type FailureHandlingKind =
  /// Applying the workspace change is simply aborted if one of the changes provided
  /// fails. All operations executed before the failing operation stay executed.
  | [<EnumMember(Value = "abort")>] Abort = 0
  /// All operations are executed transactional. That means they either all
  /// succeed or no changes at all are applied to the workspace.
  | [<EnumMember(Value = "transactional")>] Transactional = 1
  /// If the workspace edit contains only textual file changes they are executed transactional.
  /// If resource changes (create, rename or delete file) are part of the change the failure
  /// handling strategy is abort.
  | [<EnumMember(Value = "textOnlyTransactional")>] TextOnlyTransactional = 2
  /// The client tries to undo the operations already executed. But there is no
  /// guarantee that this is succeeding.
  | [<EnumMember(Value = "undo")>] Undo = 3

type PrepareSupportDefaultBehavior =
  /// The client's default behavior is to select the identifier
  /// according the to language's syntax rule.
  | Identifier = 1

[<JsonConverter(typeof<Converters.StringEnumConverter>)>]
type TokenFormat =
  | [<EnumMember(Value = "relative")>] Relative = 0