namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics
open System.Reflection

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FSharpPlus

open CSharpLanguageServer.Types
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging

module LspUtils =
    /// Return the JSON-RPC "not implemented" error
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

    /// Do nothing and ignore the notification
    let ignoreNotification: Async<unit> = async.Return(())


open LspUtils

type CSharpLspServer() =

    let logger = LogProvider.getLoggerByName "LSP"

    let mutable workspaceFolders: WorkspaceFolder list = []

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) = async {
            let serverName = "csharp-ls"
            logger.info (
                Log.setMessage "initializing, {name} version {version}"
                >> Log.addContext "name" serverName
                >> Log.addContext "version" (Assembly.GetExecutingAssembly().GetName().Version)
            )
            logger.info (
                Log.setMessage "{name} is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"
                >> Log.addContext "name" serverName
            )

            //lspClient.Capabilities <- p.Capabilities
            let lspClient = p

            (*
            workspaceFolders <-
                map Array.toList p.WorkspaceFolders
                // Can't simplify it to (:: []) like Haskell :(
                |> Option.orElse (map (Uri.toWorkspaceFolder >> (flip List.cons [])) p.RootUri)
                |> Option.orElse (map (Path.toWorkspaceFolder >> (flip List.cons [])) p.RootPath)
                |> Option.defaultValue [Path.toWorkspaceFolder (Directory.GetCurrentDirectory())]
            *)

            // TODO: Monitor the lsp client process (via processId in InitializeParams) and shutdown if the
            // lsp client dies.

            let serverCapabilities =
                { ServerCapabilities.Default with
                    TextDocumentSync = TextDocumentSync.provider lspClient.Capabilities
                    CompletionProvider = Completion.provider lspClient.Capabilities
                    HoverProvider = Hover.provider lspClient.Capabilities
                    SignatureHelpProvider = SignatureHelp.provider lspClient.Capabilities
                    // DeclarationProvider = Declaration.provider lspClient.Capabilities
                    DefinitionProvider = Definition.provider lspClient.Capabilities
                    TypeDefinitionProvider = TypeDefinition.provider lspClient.Capabilities
                    ImplementationProvider = Implementation.provider lspClient.Capabilities
                    ReferencesProvider = References.provider lspClient.Capabilities
                    DocumentHighlightProvider = DocumentHighlight.provider lspClient.Capabilities
                    DocumentSymbolProvider = DocumentSymbol.provider lspClient.Capabilities
                    CodeActionProvider = CodeAction.provider lspClient.Capabilities
                    CodeLensProvider = CodeLens.provider lspClient.Capabilities
                    // DocumentLinkProvider = DocumentLink.provider lspClient.Capabilities
                    // ColorProvider = Color.provider lspClient.Capabilities
                    DocumentFormattingProvider = DocumentFormatting.provider lspClient.Capabilities
                    DocumentRangeFormattingProvider = DocumentRangeFormatting.provider lspClient.Capabilities
                    DocumentOnTypeFormattingProvider = DocumentOnTypeFormatting.provider lspClient.Capabilities
                    RenameProvider = Rename.provider lspClient.Capabilities
                    // FoldingRangeProvider = FoldingRange.provider lspClient.Capabilities
                    // ExecuteCommandProvider = ExecuteCommand.provider lspClient.Capabilities
                    // SelectionRangeProvider = SelectionRange.provider lspClient.Capabilities
                    // LinkedEditingRangeProvider = LinkedEditingRange.provider lspClient.Capabilities
                    CallHierarchyProvider = CallHierarchy.provider lspClient.Capabilities
                    SemanticTokensProvider = SemanticTokens.provider lspClient.Capabilities
                    // MonikerProvider = Moniker.provider lspClient.Capabilities
                    TypeHierarchyProvider = TypeHierarchy.provider lspClient.Capabilities
                    // InlineValueProvider = InlineValue.provider lspClient.Capabilities
                    InlayHintProvider = InlayHint.provider lspClient.Capabilities
                    // DiagnosticProvider = Diagnostic.provider lspClient.Capabilities
                    WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities }

            // TODO: Report server info to client (name, version)
            let initializeResult =
                { InitializeResult.Default with
                    Capabilities = serverCapabilities }

            return initializeResult |> LspResult.success
        }

        override __.Initialized(p) = ignoreNotification

        override __.Shutdown() = ignoreNotification

        override __.Exit() = ignoreNotification

        override this.TextDocumentHover(p) = notImplemented

        override this.TextDocumentDidOpen(p) = ignoreNotification

        override this.TextDocumentDidChange(p) = ignoreNotification

        override this.TextDocumentDidClose(p) = ignoreNotification

        override this.TextDocumentWillSave(p) = ignoreNotification

        override this.TextDocumentWillSaveWaitUntil(p) = notImplemented

        override this.TextDocumentDidSave(p) = ignoreNotification

        override this.TextDocumentCompletion(p) = notImplemented

        override this.CompletionItemResolve(p) = notImplemented

        override this.TextDocumentPrepareRename(p) = notImplemented

        override this.TextDocumentRename(p) = notImplemented

        override this.TextDocumentDefinition(p) = notImplemented

        override this.TextDocumentReferences(p) = notImplemented

        override this.TextDocumentDocumentHighlight(p) = notImplemented

        override this.TextDocumentDocumentLink(p) = notImplemented

        override this.DocumentLinkResolve(p) = notImplemented

        override this.TextDocumentTypeDefinition(p) = notImplemented

        override this.TextDocumentImplementation(p) = notImplemented

        override this.TextDocumentCodeAction(p) = notImplemented

        override this.CodeActionResolve(p) = notImplemented

        override this.TextDocumentCodeLens(p) = notImplemented

        override this.CodeLensResolve(p) = notImplemented

        override this.TextDocumentSignatureHelp(p) = notImplemented

        override this.TextDocumentDocumentColor(p) = notImplemented

        override this.TextDocumentColorPresentation(p) = notImplemented

        override this.TextDocumentFormatting(p) = notImplemented

        override this.TextDocumentRangeFormatting(p) = notImplemented

        override this.TextDocumentOnTypeFormatting(p) = notImplemented

        override this.TextDocumentDocumentSymbol(p) = notImplemented

        override this.WorkspaceDidChangeWatchedFiles(p) = ignoreNotification

        override __.WorkspaceDidChangeWorkspaceFolders(p) = ignoreNotification

        override __.WorkspaceDidChangeConfiguration(p) = ignoreNotification

        override __.WorkspaceWillCreateFiles(p) = notImplemented

        override __.WorkspaceDidCreateFiles(p) = ignoreNotification

        override __.WorkspaceWillRenameFiles(p) = notImplemented

        override __.WorkspaceDidRenameFiles(p) = ignoreNotification

        override __.WorkspaceWillDeleteFiles(p) = notImplemented

        override __.WorkspaceDidDeleteFiles(p) = ignoreNotification

        override this.WorkspaceSymbol(p) = notImplemented

        override this.WorkspaceExecuteCommand(p) = notImplemented

        override this.TextDocumentFoldingRange(p) = notImplemented

        override this.TextDocumentSelectionRange(p) = notImplemented

        override this.TextDocumentSemanticTokensFull(p) = notImplemented

        override this.TextDocumentSemanticTokensFullDelta(p) = notImplemented

        override this.TextDocumentSemanticTokensRange(p) = notImplemented

        override this.TextDocumentInlayHint(p) = notImplemented

        override this.InlayHintResolve(p) = notImplemented

        override __.WorkDoneProgressCancel(p) = ignoreNotification

        override this.TextDocumentInlineValue(p) = notImplemented

        override this.TextDocumentPrepareCallHierarchy(p) = notImplemented

        override this.CallHierarchyIncomingCalls(p) = notImplemented

        override this.CallHierarchyOutgoingCalls(p) = notImplemented

        override this.TextDocumentPrepareTypeHierarchy(p) = notImplemented

        override this.TypeHierarchySupertypes(p) = notImplemented

        override this.TypeHierarchySubtypes(p) = notImplemented

        override this.CSharpMetadata(p) = notImplemented

        override this.TextDocumentDeclaration(p) = notImplemented

        override this.WorkspaceDiagnostic(p) = notImplemented

        override this.WorkspaceSymbolResolve(p) = notImplemented

        override this.TextDocumentDiagnostic(p) = notImplemented

        override this.TextDocumentLinkedEditingRange(p) = notImplemented

        override this.TextDocumentMoniker(p) = notImplemented

module Server =
    let logger = LogProvider.getLoggerByName "LSP"

    let startCore setupServerHandlers options =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        Ionide.LanguageServerProtocol.Server.startWithSetup
            (setupServerHandlers options)
            input
            output
            CSharpLspClient
            defaultRpc

    let start setupServerHandlers options =
        try
            let result = startCore setupServerHandlers options
            int result
        with ex ->
            logger.error (
                Log.setMessage "{name} crashed:"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
                >> Log.addException ex
            )
            3
