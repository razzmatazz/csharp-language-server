namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging

type CSharpLspServer(lspClient: ICSharpLspClient) =

    let logger = LogProvider.getLoggerByName "LSP"

    let mutable workspaceFolders: WorkspaceFolder list = []

module Server =
    let logger = LogProvider.getLoggerByName "LSP"

    let getServerCapabilities (lspClient: InitializeParams) =
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

        serverCapabilities

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
