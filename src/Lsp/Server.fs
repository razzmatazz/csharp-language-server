namespace CSharpLanguageServer.Lsp

open System
open System.IO
open System.Threading.Tasks
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open StreamJsonRpc
open FSharpPlus

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil
open CSharpLanguageServer.Common
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging


type CSharpLspServer(lspClient: CSharpLspClient, workspaceManager: IWorkspaceManager) =

    let logger = LogProvider.getLoggerByName "LSP"

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) = async {
            let workspaceFolders =
                map Array.toList p.WorkspaceFolders
                // Can't simplify it to (:: []) like Haskell :(
                |> Option.orElse (map (Uri.toWorkspaceFolder >> (flip List.cons [])) p.RootUri)
                |> Option.orElse (map (Path.toWorkspaceFolder >> (flip List.cons [])) p.RootPath)
                |> Option.orElse (Some [Path.toWorkspaceFolder (Directory.GetCurrentDirectory())])
            workspaceManager.Initialize (Option.get workspaceFolders)

            // TODO: Monitor the lsp client process (via processId in InitializeParams) and shutdown if the
            // lsp client dies.

            let serverCapabilities =
                { ServerCapabilities.Default with
                    TextDocumentSync = TextDocumentSync.provider
                    CompletionProvider = Completion.provider
                    HoverProvider = Hover.provider
                    SignatureHelpProvider = SignatureHelp.provider
                    DeclarationProvider = Declaration.provider
                    DefinitionProvider = Definition.provider
                    TypeDefinitionProvider = TypeDefinition.provider
                    ImplementationProvider = Implementation.provider
                    ReferencesProvider = References.provider
                    DocumentHighlightProvider = DocumentHighlight.provider
                    DocumentSymbolProvider = DocumentSymbol.provider
                    CodeActionProvider = CodeAction.provider
                    CodeLensProvider = CodeLens.provider
                    DocumentLinkProvider = DocumentLink.provider
                    ColorProvider = Color.provider
                    DocumentFormattingProvider = DocumentFormatting.provider
                    DocumentRangeFormattingProvider = DocumentRangeFormatting.provider
                    DocumentOnTypeFormattingProvider = DocumentOnTypeFormatting.provider
                    RenameProvider = Rename.provider
                    FoldingRangeProvider = FoldingRange.provider
                    ExecuteCommandProvider = ExecuteCommand.provider
                    SelectionRangeProvider = SelectionRange.provider
                    LinkedEditingRangeProvider = LinkedEditingRange.provider
                    CallHierarchyProvider = CallHierarchy.provider
                    SemanticTokensProvider = SemanticTokens.provider
                    MonikerProvider = Moniker.provider
                    TypeHierarchyProvider = TypeHierarchy.provider
                    InlineValueProvider = InlineValue.provider
                    InlayHintProvider = InlayHint.provider
                    DiagnosticProvider = Diagnostic.provider
                    WorkspaceSymbolProvider = WorkspaceSymbol.provider }

            let initializeResult =
                { InitializeResult.Default with
                    Capabilities = serverCapabilities }

            return initializeResult |> success
        }

        override __.Initialized(p) = ignoreNotification

        override __.Shutdown() = ignoreNotification

        override __.Exit() = ignoreNotification

        override this.TextDocumentHover(p) = Hover.handle workspaceManager p

        override this.TextDocumentDidOpen(p) = TextDocumentSync.didOpen workspaceManager p

        override this.TextDocumentDidChange(p) = TextDocumentSync.didChange workspaceManager p

        override this.TextDocumentDidClose(p) = TextDocumentSync.didClose workspaceManager p

        override this.TextDocumentWillSave(p) = TextDocumentSync.willSave workspaceManager p

        override this.TextDocumentWillSaveWaitUntil(p) = TextDocumentSync.willSaveUntil workspaceManager p

        override this.TextDocumentDidSave(p) = TextDocumentSync.didSave workspaceManager p

        override this.TextDocumentCompletion(p) = Completion.handle workspaceManager p

        override this.CompletionItemResolve(p) = Completion.resolve workspaceManager p

        override this.TextDocumentPrepareRename(p) = Rename.prepare workspaceManager p

        override this.TextDocumentRename(p) = Rename.handle workspaceManager p

        override this.TextDocumentDeclaration(p) = Declaration.handle workspaceManager p

        override this.TextDocumentDefinition(p) = Definition.handle workspaceManager p

        override this.TextDocumentReferences(p) = References.handle workspaceManager p

        override this.TextDocumentDocumentHighlight(p) = DocumentHighlight.handle workspaceManager p

        override this.TextDocumentDocumentLink(p) = DocumentLink.handle workspaceManager p

        override this.DocumentLinkResolve(p) = DocumentLink.resolve workspaceManager p

        override this.TextDocumentTypeDefinition(p) = TypeDefinition.handle workspaceManager p

        override this.TextDocumentImplementation(p) = Implementation.handle workspaceManager p

        override this.TextDocumentCodeAction(p) = CodeAction.handle workspaceManager p

        override this.CodeActionResolve(p) = CodeAction.resolve workspaceManager p

        override this.TextDocumentCodeLens(p) = CodeLens.handle workspaceManager p

        override this.CodeLensResolve(p) = CodeLens.resolve workspaceManager p

        override this.TextDocumentSignatureHelp(p) = SignatureHelp.handle workspaceManager p

        override this.TextDocumentDocumentColor(p) = Color.handle workspaceManager p

        override this.TextDocumentColorPresentation(p) = Color.present workspaceManager p

        override this.TextDocumentFormatting(p) = DocumentFormatting.handle workspaceManager p

        override this.TextDocumentRangeFormatting(p) = DocumentRangeFormatting.handle workspaceManager p

        override this.TextDocumentOnTypeFormatting(p) = DocumentOnTypeFormatting.handle workspaceManager p

        override this.TextDocumentDocumentSymbol(p) = DocumentSymbol.handle workspaceManager p

        override __.TextDocumentMoniker(p) = notImplemented

        override __.TextDocumentLinkedEditingRange(p) = notImplemented

        override __.WorkspaceDidChangeWatchedFiles(p) = ignoreNotification

        override __.WorkspaceDidChangeWorkspaceFolders(p) = ignoreNotification

        override __.WorkspaceDidChangeConfiguration(p) = ignoreNotification

        override __.WorkspaceWillCreateFiles(p) = notImplemented

        override __.WorkspaceDidCreateFiles(p) = ignoreNotification

        override __.WorkspaceWillRenameFiles(p) = notImplemented

        override __.WorkspaceDidRenameFiles(p) = ignoreNotification

        override __.WorkspaceWillDeleteFiles(p) = notImplemented

        override __.WorkspaceDidDeleteFiles(p) = ignoreNotification

        override this.WorkspaceSymbol(p) = WorkspaceSymbol.handle workspaceManager p

        override this.WorkspaceSymbolResolve(p) = WorkspaceSymbol.resolve workspaceManager p

        override this.WorkspaceExecuteCommand(p) = ExecuteCommand.handle workspaceManager p

        override this.TextDocumentFoldingRange(p) = FoldingRange.handle workspaceManager p

        override this.TextDocumentSelectionRange(p) = SelectionRange.handle workspaceManager p

        override this.TextDocumentSemanticTokensFull(p) = SemanticTokens.handleFull workspaceManager p

        override this.TextDocumentSemanticTokensFullDelta(p) = SemanticTokens.handleFullDelta workspaceManager p

        override this.TextDocumentSemanticTokensRange(p) = SemanticTokens.handleRange workspaceManager p

        override this.TextDocumentInlayHint(p) = InlayHint.handle workspaceManager p

        override this.InlayHintResolve(p) = InlayHint.resolve workspaceManager p

        override __.WorkDoneProgressCancel(p) = ignoreNotification

        override this.TextDocumentInlineValue(p) = InlineValue.handle workspaceManager p

        override this.TextDocumentPrepareCallHierarchy(p) = CallHierarchy.prepare workspaceManager p

        override this.CallHierarchyIncomingCalls(p) = CallHierarchy.incomingCalls workspaceManager p

        override this.CallHierarchyOutgoingCalls(p) = CallHierarchy.outgoingCalls workspaceManager p

        override this.TextDocumentPrepareTypeHierarchy(p) = TypeHierarchy.prepare workspaceManager p

        override this.TypeHierarchySupertypes(p) = TypeHierarchy.supertypes workspaceManager p

        override this.TypeHierarchySubtypes(p) = TypeHierarchy.subtypes workspaceManager p

        override this.TextDocumentDiagnostic(p) = notImplemented

        override this.WorkspaceDiagnostic(p) = notImplemented

        override __.CSharpMetadata(p) = notImplemented


module Server =
    let private createRpc (handler: IJsonRpcMessageHandler) : JsonRpc =
        let rec (|HandleableException|_|) (e: exn) =
            match e with
            | :? LocalRpcException -> Some()
            | :? TaskCanceledException -> Some()
            | :? OperationCanceledException -> Some()
            | :? System.AggregateException as aex ->
                if aex.InnerExceptions.Count = 1 then
                    (|HandleableException|_|) aex.InnerException
                else
                    None
            | _ -> None

        { new JsonRpc(handler) with
            member this.IsFatalException(ex: Exception) =
                match ex with
                | HandleableException -> false
                | _ -> true }

    let private customRequestHandlings =
        // Compiler of F# can't infer the type of `s` in `"csharp/metadata", serverRequestHandling (fun s p -> s.CSharpMetadata(p))`
        // to `ICSharpLspServer` even if we have written the type of `customRequestHandlings` to
        // `Map<string, ServerRequestHandling<ICSharpLspServer>>`. So write a helper function to avoid write the type of `s` every time.
        let requestHandling
            (run: ICSharpLspServer -> 'param -> AsyncLspResult<'result>)
            : ServerRequestHandling<ICSharpLspServer> =
            serverRequestHandling run

        [ "csharp/metadata", requestHandling (fun s p -> s.CSharpMetadata(p)) ]
        |> Map.ofList

    let private requestHandlings =
        Map.union (defaultRequestHandlings ()) customRequestHandlings

    let private startCore clientCreator workspaceManagerCreator =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let serverCreator client =
            new CSharpLspServer(client, workspaceManagerCreator client) :> ICSharpLspServer

        Ionide.LanguageServerProtocol.Server.start
            requestHandlings
            input
            output
            clientCreator
            serverCreator
            createRpc

    let start clientCreator workspaceCreator =
        try
            let result = startCore clientCreator workspaceCreator
            int result
        with _ex ->
            3
