namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics
open System.IO
open System.Reflection
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

// Move it to Common project?
[<AutoOpen>]
module Operators =
    // There is no such operator (with type Monad<'a> -> Monad<'b> -> Monad<'b>) in FSharpPlus like `>>` in
    // Haskell, so just write one.
    // In F#, `>>` is function composition, that is why we use another operator.
    let inline (>->) (ma: '``Monad<'a>``) (mb: '``Monad<'b>``): '``Monad<'b>`` = ma >>= konst mb


type CSharpLspServer(lspClient: ICSharpLspClient, workspaceManager: IWorkspaceManager) =

    let logger = LogProvider.getLoggerByName "LSP"

    let mutable workspaceFolders: WorkspaceFolder list = []

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) = async {
            logger.info (
                Log.setMessage "initializing, {name} version {version}"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
                // TODO: The version is not correct
                >> Log.addContext "version" (Assembly.GetExecutingAssembly().GetName().Version)
            )
            logger.info (
                Log.setMessage "{name} is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
            )

            lspClient.Capabilities <- p.Capabilities
            workspaceFolders <-
                map Array.toList p.WorkspaceFolders
                // Can't simplify it to (:: []) like Haskell :(
                |> Option.orElse (map (Uri.toWorkspaceFolder >> (flip List.cons [])) p.RootUri)
                |> Option.orElse (map (Path.toWorkspaceFolder >> (flip List.cons [])) p.RootPath)
                |> Option.defaultValue [Path.toWorkspaceFolder (Directory.GetCurrentDirectory())]

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
                    DocumentLinkProvider = DocumentLink.provider lspClient.Capabilities
                    // ColorProvider = Color.provider lspClient.Capabilities
                    DocumentFormattingProvider = DocumentFormatting.provider lspClient.Capabilities
                    DocumentRangeFormattingProvider = DocumentRangeFormatting.provider lspClient.Capabilities
                    DocumentOnTypeFormattingProvider = DocumentOnTypeFormatting.provider lspClient.Capabilities
                    RenameProvider = Rename.provider lspClient.Capabilities
                    FoldingRangeProvider = FoldingRange.provider lspClient.Capabilities
                    ExecuteCommandProvider = ExecuteCommand.provider lspClient.Capabilities
                    SelectionRangeProvider = SelectionRange.provider lspClient.Capabilities
                    // LinkedEditingRangeProvider = LinkedEditingRange.provider lspClient.Capabilities
                    CallHierarchyProvider = CallHierarchy.provider lspClient.Capabilities
                    SemanticTokensProvider = SemanticTokens.provider lspClient.Capabilities
                    // MonikerProvider = Moniker.provider lspClient.Capabilities
                    TypeHierarchyProvider = TypeHierarchy.provider lspClient.Capabilities
                    InlineValueProvider = InlineValue.provider lspClient.Capabilities
                    InlayHintProvider = InlayHint.provider lspClient.Capabilities
                    // DiagnosticProvider = Diagnostic.provider lspClient.Capabilities
                    WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities }

            // TODO: Report server info to client (name, version)
            let initializeResult =
                { InitializeResult.Default with
                    Capabilities = serverCapabilities }

            return initializeResult |> success
        }

        override __.Initialized(p) = workspaceManager.Initialize workspaceFolders

        override __.Shutdown() = ignoreNotification

        override __.Exit() = ignoreNotification

        override this.TextDocumentHover(p) = workspaceManager.WaitInitialized() >-> Hover.handle workspaceManager p

        override this.TextDocumentDidOpen(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.didOpen workspaceManager p

        override this.TextDocumentDidChange(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.didChange workspaceManager p

        override this.TextDocumentDidClose(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.didClose workspaceManager p

        override this.TextDocumentWillSave(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.willSave workspaceManager p

        override this.TextDocumentWillSaveWaitUntil(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.willSaveUntil workspaceManager p

        override this.TextDocumentDidSave(p) = workspaceManager.WaitInitialized() >-> TextDocumentSync.didSave workspaceManager p

        override this.TextDocumentCompletion(p) = workspaceManager.WaitInitialized() >-> Completion.handle workspaceManager p

        override this.CompletionItemResolve(p) = workspaceManager.WaitInitialized() >-> Completion.resolve workspaceManager p

        override this.TextDocumentPrepareRename(p) = workspaceManager.WaitInitialized() >-> Rename.prepare workspaceManager p

        override this.TextDocumentRename(p) = workspaceManager.WaitInitialized() >-> Rename.handle workspaceManager lspClient.Capabilities p

        override this.TextDocumentDefinition(p) = workspaceManager.WaitInitialized() >-> Definition.handle workspaceManager p

        override this.TextDocumentReferences(p) = workspaceManager.WaitInitialized() >-> References.handle workspaceManager p

        override this.TextDocumentDocumentHighlight(p) = workspaceManager.WaitInitialized() >-> DocumentHighlight.handle workspaceManager p

        override this.TextDocumentDocumentLink(p) = workspaceManager.WaitInitialized() >-> DocumentLink.handle workspaceManager p

        override this.DocumentLinkResolve(p) = workspaceManager.WaitInitialized() >-> DocumentLink.resolve workspaceManager p

        override this.TextDocumentTypeDefinition(p) = workspaceManager.WaitInitialized() >-> TypeDefinition.handle workspaceManager p

        override this.TextDocumentImplementation(p) = workspaceManager.WaitInitialized() >-> Implementation.handle workspaceManager p

        override this.TextDocumentCodeAction(p) = workspaceManager.WaitInitialized() >-> CodeAction.handle workspaceManager lspClient.Capabilities p

        override this.CodeActionResolve(p) = workspaceManager.WaitInitialized() >-> CodeAction.resolve workspaceManager p

        override this.TextDocumentCodeLens(p) = workspaceManager.WaitInitialized() >-> CodeLens.handle workspaceManager p

        override this.CodeLensResolve(p) = workspaceManager.WaitInitialized() >-> CodeLens.resolve workspaceManager p

        override this.TextDocumentSignatureHelp(p) = workspaceManager.WaitInitialized() >-> SignatureHelp.handle workspaceManager p

        override this.TextDocumentDocumentColor(p) = workspaceManager.WaitInitialized() >-> Color.handle workspaceManager p

        override this.TextDocumentColorPresentation(p) = workspaceManager.WaitInitialized() >-> Color.present workspaceManager p

        override this.TextDocumentFormatting(p) = workspaceManager.WaitInitialized() >-> DocumentFormatting.handle workspaceManager p

        override this.TextDocumentRangeFormatting(p) = workspaceManager.WaitInitialized() >-> DocumentRangeFormatting.handle workspaceManager p

        override this.TextDocumentOnTypeFormatting(p) = workspaceManager.WaitInitialized() >-> DocumentOnTypeFormatting.handle workspaceManager p

        override this.TextDocumentDocumentSymbol(p) = workspaceManager.WaitInitialized() >-> DocumentSymbol.handle workspaceManager lspClient.Capabilities p

        override __.WorkspaceDidChangeWatchedFiles(p) = ignoreNotification

        override __.WorkspaceDidChangeWorkspaceFolders(p) = ignoreNotification

        override __.WorkspaceDidChangeConfiguration(p) = ignoreNotification

        override __.WorkspaceWillCreateFiles(p) = notImplemented

        override __.WorkspaceDidCreateFiles(p) = ignoreNotification

        override __.WorkspaceWillRenameFiles(p) = notImplemented

        override __.WorkspaceDidRenameFiles(p) = ignoreNotification

        override __.WorkspaceWillDeleteFiles(p) = notImplemented

        override __.WorkspaceDidDeleteFiles(p) = ignoreNotification

        override this.WorkspaceSymbol(p) = workspaceManager.WaitInitialized() >-> WorkspaceSymbol.handle workspaceManager p

        override this.WorkspaceExecuteCommand(p) = workspaceManager.WaitInitialized() >-> ExecuteCommand.handle workspaceManager p

        override this.TextDocumentFoldingRange(p) = workspaceManager.WaitInitialized() >-> FoldingRange.handle workspaceManager p

        override this.TextDocumentSelectionRange(p) = workspaceManager.WaitInitialized() >-> SelectionRange.handle workspaceManager p

        override this.TextDocumentSemanticTokensFull(p) = workspaceManager.WaitInitialized() >-> SemanticTokens.handleFull workspaceManager p

        override this.TextDocumentSemanticTokensFullDelta(p) = workspaceManager.WaitInitialized() >-> SemanticTokens.handleFullDelta workspaceManager p

        override this.TextDocumentSemanticTokensRange(p) = workspaceManager.WaitInitialized() >-> SemanticTokens.handleRange workspaceManager p

        override this.TextDocumentInlayHint(p) = workspaceManager.WaitInitialized() >-> InlayHint.handle workspaceManager p

        override this.InlayHintResolve(p) = workspaceManager.WaitInitialized() >-> InlayHint.resolve workspaceManager p

        override __.WorkDoneProgessCancel(p) = ignoreNotification

        override this.TextDocumentInlineValue(p) = workspaceManager.WaitInitialized() >-> InlineValue.handle workspaceManager p

        override this.TextDocumentPrepareCallHierarchy(p) = workspaceManager.WaitInitialized() >-> CallHierarchy.prepare workspaceManager p

        override this.CallHierarchyIncomingCalls(p) = workspaceManager.WaitInitialized() >-> CallHierarchy.incomingCalls workspaceManager p

        override this.CallHierarchyOutgoingCalls(p) = workspaceManager.WaitInitialized() >-> CallHierarchy.outgoingCalls workspaceManager p

        override this.TextDocumentPrepareTypeHierarchy(p) = workspaceManager.WaitInitialized() >-> TypeHierarchy.prepare workspaceManager p

        override this.TypeHierarchySupertypes(p) = workspaceManager.WaitInitialized() >-> TypeHierarchy.supertypes workspaceManager p

        override this.TypeHierarchySubtypes(p) = workspaceManager.WaitInitialized() >-> TypeHierarchy.subtypes workspaceManager p

        override __.CSharpMetadata(p) = notImplemented


module Server =
    let logger = LogProvider.getLoggerByName "LSP"

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

    // TODO:
    // 1. log the begin and end of request.
    // 2. if there is exception during the request, log it.
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
        with ex ->
            logger.error (
                Log.setMessage "{name} crashed:"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
                >> Log.addException ex
            )
            3
