namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics
open System.Threading.Tasks

open Ionide.LanguageServerProtocol.Mappings
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open StreamJsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Types
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Util

module LspUtils =
    /// Return the JSON-RPC "not implemented" error
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

    /// Do nothing and ignore the notification
    let ignoreNotification: Async<unit> = async.Return(())


open LspUtils

type CSharpLspServer(
        lspClient: CSharpLspClient,
        settings: ServerSettings
    ) =

    let _logger = Logging.getLoggerByName "Server"

    let stateActor = MailboxProcessor.Start(
        serverEventLoop
            { ServerState.Empty with Settings = settings })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let mutable timer: System.Threading.Timer option = None

    let setupTimer () =
        timer <- Some (new System.Threading.Timer(
            System.Threading.TimerCallback(
                fun _ -> do stateActor.Post(PeriodicTimerTick)),
            null, dueTime=100, period=250))

    let mutable _workspaceFolders: WorkspaceFolder list = []

    let withContext
            requestName
            requestMode
            (handlerFn: ServerRequestContext -> 'a -> Async<LspResult<'b>>)
            param =
        // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object.)

        let startRequest rc = StartRequest (requestName, requestMode, 0, rc)
        let requestId, semaphore = stateActor.PostAndReply(startRequest)

        let stateAcquisitionAndHandlerInvocation = async {
            do! semaphore.WaitAsync() |> Async.AwaitTask

            let! state = stateActor.PostAndAsyncReply(GetState)

            let context = ServerRequestContext(requestId, state, stateActor.Post)

            return! handlerFn context param
        }

        let wrapExceptionAsLspResult op =
            async {
                let! resultOrExn = op |> Async.Catch

                return
                    match resultOrExn with
                    | Choice1Of2 result -> result
                    | Choice2Of2 exn ->
                        match exn with
                        | :? TaskCanceledException -> LspResult.requestCancelled
                        | :? OperationCanceledException -> LspResult.requestCancelled
                        | _ -> LspResult.internalError (string exn)
            }

        stateAcquisitionAndHandlerInvocation
        |> wrapExceptionAsLspResult
        |> unwindProtect (fun () -> stateActor.Post(FinishRequest requestId))

    let withReadOnlyContext requestName handlerFn = withContext requestName ReadOnly handlerFn
    let withReadWriteContext requestName handlerFn = withContext requestName ReadWrite handlerFn

    let ignoreResult handlerFn = async {
        let! _ = handlerFn
        return ()
    }

    let getDynamicRegistrations (clientCapabilities: ClientCapabilities): Registration list =
        [ Workspace.didChangeWatchedFilesRegistration ]
        |> List.choose (fun regFunc -> regFunc clientCapabilities)

    let getServerCapabilities
        (lspClient: InitializeParams) =
                { ServerCapabilities.Default with
                    TextDocumentSync = TextDocumentSync.provider lspClient.Capabilities |> Option.map U2.C1
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
                    ExecuteCommandProvider = ExecuteCommand.provider lspClient.Capabilities
                    // SelectionRangeProvider = SelectionRange.provider lspClient.Capabilities
                    // LinkedEditingRangeProvider = LinkedEditingRange.provider lspClient.Capabilities
                    CallHierarchyProvider = CallHierarchy.provider lspClient.Capabilities
                    SemanticTokensProvider = SemanticTokens.provider lspClient.Capabilities
                    // MonikerProvider = Moniker.provider lspClient.Capabilities
                    TypeHierarchyProvider = TypeHierarchy.provider lspClient.Capabilities
                    // InlineValueProvider = InlineValue.provider lspClient.Capabilities
                    InlayHintProvider = InlayHint.provider lspClient.Capabilities
                    DiagnosticProvider = Diagnostic.provider lspClient.Capabilities
                    WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities
                    Workspace = Workspace.provider lspClient.Capabilities
                }

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) =
            let serverCapabilities = getServerCapabilities p
            p |> withReadWriteContext "initialize" (Initialization.handleInitialize lspClient setupTimer serverCapabilities)

        override __.Initialized(_) =
            () |> withReadWriteContext "initialized" (Initialization.handleInitialized lspClient stateActor getDynamicRegistrations)
               |> ignoreResult

        override __.Shutdown() = () |> withReadWriteContext "shutdown" Initialization.handleShutdown
        override __.Exit() = ignoreNotification
        override __.TextDocumentHover(p) = p |> withReadOnlyContext "textDocument/hover" Hover.handle
        override __.TextDocumentDidOpen(p) = p |> withReadOnlyContext "textDocument/didOpen" TextDocumentSync.didOpen |> ignoreResult
        override __.TextDocumentDidChange(p) = p |> withReadWriteContext "textDocument/didChange" TextDocumentSync.didChange |> ignoreResult
        override __.TextDocumentDidClose(p) = p |> withReadWriteContext "textDocument/didClose" TextDocumentSync.didClose |> ignoreResult
        override __.TextDocumentWillSave(p) = p |> withReadWriteContext "textDocument/willSave" TextDocumentSync.willSave |> ignoreResult
        override __.TextDocumentWillSaveWaitUntil(p) = p |> withReadWriteContext "textDocument/willSaveWaitUntil" TextDocumentSync.willSaveWaitUntil
        override __.TextDocumentDidSave(p) = p |> withReadWriteContext "textDocument/didSave" TextDocumentSync.didSave |> ignoreResult
        override __.TextDocumentCompletion(p) = p |> withReadOnlyContext "textDocument/completion" Completion.handle
        override __.CompletionItemResolve(p) = p |> withReadOnlyContext "completionItem/resolve" Completion.resolve
        override __.TextDocumentPrepareRename(p) = p |> withReadOnlyContext "textDocument/prepareRename" Rename.prepare
        override __.TextDocumentRename(p) = p |> withReadOnlyContext "textDocument/rename" Rename.handle
        override __.TextDocumentDefinition(p) = p |> withReadOnlyContext "textDocument/definition" Definition.handle
        override __.TextDocumentReferences(p) = p |> withReadOnlyContext "textDocument/references" References.handle
        override __.TextDocumentDocumentHighlight(p) = p |> withReadOnlyContext "textDocument/documentHighlight" DocumentHighlight.handle
        override __.TextDocumentDocumentLink(p) = p |> withReadOnlyContext "textDocument/documentLink" DocumentLink.handle
        override __.DocumentLinkResolve(p) = p |> withReadOnlyContext "documentLink/resolve" DocumentLink.resolve
        override __.TextDocumentTypeDefinition(p) = p |> withReadOnlyContext "textDocument/typeDefinition" TypeDefinition.handle
        override __.TextDocumentImplementation(p) = p |> withReadOnlyContext "textDocument/implementation" Implementation.handle
        override __.TextDocumentCodeAction(p) = p |> withReadOnlyContext "textDocument/codeAction" CodeAction.handle
        override __.CodeActionResolve(p) = p |> withReadOnlyContext "codeAction/resolve" CodeAction.resolve
        override __.TextDocumentCodeLens(p) = p |> withReadOnlyContext "textDocument/codeLens" CodeLens.handle
        override __.CodeLensResolve(p) = p |> withReadOnlyContext "codeLens/resolve" CodeLens.resolve
        override __.TextDocumentSignatureHelp(p) = p |> withReadOnlyContext "textDocument/signatureHelp" SignatureHelp.handle
        override __.TextDocumentDocumentColor(p) = p |> withReadOnlyContext "textDocument/documentColor" Color.handle
        override __.TextDocumentColorPresentation(p) = p |> withReadOnlyContext "textDocument/colorPresentation" Color.present
        override __.TextDocumentFormatting(p) = p |> withReadOnlyContext "textDocument/formatting" DocumentFormatting.handle
        override __.TextDocumentRangeFormatting(p) = p |> withReadOnlyContext "textDocument/rangeFormatting" DocumentRangeFormatting.handle
        override __.TextDocumentOnTypeFormatting(p) = p |> withReadOnlyContext "textDocument/onTypeFormatting" DocumentOnTypeFormatting.handle
        override __.TextDocumentDocumentSymbol(p) = p |> withReadOnlyContext "textDocument/documentSymbol" DocumentSymbol.handle
        override __.WorkspaceDidChangeWatchedFiles(p) = p |> withReadWriteContext "workspace/didChangeWatchedFiles" Workspace.didChangeWatchedFiles |> ignoreResult
        override __.WorkspaceDidChangeWorkspaceFolders(_p) = ignoreNotification
        override __.WorkspaceDidChangeConfiguration(p) = p |> withReadWriteContext "workspace/didChangeConfiguration" Workspace.didChangeConfiguration |> ignoreResult
        override __.WorkspaceWillCreateFiles(_) = () |> withReadOnlyContext "workspace/willCreateFiles" (fun _ _ -> notImplemented)
        override __.WorkspaceDidCreateFiles(_) = ignoreNotification
        override __.WorkspaceWillRenameFiles(_) = () |> withReadOnlyContext "workspace/willRenameFiles" (fun _ _ -> notImplemented)
        override __.WorkspaceDidRenameFiles(_) = ignoreNotification
        override __.WorkspaceWillDeleteFiles(_) = () |> withReadOnlyContext "workspace/willDeleteFiles" (fun _ _ -> notImplemented)
        override __.WorkspaceDidDeleteFiles(_) = ignoreNotification
        override __.WorkspaceSymbol(p) = p |> withReadOnlyContext "workspace/symbol" WorkspaceSymbol.handle
        override __.WorkspaceExecuteCommand(p) = p |> withReadOnlyContext "workspace/executeCommand" ExecuteCommand.handle
        override __.TextDocumentFoldingRange(p) = p |> withReadOnlyContext "textDocument/foldingRange" FoldingRange.handle
        override __.TextDocumentSelectionRange(p) = p |> withReadOnlyContext "textDocument/selectionRange" SelectionRange.handle
        override __.TextDocumentSemanticTokensFull(p) = p |> withReadOnlyContext "textDocument/semanticTokens/full" SemanticTokens.handleFull
        override __.TextDocumentSemanticTokensFullDelta(p) = p |> withReadOnlyContext "textDocument/semanticTokens/full/delta" SemanticTokens.handleFullDelta
        override __.TextDocumentSemanticTokensRange(p) = p |> withReadOnlyContext "textDocument/semanticTokens/range" SemanticTokens.handleRange
        override __.TextDocumentInlayHint(p) = p |> withReadOnlyContext "textDocument/inlayHint" InlayHint.handle
        override __.InlayHintResolve(p) = p |> withReadOnlyContext "inlayHint/resolve" InlayHint.resolve
        override __.WindowWorkDoneProgressCancel (_) = raise (System.NotImplementedException())
        override __.TextDocumentInlineValue(_) = notImplemented
        override __.TextDocumentPrepareCallHierarchy(p) = p |> withReadOnlyContext "textDocument/prepareCallHierarchy" CallHierarchy.prepare
        override __.CallHierarchyIncomingCalls(p) = p |> withReadOnlyContext "callHierarchy/incomingCalls" CallHierarchy.incomingCalls
        override __.CallHierarchyOutgoingCalls(p) = p |> withReadOnlyContext "callHierarchy/outgoingCalls" CallHierarchy.outgoingCalls
        override __.TextDocumentPrepareTypeHierarchy(p) = p |> withReadOnlyContext "textDocument/prepareTypeHierarchy" TypeHierarchy.prepare
        override __.TypeHierarchySupertypes(p) = p |> withReadOnlyContext "typeHierarchy/supertypes" TypeHierarchy.supertypes
        override __.TypeHierarchySubtypes(p) = p |> withReadOnlyContext "typeHierarchy/subtypes" TypeHierarchy.subtypes
        override __.TextDocumentDeclaration(p) = p |> withReadOnlyContext "textDocument/declaration" Declaration.handle
        override __.WorkspaceDiagnostic(p) = p |> withReadOnlyContext "workspace/diagnostic" Diagnostic.handleWorkspaceDiagnostic
        override __.CancelRequest(_) = ignoreNotification
        override __.NotebookDocumentDidChange(_) = ignoreNotification
        override __.NotebookDocumentDidClose(_) = ignoreNotification
        override __.NotebookDocumentDidOpen(_) = ignoreNotification
        override __.NotebookDocumentDidSave(_) = ignoreNotification
        override __.WorkspaceSymbolResolve(p) = p |> withReadOnlyContext "workspaceSymbol/resolve" WorkspaceSymbol.resolve
        override __.TextDocumentDiagnostic(p) = p |> withReadOnlyContext "textDocument/diagnostic" Diagnostic.handle
        override __.TextDocumentLinkedEditingRange(p) = p |> withReadOnlyContext "textDocument/linkedEditingRange" LinkedEditingRange.handle
        override __.TextDocumentMoniker(p) = p |> withReadOnlyContext "textDocument/moniker" Moniker.handle
        override __.Progress(_) = ignoreNotification
        override __.SetTrace(_) = ignoreNotification
        override __.CSharpMetadata(p) = p |> withReadOnlyContext "csharp/metadata" CSharpMetadata.handle

module Server =
    let logger = Logging.getLoggerByName "LSP"

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
            member _.IsFatalException(ex: Exception) =
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

    let startCore settings =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let serverCreator client =
            new CSharpLspServer(client, settings) :> ICSharpLspServer

        let clientCreator =
            fun (a, b) -> new CSharpLspClient(a, b)

        Ionide.LanguageServerProtocol.Server.start
            requestHandlings
            input
            output
            clientCreator
            serverCreator
            createRpc

    let start options =
        try
            let result = startCore options
            int result
        with ex ->
            logger.LogError("{name} crashed", ex, (Process.GetCurrentProcess().ProcessName))
            3
