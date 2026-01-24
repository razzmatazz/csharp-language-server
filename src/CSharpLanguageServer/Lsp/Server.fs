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

type CSharpLspServer(lspClient: CSharpLspClient, settings: ServerSettings) =

    let _logger = Logging.getLoggerByName "Server"

    let stateActor =
        MailboxProcessor.Start(
            serverEventLoop
                { ServerState.Empty with
                    Settings = settings }
        )

    let mutable timer: Threading.Timer option = None

    let setupTimer () =
        timer <-
            Some(
                new Threading.Timer(
                    Threading.TimerCallback(fun _ -> do stateActor.Post PeriodicTimerTick),
                    null,
                    dueTime = 100,
                    period = 250
                )
            )

    let withContext
        requestMode
        requestName
        (requestUri: option<string>)
        (handlerFn: ServerRequestContext -> 'a -> Async<LspResult<'b>>)
        param
        =
        // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object.)

        let requestDetails =
            { Name = requestName
              Mode = requestMode
              Priority = 0
              Uri = requestUri }

        let requestId, semaphore =
            stateActor.PostAndReply(fun rc -> StartRequest(requestDetails, rc))

        let stateAcquisitionAndHandlerInvocation = async {
            do! semaphore.WaitAsync() |> Async.AwaitTask

            let! state = stateActor.PostAndAsyncReply GetState

            let context = ServerRequestContext(state, stateActor.Post)

            return! handlerFn context param
        }

        let wrapExceptionAsLspResult op = async {
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

    let withReadOnlyContext requestName requestUri handlerFn =
        withContext ReadOnly requestName requestUri handlerFn

    let withReadWriteContext requestName requestUri handlerFn =
        withContext ReadWrite requestName requestUri handlerFn

    let ignoreResult handlerFn = async {
        let! _ = handlerFn
        return ()
    }

    let getDynamicRegistrations
        (serverSettings: ServerSettings)
        (clientCapabilities: ClientCapabilities)
        : Registration list =
        [ CallHierarchy.registration
          CodeAction.registration
          CodeLens.registration
          Color.registration
          Completion.registration
          Declaration.registration
          Definition.registration
          Diagnostic.registration
          DocumentFormatting.registration
          DocumentHighlight.registration
          DocumentLink.registration
          DocumentOnTypeFormatting.registration
          DocumentRangeFormatting.registration
          DocumentSymbol.registration
          ExecuteCommand.registration
          FoldingRange.registration
          Hover.registration
          Implementation.registration
          InlayHint.registration
          InlineValue.registration
          LinkedEditingRange.registration
          Moniker.registration
          References.registration
          Rename.registration
          SelectionRange.registration
          SemanticTokens.registration
          SignatureHelp.registration
          TextDocumentSync.didOpenRegistration
          TextDocumentSync.didChangeRegistration
          TextDocumentSync.didSaveRegistration
          TextDocumentSync.didCloseRegistration
          TextDocumentSync.willSaveRegistration
          TextDocumentSync.willSaveWaitUntilRegistration
          TypeDefinition.registration
          TypeHierarchy.registration
          Workspace.didChangeWatchedFilesRegistration
          WorkspaceSymbol.registration ]
        |> List.choose (fun regFunc -> regFunc serverSettings clientCapabilities)

    let getServerCapabilities (settings: ServerSettings) (lspClient: InitializeParams) =
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
            DiagnosticProvider = Diagnostic.provider settings lspClient.Capabilities
            WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities
            Workspace = Workspace.provider lspClient.Capabilities }

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize p =
            let state = stateActor.PostAndReply GetState
            let serverCapabilities = getServerCapabilities state.Settings p

            p
            |> withReadWriteContext
                "initialize"
                None
                (Initialization.handleInitialize lspClient setupTimer serverCapabilities)

        override __.Initialized _ =
            ()
            |> withReadWriteContext
                "initialized"
                None
                (Initialization.handleInitialized lspClient stateActor getDynamicRegistrations)
            |> ignoreResult

        override __.Shutdown() =
            () |> withReadWriteContext "shutdown" None Initialization.handleShutdown

        override __.Exit() = ignoreNotification

        override __.TextDocumentHover p =
            p |> withReadOnlyContext "textDocument/hover" (Some p.TextDocument.Uri) Hover.handle

        override __.TextDocumentDidOpen p =
            p
            |> withReadOnlyContext "textDocument/didOpen" (Some p.TextDocument.Uri) TextDocumentSync.didOpen
            |> ignoreResult

        override __.TextDocumentDidChange p =
            p
            |> withReadWriteContext "textDocument/didChange" (Some p.TextDocument.Uri) TextDocumentSync.didChange
            |> ignoreResult

        override __.TextDocumentDidClose p =
            p
            |> withReadWriteContext "textDocument/didClose" (Some p.TextDocument.Uri) TextDocumentSync.didClose
            |> ignoreResult

        override __.TextDocumentWillSave p =
            p
            |> withReadWriteContext "textDocument/willSave" (Some p.TextDocument.Uri) TextDocumentSync.willSave
            |> ignoreResult

        override __.TextDocumentWillSaveWaitUntil p =
            p
            |> withReadWriteContext
                "textDocument/willSaveWaitUntil"
                (Some p.TextDocument.Uri)
                TextDocumentSync.willSaveWaitUntil

        override __.TextDocumentDidSave p =
            p
            |> withReadWriteContext "textDocument/didSave" (Some p.TextDocument.Uri) TextDocumentSync.didSave
            |> ignoreResult

        override __.TextDocumentCompletion p =
            p |> withReadOnlyContext "textDocument/completion" (Some p.TextDocument.Uri) Completion.handle

        override __.CompletionItemResolve p =
            p |> withReadOnlyContext "completionItem/resolve" (Some p.TextDocument.Uri) Completion.resolve

        override __.TextDocumentPrepareRename p =
            p |> withReadOnlyContext "textDocument/prepareRename" (Some p.TextDocument.Uri) Rename.prepare

        override __.TextDocumentRename p =
            p |> withReadOnlyContext "textDocument/rename" (Some p.TextDocument.Uri) Rename.handle

        override __.TextDocumentDefinition p =
            p |> withReadOnlyContext "textDocument/definition" (Some p.TextDocument.Uri) Definition.handle

        override __.TextDocumentReferences p =
            p |> withReadOnlyContext "textDocument/references" (Some p.TextDocument.Uri) References.handle

        override __.TextDocumentDocumentHighlight p =
            p
            |> withReadOnlyContext "textDocument/documentHighlight" (Some p.TextDocument.Uri) DocumentHighlight.handle

        override __.TextDocumentDocumentLink p =
            p |> withReadOnlyContext "textDocument/documentLink" (Some p.TextDocument.Uri) DocumentLink.handle

        override __.DocumentLinkResolve p =
            p |> withReadOnlyContext "documentLink/resolve" (Some p.TextDocument.Uri) DocumentLink.resolve

        override __.TextDocumentTypeDefinition p =
            p |> withReadOnlyContext "textDocument/typeDefinition" (Some p.TextDocument.Uri) TypeDefinition.handle

        override __.TextDocumentImplementation p =
            p |> withReadOnlyContext "textDocument/implementation" (Some p.TextDocument.Uri) Implementation.handle

        override __.TextDocumentCodeAction p =
            p |> withReadOnlyContext "textDocument/codeAction" (Some p.TextDocument.Uri) CodeAction.handle

        override __.CodeActionResolve p =
            p |> withReadOnlyContext "codeAction/resolve" (Some p.TextDocument.Uri) CodeAction.resolve

        override __.TextDocumentCodeLens p =
            p |> withReadOnlyContext "textDocument/codeLens" (Some p.TextDocument.Uri) CodeLens.handle

        override __.CodeLensResolve p =
            p |> withReadOnlyContext "codeLens/resolve" (Some p.TextDocument.Uri) CodeLens.resolve

        override __.TextDocumentSignatureHelp p =
            p |> withReadOnlyContext "textDocument/signatureHelp" (Some p.TextDocument.Uri) SignatureHelp.handle

        override __.TextDocumentDocumentColor p =
            p |> withReadOnlyContext "textDocument/documentColor" (Some p.TextDocument.Uri) Color.handle

        override __.TextDocumentColorPresentation p =
            p |> withReadOnlyContext "textDocument/colorPresentation" (Some p.TextDocument.Uri) Color.present

        override __.TextDocumentFormatting p =
            p |> withReadOnlyContext "textDocument/formatting" (Some p.TextDocument.Uri) DocumentFormatting.handle

        override __.TextDocumentRangeFormatting p =
            p
            |> withReadOnlyContext "textDocument/rangeFormatting" (Some p.TextDocument.Uri) DocumentRangeFormatting.handle

        override __.TextDocumentOnTypeFormatting p =
            p
            |> withReadOnlyContext "textDocument/onTypeFormatting" (Some p.TextDocument.Uri) DocumentOnTypeFormatting.handle

        override __.TextDocumentDocumentSymbol p =
            p |> withReadOnlyContext "textDocument/documentSymbol" (Some p.TextDocument.Uri) DocumentSymbol.handle

        override __.WorkspaceDidChangeWatchedFiles p =
            p
            |> withReadWriteContext "workspace/didChangeWatchedFiles" None Workspace.didChangeWatchedFiles
            |> ignoreResult

        override __.WorkspaceDidChangeWorkspaceFolders p =
            p
            |> withReadWriteContext "workspace/didChangeWorkspaceFolders" None Workspace.didChangeWorkspaceFolders
            |> ignoreResult

        override __.WorkspaceDidChangeConfiguration p =
            p
            |> withReadWriteContext "workspace/didChangeConfiguration" None Workspace.didChangeConfiguration
            |> ignoreResult

        override __.WorkspaceWillCreateFiles _ =
            ()
            |> withReadOnlyContext "workspace/willCreateFiles" None (fun _ _ -> notImplemented)

        override __.WorkspaceDidCreateFiles _ = ignoreNotification

        override __.WorkspaceWillRenameFiles _ =
            ()
            |> withReadOnlyContext "workspace/willRenameFiles" None (fun _ _ -> notImplemented)

        override __.WorkspaceDidRenameFiles _ = ignoreNotification

        override __.WorkspaceWillDeleteFiles _ =
            ()
            |> withReadOnlyContext "workspace/willDeleteFiles" None (fun _ _ -> notImplemented)

        override __.WorkspaceDidDeleteFiles _ = ignoreNotification

        override __.WorkspaceSymbol p =
            p |> withReadOnlyContext "workspace/symbol" None WorkspaceSymbol.handle

        override __.WorkspaceExecuteCommand p =
            p |> withReadOnlyContext "workspace/executeCommand" None ExecuteCommand.handle

        override __.TextDocumentFoldingRange p =
            p |> withReadOnlyContext "textDocument/foldingRange" (Some p.TextDocument.Uri) FoldingRange.handle

        override __.TextDocumentSelectionRange p =
            p |> withReadOnlyContext "textDocument/selectionRange" (Some p.TextDocument.Uri) SelectionRange.handle

        override __.TextDocumentSemanticTokensFull p =
            p
            |> withReadOnlyContext "textDocument/semanticTokens/full" (Some p.TextDocument.Uri) SemanticTokens.handleFull

        override __.TextDocumentSemanticTokensFullDelta p =
            p
            |> withReadOnlyContext "textDocument/semanticTokens/full/delta" (Some p.TextDocument.Uri) SemanticTokens.handleFullDelta

        override __.TextDocumentSemanticTokensRange p =
            p
            |> withReadOnlyContext "textDocument/semanticTokens/range" (Some p.TextDocument.Uri) SemanticTokens.handleRange

        override __.TextDocumentInlayHint p =
            p |> withReadOnlyContext "textDocument/inlayHint" (Some p.TextDocument.Uri) InlayHint.handle

        override __.InlayHintResolve p =
            p |> withReadOnlyContext "inlayHint/resolve" (Some p.TextDocument.Uri) InlayHint.resolve

        override __.WindowWorkDoneProgressCancel _ = raise (NotImplementedException())
        override __.TextDocumentInlineValue _ = notImplemented

        override __.TextDocumentPrepareCallHierarchy p =
            p
            |> withReadOnlyContext "textDocument/prepareCallHierarchy" (Some p.TextDocument.Uri) CallHierarchy.prepare

        override __.CallHierarchyIncomingCalls p =
            p
            |> withReadOnlyContext "callHierarchy/incomingCalls" (Some p.TextDocument.Uri) CallHierarchy.incomingCalls

        override __.CallHierarchyOutgoingCalls p =
            p
            |> withReadOnlyContext "callHierarchy/outgoingCalls" (Some p.TextDocument.Uri) CallHierarchy.outgoingCalls

        override __.TextDocumentPrepareTypeHierarchy p =
            p
            |> withReadOnlyContext "textDocument/prepareTypeHierarchy" (Some p.TextDocument.Uri) TypeHierarchy.prepare

        override __.TypeHierarchySupertypes p =
            p |> withReadOnlyContext "typeHierarchy/supertypes" (Some p.TextDocument.Uri) TypeHierarchy.supertypes

        override __.TypeHierarchySubtypes p =
            p |> withReadOnlyContext "typeHierarchy/subtypes" (Some p.TextDocument.Uri) TypeHierarchy.subtypes

        override __.TextDocumentDeclaration p =
            p |> withReadOnlyContext "textDocument/declaration" (Some p.TextDocument.Uri) Declaration.handle

        override __.WorkspaceDiagnostic p =
            p
            |> withReadOnlyContext "workspace/diagnostic" (Some p.TextDocument.Uri) Diagnostic.handleWorkspaceDiagnostic

        override __.CancelRequest _ = ignoreNotification
        override __.NotebookDocumentDidChange _ = ignoreNotification
        override __.NotebookDocumentDidClose _ = ignoreNotification
        override __.NotebookDocumentDidOpen _ = ignoreNotification
        override __.NotebookDocumentDidSave _ = ignoreNotification

        override __.WorkspaceSymbolResolve p =
            p |> withReadOnlyContext "workspaceSymbol/resolve" (Some p.TextDocument.Uri) WorkspaceSymbol.resolve

        override __.TextDocumentDiagnostic p =
            p |> withReadOnlyContext "textDocument/diagnostic" (Some p.TextDocument.Uri) Diagnostic.handle

        override __.TextDocumentLinkedEditingRange p =
            p
            |> withReadOnlyContext "textDocument/linkedEditingRange" (Some p.TextDocument.Uri) LinkedEditingRange.handle

        override __.TextDocumentMoniker p =
            p |> withReadOnlyContext "textDocument/moniker" (Some p.TextDocument.Uri) Moniker.handle

        override __.Progress _ = ignoreNotification
        override __.SetTrace _ = ignoreNotification

        override __.CSharpMetadata p =
            p |> withReadOnlyContext "csharp/metadata" (Some p.TextDocument.Uri) CSharpMetadata.handle

module Server =
    let logger = Logging.getLoggerByName "LSP"

    let private createRpc (handler: IJsonRpcMessageHandler) : JsonRpc =
        let rec (|HandleableException|_|) (e: exn) =
            match e with
            | :? LocalRpcException -> Some()
            | :? TaskCanceledException -> Some()
            | :? OperationCanceledException -> Some()
            | :? AggregateException as aex ->
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

        [ "csharp/metadata", requestHandling (fun s p -> s.CSharpMetadata p) ]
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

        let clientCreator = fun (a, b) -> new CSharpLspClient(a, b)

        start requestHandlings input output clientCreator serverCreator createRpc

    let start options =
        try
            let result = startCore options
            int result
        with ex ->
            logger.LogError("{name} crashed", ex, Process.GetCurrentProcess().ProcessName)
            3
