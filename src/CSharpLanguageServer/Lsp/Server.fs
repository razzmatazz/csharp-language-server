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
open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Runtime.ServerStateLoop
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

    let requestHandler requestName (handler: ActivateServerRequest -> 'a -> Async<LspResult<'b>>) requestParams =
        // We want to be careful and register/enqueue the request immediately w/o entering the associated `async` fn
        // to preserve request sequence outside of async context!
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object wrapped as Task.)
        let requestId = stateActor.PostAndReply(fun rc -> RegisterRequest(requestName, rc))

        let emittedEvents = System.Collections.Generic.List<ServerEvent>()

        let activateRequest mode (targetUri: string option) = async {
            let! state = stateActor.PostAndAsyncReply(fun rc -> RequestActivation(requestId, mode, targetUri, rc))
            let state = state :?> ServerState

            return
                ServerRequestContext(
                    mode,
                    state.LspClient,
                    state.Settings,
                    state.Workspace,
                    state.ClientCapabilities,
                    emittedEvents.Add)
        }

        let retireRequest (_: obj) =
            stateActor.Post(RetireRequest(requestId, emittedEvents |> List.ofSeq))

        let requestProcessing = async {
            let handlerAsync = handler activateRequest requestParams
            let! outcome = handlerAsync |> Async.Catch

            do retireRequest ()

            return
                match outcome with
                | Choice1Of2 result -> result
                | Choice2Of2 ex ->
                    match ex with
                    | :? TaskCanceledException -> LspResult.requestCancelled
                    | :? OperationCanceledException -> LspResult.requestCancelled
                    | _ -> LspResult.internalError (string ex)
        }

        Async.TryCancelled(requestProcessing, retireRequest)

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
            p
            |> requestHandler "initialize" (LifeCycle.handleInitialize lspClient getServerCapabilities)

        override __.Initialized _ =
            ()
            |> requestHandler "initialized" (LifeCycle.handleInitialized lspClient stateActor getDynamicRegistrations)
            |> ignoreResult

        override __.Shutdown() =
            () |> requestHandler "shutdown" LifeCycle.handleShutdown

        override __.Exit() = ignoreNotification

        override __.TextDocumentHover p =
            p |> requestHandler "textDocument/hover" Hover.handle

        override __.TextDocumentDidOpen p =
            p
            |> requestHandler "textDocument/didOpen" TextDocumentSync.didOpen
            |> ignoreResult

        override __.TextDocumentDidChange p =
            p
            |> requestHandler "textDocument/didChange" TextDocumentSync.didChange
            |> ignoreResult

        override __.TextDocumentDidClose p =
            p
            |> requestHandler "textDocument/didClose" TextDocumentSync.didClose
            |> ignoreResult

        override __.TextDocumentWillSave p =
            p
            |> requestHandler "textDocument/willSave" TextDocumentSync.willSave
            |> ignoreResult

        override __.TextDocumentWillSaveWaitUntil p =
            p
            |> requestHandler "textDocument/willSaveWaitUntil" TextDocumentSync.willSaveWaitUntil

        override __.TextDocumentDidSave p =
            p
            |> requestHandler "textDocument/didSave" TextDocumentSync.didSave
            |> ignoreResult

        override __.TextDocumentCompletion p =
            p |> requestHandler "textDocument/completion" Completion.handle

        override __.CompletionItemResolve p =
            p |> requestHandler "completionItem/resolve" Completion.resolve

        override __.TextDocumentPrepareRename p =
            p |> requestHandler "textDocument/prepareRename" Rename.prepare

        override __.TextDocumentRename p =
            p |> requestHandler "textDocument/rename" Rename.handle

        override __.TextDocumentDefinition p =
            p |> requestHandler "textDocument/definition" Definition.handle

        override __.TextDocumentReferences p =
            p |> requestHandler "textDocument/references" References.handle

        override __.TextDocumentDocumentHighlight p =
            p |> requestHandler "textDocument/documentHighlight" DocumentHighlight.handle

        override __.TextDocumentDocumentLink p =
            p |> requestHandler "textDocument/documentLink" DocumentLink.handle

        override __.DocumentLinkResolve p =
            p |> requestHandler "documentLink/resolve" DocumentLink.resolve

        override __.TextDocumentTypeDefinition p =
            p |> requestHandler "textDocument/typeDefinition" TypeDefinition.handle

        override __.TextDocumentImplementation p =
            p |> requestHandler "textDocument/implementation" Implementation.handle

        override __.TextDocumentCodeAction p =
            p |> requestHandler "textDocument/codeAction" CodeAction.handle

        override __.CodeActionResolve p =
            p |> requestHandler "codeAction/resolve" CodeAction.resolve

        override __.TextDocumentCodeLens p =
            p |> requestHandler "textDocument/codeLens" CodeLens.handle

        override __.CodeLensResolve p =
            p |> requestHandler "codeLens/resolve" CodeLens.resolve

        override __.TextDocumentSignatureHelp p =
            p |> requestHandler "textDocument/signatureHelp" SignatureHelp.handle

        override __.TextDocumentDocumentColor p =
            p |> requestHandler "textDocument/documentColor" Color.handle

        override __.TextDocumentColorPresentation p =
            p |> requestHandler "textDocument/colorPresentation" Color.present

        override __.TextDocumentFormatting p =
            p |> requestHandler "textDocument/formatting" DocumentFormatting.handle

        override __.TextDocumentRangeFormatting p =
            p
            |> requestHandler "textDocument/rangeFormatting" DocumentRangeFormatting.handle

        override __.TextDocumentOnTypeFormatting p =
            p
            |> requestHandler "textDocument/onTypeFormatting" DocumentOnTypeFormatting.handle

        override __.TextDocumentDocumentSymbol p =
            p |> requestHandler "textDocument/documentSymbol" DocumentSymbol.handle

        override __.WorkspaceDidChangeWatchedFiles p =
            p
            |> requestHandler "workspace/didChangeWatchedFiles" Workspace.didChangeWatchedFiles
            |> ignoreResult

        override __.WorkspaceDidChangeWorkspaceFolders p =
            p
            |> requestHandler "workspace/didChangeWorkspaceFolders" Workspace.didChangeWorkspaceFolders
            |> ignoreResult

        override __.WorkspaceDidChangeConfiguration p =
            p
            |> requestHandler "workspace/didChangeConfiguration" Workspace.didChangeConfiguration
            |> ignoreResult

        override __.WorkspaceWillCreateFiles _ =
            () |> requestHandler "workspace/willCreateFiles" (fun _ _ -> notImplemented)

        override __.WorkspaceDidCreateFiles _ = ignoreNotification

        override __.WorkspaceWillRenameFiles _ =
            () |> requestHandler "workspace/willRenameFiles" (fun _ _ -> notImplemented)

        override __.WorkspaceDidRenameFiles _ = ignoreNotification

        override __.WorkspaceWillDeleteFiles _ =
            () |> requestHandler "workspace/willDeleteFiles" (fun _ _ -> notImplemented)

        override __.WorkspaceDidDeleteFiles _ = ignoreNotification

        override __.WorkspaceSymbol p =
            p |> requestHandler "workspace/symbol" WorkspaceSymbol.handle

        override __.WorkspaceExecuteCommand p =
            p |> requestHandler "workspace/executeCommand" ExecuteCommand.handle

        override __.TextDocumentFoldingRange p =
            p |> requestHandler "textDocument/foldingRange" FoldingRange.handle

        override __.TextDocumentSelectionRange p =
            p |> requestHandler "textDocument/selectionRange" SelectionRange.handle

        override __.TextDocumentSemanticTokensFull p =
            p |> requestHandler "textDocument/semanticTokens/full" SemanticTokens.handleFull

        override __.TextDocumentSemanticTokensFullDelta p =
            p
            |> requestHandler "textDocument/semanticTokens/full/delta" SemanticTokens.handleFullDelta

        override __.TextDocumentSemanticTokensRange p =
            p
            |> requestHandler "textDocument/semanticTokens/range" SemanticTokens.handleRange

        override __.TextDocumentInlayHint p =
            p |> requestHandler "textDocument/inlayHint" InlayHint.handle

        override __.InlayHintResolve p =
            p |> requestHandler "inlayHint/resolve" InlayHint.resolve

        override __.WindowWorkDoneProgressCancel _ = raise (NotImplementedException())
        override __.TextDocumentInlineValue _ = notImplemented

        override __.TextDocumentPrepareCallHierarchy p =
            p |> requestHandler "textDocument/prepareCallHierarchy" CallHierarchy.prepare

        override __.CallHierarchyIncomingCalls p =
            p |> requestHandler "callHierarchy/incomingCalls" CallHierarchy.incomingCalls

        override __.CallHierarchyOutgoingCalls p =
            p |> requestHandler "callHierarchy/outgoingCalls" CallHierarchy.outgoingCalls

        override __.TextDocumentPrepareTypeHierarchy p =
            p |> requestHandler "textDocument/prepareTypeHierarchy" TypeHierarchy.prepare

        override __.TypeHierarchySupertypes p =
            p |> requestHandler "typeHierarchy/supertypes" TypeHierarchy.supertypes

        override __.TypeHierarchySubtypes p =
            p |> requestHandler "typeHierarchy/subtypes" TypeHierarchy.subtypes

        override __.TextDocumentDeclaration p =
            p |> requestHandler "textDocument/declaration" Declaration.handle

        override __.WorkspaceDiagnostic p =
            p |> requestHandler "workspace/diagnostic" Diagnostic.handleWorkspaceDiagnostic

        override __.CancelRequest _ = ignoreNotification
        override __.NotebookDocumentDidChange _ = ignoreNotification
        override __.NotebookDocumentDidClose _ = ignoreNotification
        override __.NotebookDocumentDidOpen _ = ignoreNotification
        override __.NotebookDocumentDidSave _ = ignoreNotification

        override __.WorkspaceSymbolResolve p =
            p |> requestHandler "workspaceSymbol/resolve" WorkspaceSymbol.resolve

        override __.TextDocumentDiagnostic p =
            p |> requestHandler "textDocument/diagnostic" Diagnostic.handle

        override __.TextDocumentLinkedEditingRange p =
            p |> requestHandler "textDocument/linkedEditingRange" LinkedEditingRange.handle

        override __.TextDocumentMoniker p =
            p |> requestHandler "textDocument/moniker" Moniker.handle

        override __.Progress _ = ignoreNotification
        override __.SetTrace _ = ignoreNotification

        override __.CSharpMetadata p =
            p |> requestHandler "csharp/metadata" CSharpMetadata.handle

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
