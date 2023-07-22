namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics
open System.Reflection
open System.Threading.Tasks

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open StreamJsonRpc
open FSharpPlus

open CSharpLanguageServer.Types
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging
open CSharpLanguageServer.State
open CSharpLanguageServer.Util

module LspUtils =
    /// Return the JSON-RPC "not implemented" error
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

    /// Do nothing and ignore the notification
    let ignoreNotification: Async<unit> = async.Return(())


open LspUtils

type CSharpLspServer(
        lspClient: CSharpLspClient,
        settings: ServerSettings,
        lspClientLogEventSink: LspClientLogEventSink
    ) =

    let logger = LogProvider.getLoggerByName "LSP"

    let logMessage m =
        logger.info (Log.setMessage m)
        async.Return ()

    let stateActor = MailboxProcessor.Start(
        serverEventLoop
            logMessage
            { emptyServerState with Settings = settings })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let diagnostics = MailboxProcessor.Start(
        diagnosticsEventLoop
            lspClient
            getDocumentForUriFromCurrentState)

    let mutable timer: System.Threading.Timer option = None

    let setupTimer () =
        timer <- Some (new System.Threading.Timer(
            System.Threading.TimerCallback(
                fun _ -> do diagnostics.Post(ProcessPendingDiagnostics)
                         do stateActor.Post(PeriodicTimerTick)),
            null, dueTime=1000, period=250))

    let mutable workspaceFolders: WorkspaceFolder list = []

    let withScope
            requestType
            (handlerFn: ServerRequestScope -> 'a -> Async<LspResult<'b>>)
            param =
        let requestName = handlerFn.ToString()

        // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object.)

        let startRequest rc = StartRequest (requestName, requestType, 0, rc)
        let requestId, semaphore = stateActor.PostAndReply(startRequest)

        let stateAcquisitionAndHandlerInvocation = async {
            do! semaphore.WaitAsync() |> Async.AwaitTask

            let! state = stateActor.PostAndAsyncReply(GetState)

            let scope = ServerRequestScope(requestId, state, stateActor.Post, logMessage)

            return! handlerFn scope param
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

    let withReadOnlyScope handlerFn = withScope ReadOnly handlerFn
    let withReadWriteScope handlerFn = withScope ReadWrite handlerFn

    let ignoreResult handlerFn = async {
        let! _ = handlerFn
        return ()
    }

    let getRegistrations (clientCapabilities: ClientCapabilities option): Registration list =
        let registrationBuilders =
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
              TextDocumentSync.registration
              TypeDefinition.registration
              TypeHierarchy.registration
              WorkspaceSymbol.registration ]
        registrationBuilders
        |> List.map ((|>) clientCapabilities)
        |> List.filter (Option.isSome)
        |> List.map (Option.get)

    let getServerCapabilities
        (lspClient: InitializeParams) =
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
                    ExecuteCommandProvider = ExecuteCommand.provider lspClient.Capabilities
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

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) =
            lspClientLogEventSink.SetLspClient(Some lspClient)

            let serverCapabilities = getServerCapabilities p
            p |> withReadWriteScope (Initialization.handleInitialize setupTimer serverCapabilities)

        override __.Initialized(p) =
            p |> withReadWriteScope (Initialization.handleInitialized lspClient stateActor getRegistrations)
              |> ignoreResult

        override __.Shutdown() =
            lspClientLogEventSink.SetLspClient(None)
            () |> async.Return

        override __.Exit() = ignoreNotification

        override this.TextDocumentHover(p) =
            p |> withReadOnlyScope Hover.handle

        override this.TextDocumentDidOpen(p) =
            p |> withReadOnlyScope (TextDocumentSync.didOpen logMessage diagnostics.Post)
              |> ignoreResult

        override this.TextDocumentDidChange(p) =
            p |> withReadWriteScope (TextDocumentSync.didChange diagnostics.Post)
              |> ignoreResult

        override this.TextDocumentDidClose(p) =
            p |> withReadWriteScope (TextDocumentSync.didClose diagnostics.Post)
              |> ignoreResult

        override this.TextDocumentDidSave(p) =
            p |> withReadWriteScope (TextDocumentSync.didSave logMessage diagnostics.Post)
              |> ignoreResult

        override this.TextDocumentWillSave(p) = ignoreNotification

        override this.TextDocumentWillSaveWaitUntil(p) = notImplemented

        override this.TextDocumentCompletion(p) =
            p |> withReadOnlyScope Completion.handle

        override this.CompletionItemResolve(p) = notImplemented

        override this.TextDocumentPrepareRename(p) =
            p |> withReadOnlyScope (Rename.prepare getDocumentForUriFromCurrentState)

        override this.TextDocumentRename(p) =
            p |> withReadOnlyScope Rename.handle

        override this.TextDocumentDefinition(p) =
            p |> withReadOnlyScope Definition.handle

        override this.TextDocumentReferences(p) =
            p |> withReadOnlyScope References.handle

        override this.TextDocumentDocumentHighlight(p) =
            p |> withReadOnlyScope DocumentHighlight.handle

        override this.TextDocumentDocumentLink(p) =
            p |> withReadOnlyScope DocumentLink.handle

        override this.DocumentLinkResolve(p) =
            p |> withReadOnlyScope DocumentLink.resolve

        override this.TextDocumentTypeDefinition(p) =
            p |> withReadOnlyScope TypeDefinition.handle

        override this.TextDocumentImplementation(p) =
            p |> withReadOnlyScope Implementation.handle

        override this.TextDocumentCodeAction(p) =
            p |> withReadOnlyScope (CodeAction.handle logMessage)

        override this.CodeActionResolve(p) =
            p |> withReadOnlyScope (CodeAction.resolve logMessage)

        override this.TextDocumentCodeLens(p) =
            p |> withReadOnlyScope CodeLens.handle

        override this.CodeLensResolve(p) =
            p |> withReadOnlyScope CodeLens.resolve

        override this.TextDocumentSignatureHelp(p) =
            p |> withReadOnlyScope SignatureHelp.handle

        override this.TextDocumentDocumentColor(p) =
            p |> withReadOnlyScope Color.handle

        override this.TextDocumentColorPresentation(p) =
            p |> withReadOnlyScope Color.present

        override this.TextDocumentFormatting(p) =
            p |> withReadOnlyScope DocumentFormatting.handle

        override this.TextDocumentRangeFormatting(p) =
            p |> withReadOnlyScope DocumentRangeFormatting.handle

        override this.TextDocumentOnTypeFormatting(p) =
            p |> withReadOnlyScope DocumentOnTypeFormatting.handle

        override this.TextDocumentDocumentSymbol(p) =
            p |> withReadOnlyScope DocumentSymbol.handle

        override __.WorkspaceDidChangeWatchedFiles(p) =
            p |> withReadWriteScope (Workspace.didChangeWatchedFiles logMessage diagnostics.Post)
              |> ignoreResult

        override __.WorkspaceDidChangeWorkspaceFolders(p) = ignoreNotification

        override __.WorkspaceDidChangeConfiguration(p) =
            p |> withReadWriteScope Workspace.didChangeConfiguration
              |> ignoreResult

        override __.WorkspaceWillCreateFiles(p) = notImplemented

        override __.WorkspaceDidCreateFiles(p) = ignoreNotification

        override __.WorkspaceWillRenameFiles(p) = notImplemented

        override __.WorkspaceDidRenameFiles(p) = ignoreNotification

        override __.WorkspaceWillDeleteFiles(p) = notImplemented

        override __.WorkspaceDidDeleteFiles(p) = ignoreNotification

        override this.WorkspaceSymbol(p) =
            p |> withReadOnlyScope WorkspaceSymbol.handle

        override this.WorkspaceExecuteCommand(p) =
            p |> withReadOnlyScope ExecuteCommand.handle

        override this.TextDocumentFoldingRange(p) =
            p |> withReadOnlyScope FoldingRange.handle

        override this.TextDocumentSelectionRange(p) =
            p |> withReadOnlyScope SelectionRange.handle

        override this.TextDocumentSemanticTokensFull(p) =
            p |> withReadOnlyScope SemanticTokens.handleFull

        override this.TextDocumentSemanticTokensFullDelta(p) =
            p |> withReadOnlyScope SemanticTokens.handleFullDelta

        override this.TextDocumentSemanticTokensRange(p) =
            p |> withReadOnlyScope SemanticTokens.handleRange

        override this.TextDocumentInlayHint(p) =
            p |> withReadOnlyScope InlayHint.handle

        override this.InlayHintResolve(p) =
            p |> withReadOnlyScope InlayHint.resolve

        override __.WorkDoneProgressCancel(p) = ignoreNotification

        override this.TextDocumentInlineValue(p) = notImplemented

        override this.TextDocumentPrepareCallHierarchy(p) =
            p |> withReadOnlyScope CallHierarchy.prepare

        override this.CallHierarchyIncomingCalls(p) =
            p |> withReadOnlyScope CallHierarchy.incomingCalls

        override this.CallHierarchyOutgoingCalls(p) =
            p |> withReadOnlyScope CallHierarchy.outgoingCalls

        override this.TextDocumentPrepareTypeHierarchy(p) =
            p |> withReadOnlyScope TypeHierarchy.prepare

        override this.TypeHierarchySupertypes(p) =
            p |> withReadOnlyScope TypeHierarchy.supertypes

        override this.TypeHierarchySubtypes(p) =
            p |> withReadOnlyScope TypeHierarchy.subtypes

        override this.TextDocumentDeclaration(p) =
            p |> withReadOnlyScope Declaration.handle

        override this.WorkspaceDiagnostic(p) = notImplemented

        override this.WorkspaceSymbolResolve(p) = notImplemented

        override this.TextDocumentDiagnostic(p) = notImplemented

        override this.TextDocumentLinkedEditingRange(p) =
            p |> withReadOnlyScope LinkedEditingRange.handle

        override this.TextDocumentMoniker(p) =
            p |> withReadOnlyScope Moniker.handle

        override this.CSharpMetadata(p) =
            p |> withReadOnlyScope CSharpMetadata.handle

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

    let startCore settings lspClientLogEventSink =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let serverCreator client =
            new CSharpLspServer(client, settings, lspClientLogEventSink) :> ICSharpLspServer

        let clientCreator = CSharpLspClient

        Ionide.LanguageServerProtocol.Server.start
            requestHandlings
            input
            output
            clientCreator
            serverCreator
            createRpc

    let start options lspClientLogEventSink =
        try
            let result = startCore options lspClientLogEventSink
            int result
        with ex ->
            logger.error (
                Log.setMessage "{name} crashed:"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
                >> Log.addException ex
            )
            3
