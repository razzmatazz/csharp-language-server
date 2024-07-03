namespace CSharpLanguageServer.Lsp

open System
open System.Diagnostics
open System.Threading.Tasks

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open StreamJsonRpc
open FSharpPlus

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

    let logger = LogProvider.getLoggerByName "Server"

    let stateActor = MailboxProcessor.Start(
        serverEventLoop
            { emptyServerState with Settings = settings })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let mutable timer: System.Threading.Timer option = None

    let setupTimer () =
        timer <- Some (new System.Threading.Timer(
            System.Threading.TimerCallback(
                fun _ -> do stateActor.Post(PeriodicTimerTick)),
            null, dueTime=100, period=250))

    let mutable workspaceFolders: WorkspaceFolder list = []

    let withContext
            requestType
            (handlerFn: ServerRequestContext -> 'a -> Async<LspResult<'b>>)
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

    let withReadOnlyContext handlerFn = withContext ReadOnly handlerFn
    let withReadWriteContext handlerFn = withContext ReadWrite handlerFn

    let ignoreResult handlerFn = async {
        let! _ = handlerFn
        return ()
    }

    let getRegistrations (clientCapabilities: ClientCapabilities): Registration list =
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
              TextDocumentSync.didOpenRegistration
              TextDocumentSync.didChangeRegistration
              TextDocumentSync.didSaveRegistration
              TextDocumentSync.didCloseRegistration
              TextDocumentSync.willSaveRegistration
              TextDocumentSync.willSaveWaitUntilRegistration
              TypeDefinition.registration
              TypeHierarchy.registration
              Workspace.registration
              WorkspaceSymbol.registration ]
        registrationBuilders
        |> List.map ((|>) clientCapabilities)
        |> List.filter (Option.isSome)
        |> List.map (Option.get)

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
                    WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities }

    interface ICSharpLspServer with
        override __.Dispose() = ()

        override __.Initialize(p) =
            let serverCapabilities = getServerCapabilities p
            p |> withReadWriteContext (Initialization.handleInitialize lspClient setupTimer serverCapabilities)

        override __.Initialized() =
            () |> withReadWriteContext (Initialization.handleInitialized lspClient stateActor getRegistrations)
               |> ignoreResult

        override __.Shutdown() =
            () |> withReadWriteContext Initialization.handleShutdown |> ignoreResult

        override __.Exit() = ignoreNotification

        override this.TextDocumentHover(p) =
            p |> withReadOnlyContext Hover.handle

        override this.TextDocumentDidOpen(p) =
            p |> withReadOnlyContext (TextDocumentSync.didOpen)
              |> ignoreResult

        override this.TextDocumentDidChange(p) =
            p |> withReadWriteContext (TextDocumentSync.didChange)
              |> ignoreResult

        override this.TextDocumentDidClose(p) =
            p |> withReadWriteContext (TextDocumentSync.didClose)
              |> ignoreResult

        override this.TextDocumentWillSave(p) =
            p |> withReadWriteContext TextDocumentSync.willSave
              |> ignoreResult

        override this.TextDocumentWillSaveWaitUntil(p) =
            p |> withReadWriteContext TextDocumentSync.willSaveWaitUntil

        override this.TextDocumentDidSave(p) =
            p |> withReadWriteContext (TextDocumentSync.didSave)
              |> ignoreResult

        override this.TextDocumentCompletion(p) =
            p |> withReadOnlyContext Completion.handle

        override this.CompletionItemResolve(p) =
            p |> withReadOnlyContext Completion.resolve

        override this.TextDocumentPrepareRename(p) =
            p |> withReadOnlyContext Rename.prepare

        override this.TextDocumentRename(p) =
            p |> withReadOnlyContext Rename.handle

        override this.TextDocumentDefinition(p) =
            p |> withReadOnlyContext Definition.handle

        override this.TextDocumentReferences(p) =
            p |> withReadOnlyContext References.handle

        override this.TextDocumentDocumentHighlight(p) =
            p |> withReadOnlyContext DocumentHighlight.handle

        override this.TextDocumentDocumentLink(p) =
            p |> withReadOnlyContext DocumentLink.handle

        override this.DocumentLinkResolve(p) =
            p |> withReadOnlyContext DocumentLink.resolve

        override this.TextDocumentTypeDefinition(p) =
            p |> withReadOnlyContext TypeDefinition.handle

        override this.TextDocumentImplementation(p) =
            p |> withReadOnlyContext Implementation.handle

        override this.TextDocumentCodeAction(p) =
            p |> withReadOnlyContext CodeAction.handle

        override this.CodeActionResolve(p) =
            p |> withReadOnlyContext CodeAction.resolve

        override this.TextDocumentCodeLens(p) =
            p |> withReadOnlyContext CodeLens.handle

        override this.CodeLensResolve(p) =
            p |> withReadOnlyContext CodeLens.resolve

        override this.TextDocumentSignatureHelp(p) =
            p |> withReadOnlyContext SignatureHelp.handle

        override this.TextDocumentDocumentColor(p) =
            p |> withReadOnlyContext Color.handle

        override this.TextDocumentColorPresentation(p) =
            p |> withReadOnlyContext Color.present

        override this.TextDocumentFormatting(p) =
            p |> withReadOnlyContext DocumentFormatting.handle

        override this.TextDocumentRangeFormatting(p) =
            p |> withReadOnlyContext DocumentRangeFormatting.handle

        override this.TextDocumentOnTypeFormatting(p) =
            p |> withReadOnlyContext DocumentOnTypeFormatting.handle

        override this.TextDocumentDocumentSymbol(p) =
            p |> withReadOnlyContext DocumentSymbol.handle

        override __.WorkspaceDidChangeWatchedFiles(p) =
            p |> withReadWriteContext Workspace.didChangeWatchedFiles
              |> ignoreResult

        override __.WorkspaceDidChangeWorkspaceFolders(_p) = ignoreNotification

        override __.WorkspaceDidChangeConfiguration(p) =
            p |> withReadWriteContext Workspace.didChangeConfiguration
              |> ignoreResult

        override __.WorkspaceWillCreateFiles(p) = notImplemented

        override __.WorkspaceDidCreateFiles(p) = ignoreNotification

        override __.WorkspaceWillRenameFiles(p) = notImplemented

        override __.WorkspaceDidRenameFiles(p) = ignoreNotification

        override __.WorkspaceWillDeleteFiles(p) = notImplemented

        override __.WorkspaceDidDeleteFiles(p) = ignoreNotification

        override this.WorkspaceSymbol(p) =
            p |> withReadOnlyContext WorkspaceSymbol.handle

        override this.WorkspaceExecuteCommand(p) =
            p |> withReadOnlyContext ExecuteCommand.handle

        override this.TextDocumentFoldingRange(p) =
            p |> withReadOnlyContext FoldingRange.handle

        override this.TextDocumentSelectionRange(p) =
            p |> withReadOnlyContext SelectionRange.handle

        override this.TextDocumentSemanticTokensFull(p) =
            p |> withReadOnlyContext SemanticTokens.handleFull

        override this.TextDocumentSemanticTokensFullDelta(p) =
            p |> withReadOnlyContext SemanticTokens.handleFullDelta

        override this.TextDocumentSemanticTokensRange(p) =
            p |> withReadOnlyContext SemanticTokens.handleRange

        override this.TextDocumentInlayHint(p) =
            p |> withReadOnlyContext InlayHint.handle

        override this.InlayHintResolve(p) =
            p |> withReadOnlyContext InlayHint.resolve

        override __.WorkDoneProgressCancel(p) = ignoreNotification

        override this.TextDocumentInlineValue(p) = notImplemented

        override this.TextDocumentPrepareCallHierarchy(p) =
            p |> withReadOnlyContext CallHierarchy.prepare

        override this.CallHierarchyIncomingCalls(p) =
            p |> withReadOnlyContext CallHierarchy.incomingCalls

        override this.CallHierarchyOutgoingCalls(p) =
            p |> withReadOnlyContext CallHierarchy.outgoingCalls

        override this.TextDocumentPrepareTypeHierarchy(p) =
            p |> withReadOnlyContext TypeHierarchy.prepare

        override this.TypeHierarchySupertypes(p) =
            p |> withReadOnlyContext TypeHierarchy.supertypes

        override this.TypeHierarchySubtypes(p) =
            p |> withReadOnlyContext TypeHierarchy.subtypes

        override this.TextDocumentDeclaration(p) =
            p |> withReadOnlyContext Declaration.handle

        override this.WorkspaceDiagnostic(p) = notImplemented

        override this.WorkspaceSymbolResolve(p) =
            p |> withReadOnlyContext WorkspaceSymbol.resolve

        override this.TextDocumentDiagnostic(p) =
            p |> withReadOnlyContext Diagnostic.handle

        override this.TextDocumentLinkedEditingRange(p) =
            p |> withReadOnlyContext LinkedEditingRange.handle

        override this.TextDocumentMoniker(p) =
            p |> withReadOnlyContext Moniker.handle

        override this.CSharpMetadata(p) =
            p |> withReadOnlyContext CSharpMetadata.handle

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

    let startCore settings =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let serverCreator client =
            new CSharpLspServer(client, settings) :> ICSharpLspServer

        let clientCreator = CSharpLspClient

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
            logger.error (
                Log.setMessage "{name} crashed:"
                >> Log.addContext "name" (Process.GetCurrentProcess().ProcessName)
                >> Log.addException ex
            )
            3
