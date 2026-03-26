module CSharpLanguageServer.Lsp.Server

open System
open System.Diagnostics
open System.Threading.Tasks

open Ionide.LanguageServerProtocol.Mappings
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open StreamJsonRpc
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq

open CSharpLanguageServer.Types
open CSharpLanguageServer.Handlers
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Runtime.ServerStateLoop
open CSharpLanguageServer.Runtime.JsonRpcServer
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

let logger = Logging.getLoggerByName "Lsp.Server"

let getDynamicRegistrations (config: CSharpConfiguration) (clientCapabilities: ClientCapabilities) : Registration list =
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
    |> List.choose (fun regFunc -> regFunc config clientCapabilities)

let getServerCapabilities (config: CSharpConfiguration) (lspClient: InitializeParams) =
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
        DiagnosticProvider = Diagnostic.provider config lspClient.Capabilities
        WorkspaceSymbolProvider = WorkspaceSymbol.provider lspClient.Capabilities
        Workspace = Workspace.provider lspClient.Capabilities }

let configureRpcServer (stateActor: MailboxProcessor<ServerEvent>) (rpcServer: MailboxProcessor<JsonRpcServerEvent>) =
    let lspClient =
        new CSharpLspClient(sendJsonRpcNotification rpcServer, sendJsonRpcCall rpcServer)

    let wrapHandler (unwrapResult: LspResult<_> -> 'r) (requestMode: ServerRequestMode) fn jsonRpcCtx = async {
        let mutable requestCtx: ServerRequestContext option = None

        try
            let! serverRequestCtxObj =
                stateActor.PostAndAsyncReply(fun rc ->
                    EnterRequestContext(jsonRpcCtx.RequestOrdinal, jsonRpcCtx.MethodName, requestMode, rc))

            let ctx = serverRequestCtxObj :?> ServerRequestContext
            requestCtx <- Some ctx

            let fnParams =
                jsonRpcCtx.Params
                |> Option.defaultWith (fun _ -> Newtonsoft.Json.Linq.JValue.CreateNull())
                |> deserialize

            let! result = fn ctx fnParams

            return unwrapResult result
        finally
            let bufferedEvents =
                match requestCtx with
                | Some ctx -> ctx.GetBufferedEvents()
                | None -> []

            stateActor.Post(LeaveRequestContext(jsonRpcCtx.RequestOrdinal, bufferedEvents))
    }

    let callHandler requestMode fn : JsonRpcCallHandler =
        let serializeNullable value =
            if isNull (box value) then
                Newtonsoft.Json.Linq.JValue.CreateNull() :> Newtonsoft.Json.Linq.JToken
            else
                serialize value

        let unwrapResult result =
            match result with
            | Ok value -> value |> serializeNullable |> Ok
            | Error error -> error |> serialize |> Error

        wrapHandler unwrapResult requestMode fn

    let notificationHandler requestMode fn : JsonRpcNotificationHandler = wrapHandler ignore requestMode fn

    let callHandlers: Map<string, JsonRpcCallHandler> =
        Map.empty
        |> Map.add "initialize" (callHandler ReadWrite (LifeCycle.handleInitialize getServerCapabilities))
        |> Map.add "codeAction/resolve" (callHandler ReadOnly CodeAction.resolve)
        |> Map.add "csharp/metadata" (callHandler ReadOnly CSharpMetadata.handle)
        |> Map.add "textDocument/codeAction" (callHandler ReadOnly CodeAction.handle)
        |> Map.add "textDocument/completion" (callHandler ReadOnly Completion.handle)
        |> Map.add "completionItem/resolve" (callHandler ReadOnly Completion.resolve)
        |> Map.add "textDocument/definition" (callHandler ReadOnly Definition.handle)
        |> Map.add "textDocument/hover" (callHandler ReadOnly Hover.handle)
        |> Map.add "textDocument/references" (callHandler ReadOnly References.handle)
        |> Map.add "workspace/diagnostic" (callHandler ReadOnlyBackground Diagnostic.handleWorkspaceDiagnostic)
        |> Map.add "textDocument/diagnostic" (callHandler ReadOnly Diagnostic.handle)
        |> Map.add "textDocument/documentSymbol" (callHandler ReadOnly DocumentSymbol.handle)
        |> Map.add "textDocument/documentHighlight" (callHandler ReadOnly DocumentHighlight.handle)
        |> Map.add "workspace/symbol" (callHandler ReadOnly WorkspaceSymbol.handle)
        |> Map.add "textDocument/implementation" (callHandler ReadOnly Implementation.handle)
        |> Map.add "textDocument/prepareRename" (callHandler ReadOnly Rename.prepare)
        |> Map.add "textDocument/rename" (callHandler ReadOnly Rename.handle)
        |> Map.add "textDocument/formatting" (callHandler ReadOnly DocumentFormatting.handle)
        |> Map.add "textDocument/rangeFormatting" (callHandler ReadOnly DocumentRangeFormatting.handle)
        |> Map.add "textDocument/onTypeFormatting" (callHandler ReadOnly DocumentOnTypeFormatting.handle)
        |> Map.add "textDocument/typeDefinition" (callHandler ReadOnly TypeDefinition.handle)
        |> Map.add "textDocument/signatureHelp" (callHandler ReadOnly SignatureHelp.handle)
        |> Map.add "textDocument/semanticTokens/full" (callHandler ReadOnly SemanticTokens.handleFull)
        |> Map.add "textDocument/semanticTokens/full/delta" (callHandler ReadOnly SemanticTokens.handleFullDelta)
        |> Map.add "textDocument/semanticTokens/range" (callHandler ReadOnly SemanticTokens.handleRange)
        |> Map.add "textDocument/prepareCallHierarchy" (callHandler ReadOnly CallHierarchy.prepare)
        |> Map.add "callHierarchy/incomingCalls" (callHandler ReadOnly CallHierarchy.incomingCalls)
        |> Map.add "callHierarchy/outgoingCalls" (callHandler ReadOnly CallHierarchy.outgoingCalls)
        |> Map.add "textDocument/prepareTypeHierarchy" (callHandler ReadOnly TypeHierarchy.prepare)
        |> Map.add "typeHierarchy/supertypes" (callHandler ReadOnly TypeHierarchy.supertypes)
        |> Map.add "typeHierarchy/subtypes" (callHandler ReadOnly TypeHierarchy.subtypes)
        |> Map.add "shutdown" (callHandler ReadWrite LifeCycle.handleShutdown)
        |> Map.add "textDocument/codeLens" (callHandler ReadOnly CodeLens.handle)
        |> Map.add "codeLens/resolve" (callHandler ReadOnly CodeLens.resolve)
        |> Map.add "textDocument/inlayHint" (callHandler ReadOnly InlayHint.handle)

    let notificationHandlers: Map<string, JsonRpcNotificationHandler> =
        Map.empty
        |> Map.add
            "initialized"
            (notificationHandler ReadWrite (LifeCycle.handleInitialized lspClient stateActor getDynamicRegistrations))
        |> Map.add "textDocument/didOpen" (notificationHandler ReadWrite TextDocumentSync.didOpen)
        |> Map.add "textDocument/didChange" (notificationHandler ReadWrite TextDocumentSync.didChange)
        |> Map.add "textDocument/didClose" (notificationHandler ReadWrite TextDocumentSync.didClose)
        |> Map.add "textDocument/willSave" (notificationHandler ReadWrite TextDocumentSync.willSave)
        |> Map.add "textDocument/willSaveWaitUntil" (notificationHandler ReadWrite TextDocumentSync.willSaveWaitUntil)
        |> Map.add "textDocument/didSave" (notificationHandler ReadWrite TextDocumentSync.didSave)
        |> Map.add "workspace/didChangeWatchedFiles" (notificationHandler ReadWrite Workspace.didChangeWatchedFiles)
        |> Map.add
            "workspace/didChangeWorkspaceFolders"
            (notificationHandler ReadWrite Workspace.didChangeWorkspaceFolders)
        |> Map.add "workspace/didChangeConfiguration" (notificationHandler ReadWrite Workspace.didChangeConfiguration)
        |> Map.add "$/setTrace" (notificationHandler ReadWrite Trace.handleSetTrace)
        |> Map.add "exit" (notificationHandler ReadWrite LifeCycle.handleExit)

    callHandlers, notificationHandlers

let startCore
    (config: CSharpConfiguration)
    (initialWorkspace: LspWorkspace option)
    (rpcLogCallback: (RpcLogEntry -> unit) option)
    =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let stateActor =
        MailboxProcessor.Start(
            serverEventLoop
                { ServerState.Empty with
                    Config = config
                    Workspace = initialWorkspace |> Option.defaultValue LspWorkspace.Empty }
        )

    let rpcServer =
        startJsonRpcServer input output rpcLogCallback (fun rpcServer ->
            let lspClient =
                new CSharpLspClient(sendJsonRpcNotification rpcServer, sendJsonRpcCall rpcServer)

            stateActor.Post(ServerStarted lspClient)
            configureRpcServer stateActor rpcServer)

    rpcServer.PostAndAsyncReply(AwaitShutdown) |> Async.RunSynchronously

    0 // OK

let start (config: CSharpConfiguration) (initialWorkspace: LspWorkspace option) rpcLogCallback =
    try
        let result = startCore config initialWorkspace rpcLogCallback
        int result
    with ex ->
        logger.LogError("{name} crashed", ex, Process.GetCurrentProcess().ProcessName)
        3
