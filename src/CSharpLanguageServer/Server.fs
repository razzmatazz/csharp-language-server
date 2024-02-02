module CSharpLanguageServer.Server

open System
open System.Text
open System.IO
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Classification
open Microsoft.Build.Locator
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.State
open CSharpLanguageServer.Lsp
open System.Threading.Tasks
open CSharpLanguageServer.Util

let getDotnetCliVersion () : string =
    use proc = new Process()
    proc.StartInfo <- ProcessStartInfo()
    proc.StartInfo.FileName <- "dotnet"
    proc.StartInfo.Arguments <- "--version"
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.CreateNoWindow <- true

    let startOK = proc.Start()
    if startOK then
        let sbuilder = StringBuilder()
        while (not (proc.StandardOutput.EndOfStream)) do
            sbuilder.Append(proc.StandardOutput.ReadLine()) |> ignore

        sbuilder.ToString()
    else
        "(could not launch `dotnet --version`)"

let setupServerHandlers settings (lspClient: LspClient) =
    let success = LspResult.success
    let mutable logMessageCurrent: AsyncLogFn = fun _ -> async { return() }
    let logMessageInvoke m = logMessageCurrent(m)

    let stateActor = MailboxProcessor.Start(
        serverEventLoop
            (fun m -> logMessageInvoke m)
            { emptyServerState with Settings = settings })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let diagnostics = MailboxProcessor.Start(
        fun inbox -> async {
            let rec loop state = async {
                try
                    let! msg = inbox.Receive()
                    let! (newState, eventsToPost) =
                        processDiagnosticsEvent
                            logMessageInvoke
                            // TODO: can we provide value for PublishDiagnosticsParams.Version?
                            (fun docUri diagnostics -> lspClient.TextDocumentPublishDiagnostics { Uri = docUri;
                                                                                                  Version = None;
                                                                                                  Diagnostics = diagnostics;
                                                                                                })
                            (getDocumentForUriFromCurrentState AnyDocument)
                            state
                            inbox.CurrentQueueLength
                            msg

                    for ev in eventsToPost do inbox.Post(ev)

                    return! loop newState
                with
                | ex ->
                    do! logMessageInvoke (sprintf "unhandled exception in `diagnostics`: %s" (ex|>string))
                    raise ex
            }

            return! loop emptyDiagnosticsState
        })

    let mutable timer: System.Threading.Timer option = None

    let setupTimer () =
        timer <- Some (new System.Threading.Timer(
            System.Threading.TimerCallback(
                fun _ -> do diagnostics.Post(ProcessPendingDiagnostics)
                         do stateActor.Post(PeriodicTimerTick)),
            null, dueTime=1000, period=250))

    let logMessageWithLevel l message = async {
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do! lspClient.WindowShowMessage messageParams
    }

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info

    let handleInitialize (scope: ServerRequestScope) (p: InitializeParams): AsyncLspResult<InitializeResult> = async {
      do! infoMessage (sprintf "initializing, csharp-ls version %s; cwd: \"%s\""
                               (typeof<CSharpLspClient>.Assembly.GetName().Version |> string)
                               (Directory.GetCurrentDirectory()))

      do! infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

      // do! infoMessage (sprintf "`dotnet --version`: %s"
      //                          (getDotnetCliVersion ()))

      let vsInstanceQueryOpt = VisualStudioInstanceQueryOptions.Default
      let vsInstanceList = MSBuildLocator.QueryVisualStudioInstances(vsInstanceQueryOpt)
      if Seq.isEmpty vsInstanceList then
         raise (InvalidOperationException("No instances of MSBuild could be detected." + Environment.NewLine + "Try calling RegisterInstance or RegisterMSBuildPath to manually register one."))

      // do! infoMessage "MSBuildLocator instances found:"
      //
      // for vsInstance in vsInstanceList do
      //     do! infoMessage (sprintf "- SDK=\"%s\", Version=%s, MSBuildPath=\"%s\", DiscoveryType=%s"
      //                              vsInstance.Name
      //                              (string vsInstance.Version)
      //                              vsInstance.MSBuildPath
      //                              (string vsInstance.DiscoveryType))

      let vsInstance = vsInstanceList |> Seq.head

      do! infoMessage (sprintf "MSBuildLocator: will register \"%s\", Version=%s as default instance"
                               vsInstance.Name
                               (string vsInstance.Version))

      MSBuildLocator.RegisterInstance(vsInstance)

      scope.Emit(ClientCapabilityChange p.Capabilities)

      let rootPath =
          p.RootUri
          |> Option.map (fun uri -> Uri.UnescapeDataString(Uri(uri.Replace("%3a", ":", true, null)).LocalPath))
          |> Option.orElse p.RootPath
          |> Option.defaultValue (Directory.GetCurrentDirectory())
      scope.Emit(RootPathChange rootPath)

      // setup timer so actors get period ticks
      setupTimer ()

      let initializeResult = {
              InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Handlers.Hover.provider p.Capabilities
                        RenameProvider = Handlers.Rename.provider p.Capabilities
                        DefinitionProvider = Handlers.Definition.provider p.Capabilities
                        TypeDefinitionProvider = Handlers.TypeDefinition.provider p.Capabilities
                        ImplementationProvider = Handlers.Implementation.provider p.Capabilities
                        ReferencesProvider = Handlers.References.provider p.Capabilities
                        DocumentHighlightProvider = Handlers.DocumentHighlight.provider p.Capabilities
                        DocumentSymbolProvider = Handlers.DocumentSymbol.provider p.Capabilities
                        WorkspaceSymbolProvider = Handlers.WorkspaceSymbol.provider p.Capabilities
                        DocumentFormattingProvider = Handlers.DocumentFormatting.provider p.Capabilities
                        DocumentRangeFormattingProvider = Handlers.DocumentRangeFormatting.provider p.Capabilities
                        DocumentOnTypeFormattingProvider = Handlers.DocumentOnTypeFormatting.provider p.Capabilities
                        SignatureHelpProvider = Handlers.SignatureHelp.provider p.Capabilities
                        CompletionProvider = Handlers.Completion.provider p.Capabilities
                        CodeLensProvider = Handlers.CodeLens.provider p.Capabilities
                        CodeActionProvider = Handlers.CodeAction.provider p.Capabilities
                        TextDocumentSync = Handlers.TextDocumentSync.provider p.Capabilities
                        FoldingRangeProvider = None
                        SelectionRangeProvider = None
                        SemanticTokensProvider = Handlers.SemanticTokens.provider p.Capabilities
                        InlayHintProvider = Handlers.InlayHint.provider p.Capabilities
                        TypeHierarchyProvider = Handlers.TypeHierarchy.provider p.Capabilities
                        CallHierarchyProvider = Handlers.CallHierarchy.provider p.Capabilities
                    }
              }

      return initializeResult |> success
    }

    let handleInitialized (scope: ServerRequestScope) (_p: InitializedParams): Async<LspResult<unit>> =
        async {
            // do! logMessage "handleInitialized: \"initialized\" notification received from client"

            //
            // registering w/client for didChangeWatchedFiles notifications"
            //
            let clientSupportsWorkspaceDidChangeWatchedFilesDynamicReg =
                scope.ClientCapabilities
                |> Option.bind (fun x -> x.Workspace)
                |> Option.bind (fun x -> x.DidChangeWatchedFiles)
                |> Option.bind (fun x -> x.DynamicRegistration)
                |> Option.defaultValue true

            match clientSupportsWorkspaceDidChangeWatchedFilesDynamicReg with
            | true ->
                let fileChangeWatcher = { GlobPattern = U2.First "**/*.{cs,csproj,sln}"
                                          Kind = None }

                let didChangeWatchedFilesRegistration: Types.Registration =
                    { Id = "id:workspace/didChangeWatchedFiles"
                      Method = "workspace/didChangeWatchedFiles"
                      RegisterOptions = { Watchers = [| fileChangeWatcher |] } |> serialize |> Some
                    }

                try
                    let! regResult =
                        lspClient.ClientRegisterCapability(
                            { Registrations = [| didChangeWatchedFilesRegistration |] })

                    match regResult with
                    | Ok _ -> ()
                    | Error error ->
                        do! infoMessage (sprintf "handleInitialized: didChangeWatchedFiles registration has failed with %s"
                                                 (error |> string))
                with
                | ex ->
                    do! infoMessage (sprintf "handleInitialized: didChangeWatchedFiles registration has failed with %s"
                                             (ex |> string))
            | false -> ()

            //
            // retrieve csharp settings
            //
            try
                let! workspaceCSharpConfig =
                    lspClient.WorkspaceConfiguration(
                        { items=[| { Section=Some "csharp"; ScopeUri=None } |] })

                let csharpConfigTokensMaybe =
                    match workspaceCSharpConfig with
                    | Ok ts -> Some ts
                    | _ -> None

                let newSettingsMaybe =
                  match csharpConfigTokensMaybe with
                  | Some [| t |] ->
                      let csharpSettingsMaybe = t |> deserialize<ServerSettingsCSharpDto option>

                      match csharpSettingsMaybe with
                      | Some csharpSettings ->

                          match csharpSettings.solution with
                          | Some solutionPath-> Some { scope.State.Settings with SolutionPath = Some solutionPath }
                          | _ -> None

                      | _ -> None
                  | _ -> None

                // do! logMessage (sprintf "handleInitialized: newSettingsMaybe=%s" (string newSettingsMaybe))

                match newSettingsMaybe with
                | Some newSettings ->
                    scope.Emit(SettingsChange newSettings)
                | _ -> ()
            with
            | ex ->
                do! infoMessage (sprintf "handleInitialized: could not retrieve `csharp` workspace configuration section: %s"
                                         (ex |> string))

            //
            // start loading the solution
            //
            stateActor.Post(SolutionReloadRequest (TimeSpan.FromMilliseconds(100)))

            // do! logMessage "handleInitialized: OK"

            return LspResult.Ok()
        }

    let requestHandlingWithScope requestType requestPriority nameAndAsyncFn =
        let requestName = nameAndAsyncFn |> fst
        let asyncFn = nameAndAsyncFn |> snd

        let requestHandler param =
            logMessageCurrent <- logMessage

            // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
            //
            // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
            // handler until previous one returns a Task (in our case -- F# `async` object.)

            let startRequest rc = StartRequest (requestName, requestType, requestPriority, rc)
            let requestId, semaphore = stateActor.PostAndReply(startRequest)

            let stateAcquisitionAndHandlerInvocation = async {
                do! semaphore.WaitAsync() |> Async.AwaitTask

                let! state = stateActor.PostAndAsyncReply(GetState)

                let scope = ServerRequestScope(requestId, state, stateActor.Post, logMessage)

                return! asyncFn scope param
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

        (requestName, requestHandler |> requestHandling)

    let requestHandlingWithReadOnlyScope nameAndAsyncFn =
        requestHandlingWithScope ReadOnly 0 nameAndAsyncFn

    let requestHandlingWithReadOnlyScopeWithPriority priority nameAndAsyncFn =
        requestHandlingWithScope ReadOnly priority nameAndAsyncFn

    let requestHandlingWithReadWriteScope nameAndAsyncFn =
        requestHandlingWithScope ReadWrite 0 nameAndAsyncFn

    [
        ("initialize"                        , handleInitialize)                           |> requestHandlingWithReadWriteScope
        ("initialized"                       , handleInitialized)                          |> requestHandlingWithReadWriteScope
        ("textDocument/didChange"            , Handlers.TextDocumentSync.didChange diagnostics.Post)
                                                                                           |> requestHandlingWithReadWriteScope
        ("textDocument/didClose"             , Handlers.TextDocumentSync.didClose diagnostics.Post)
                                                                                           |> requestHandlingWithReadWriteScope
        ("textDocument/didOpen"              , Handlers.TextDocumentSync.didOpen logMessage diagnostics.Post)
                                                                                           |> requestHandlingWithReadWriteScope
        ("textDocument/didSave"              , Handlers.TextDocumentSync.didSave logMessage diagnostics.Post)
                                                                                           |> requestHandlingWithReadWriteScope
        ("textDocument/codeAction"           , Handlers.CodeAction.handle logMessage)      |> requestHandlingWithReadOnlyScope
        ("codeAction/resolve"                , Handlers.CodeAction.resolve logMessage)     |> requestHandlingWithReadOnlyScope
        ("textDocument/codeLens"             , Handlers.CodeLens.handle)                   |> requestHandlingWithReadOnlyScopeWithPriority 99
        ("codeLens/resolve"                  , Handlers.CodeLens.resolve)                  |> requestHandlingWithReadOnlyScope
        ("textDocument/completion"           , Handlers.Completion.handle)                 |> requestHandlingWithReadOnlyScope
        ("textDocument/definition"           , Handlers.Definition.handle)                 |> requestHandlingWithReadOnlyScope
        ("textDocument/typeDefinition"       , Handlers.TypeDefinition.handle)             |> requestHandlingWithReadOnlyScope
        ("textDocument/documentHighlight"    , Handlers.DocumentHighlight.handle)          |> requestHandlingWithReadOnlyScope
        ("textDocument/documentSymbol"       , Handlers.DocumentSymbol.handle)             |> requestHandlingWithReadOnlyScope
        ("textDocument/hover"                , Handlers.Hover.handle)                      |> requestHandlingWithReadOnlyScope
        ("textDocument/implementation"       , Handlers.Implementation.handle)             |> requestHandlingWithReadOnlyScope
        ("textDocument/formatting"           , Handlers.DocumentFormatting.handle)         |> requestHandlingWithReadOnlyScope
        ("textDocument/onTypeFormatting"     , Handlers.DocumentOnTypeFormatting.handle)   |> requestHandlingWithReadOnlyScope
        ("textDocument/rangeFormatting"      , Handlers.DocumentRangeFormatting.handle)    |> requestHandlingWithReadOnlyScope
        ("textDocument/references"           , Handlers.References.handle)                 |> requestHandlingWithReadOnlyScope
        ("textDocument/prepareRename"        , Handlers.Rename.prepare getDocumentForUriFromCurrentState logMessage)
                                                                                           |> requestHandlingWithReadOnlyScope
        ("textDocument/rename"               , Handlers.Rename.handle)                     |> requestHandlingWithReadOnlyScope
        ("textDocument/signatureHelp"        , Handlers.SignatureHelp.handle)              |> requestHandlingWithReadOnlyScope
        ("textDocument/semanticTokens/full"  , Handlers.SemanticTokens.handleFull)         |> requestHandlingWithReadOnlyScope
        ("textDocument/semanticTokens/range" , Handlers.SemanticTokens.handleRange)        |> requestHandlingWithReadOnlyScope
        ("textDocument/inlayHint"            , Handlers.InlayHint.handle)                  |> requestHandlingWithReadOnlyScope
        ("textDocument/prepareTypeHierarchy" , Handlers.TypeHierarchy.prepare)             |> requestHandlingWithReadOnlyScope
        ("typeHierarchy/supertypes"          , Handlers.TypeHierarchy.handleSupertypes)    |> requestHandlingWithReadOnlyScope
        ("typeHierarchy/subtypes"            , Handlers.TypeHierarchy.handleSubtypes)      |> requestHandlingWithReadOnlyScope
        ("textDocument/prepareCallHierarchy" , Handlers.CallHierarchy.prepare)             |> requestHandlingWithReadOnlyScope
        ("callHierarchy/incomingCalls"       , Handlers.CallHierarchy.handleIncomingCalls) |> requestHandlingWithReadOnlyScope
        ("callHierarchy/outgoingCalls"       , Handlers.CallHierarchy.handleOutgoingCalls) |> requestHandlingWithReadOnlyScope
        ("workspace/symbol"                  , Handlers.WorkspaceSymbol.handle)            |> requestHandlingWithReadOnlyScope
        ("workspace/didChangeWatchedFiles"   , Handlers.Workspace.didChangeWatchedFiles logMessage diagnostics.Post)
                                                                                           |> requestHandlingWithReadWriteScope
        ("workspace/didChangeConfiguration"  , Handlers.Workspace.didChangeConfiguration)  |> requestHandlingWithReadWriteScope
        ("csharp/metadata"                   , Handlers.CSharpMetadata.handle)             |> requestHandlingWithReadOnlyScope
    ]
    |> Map.ofList

let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    Ionide.LanguageServerProtocol.Server.startWithSetup
        (setupServerHandlers options)
        input
        output
        CSharpLspClient
        defaultRpc

let start options =
    try
        let result = startCore options
        int result
    with
    | _ex ->
        // logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
