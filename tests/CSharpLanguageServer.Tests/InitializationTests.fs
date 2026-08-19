module CSharpLanguageServer.Tests.InitializationTests

open System

open NUnit.Framework
open NUnit.Framework.Legacy

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Types

let assertHoverWorks (client: LspTestClient) file pos expectedMarkupContent =
    use classFile = client.Open(file)

    let hover0Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = pos
          WorkDoneToken = None }

    let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

    match hover0 with
    | Some { Contents = U3.C1 markupContent
             Range = None } ->
        ClassicAssert.AreEqual(MarkupKind.Markdown, markupContent.Kind)
        ClassicAssert.AreEqual(expectedMarkupContent, markupContent.Value)

    | x -> failwithf "'{ Contents = U3.C1 markupContent; Range = None }' was expected but '%s' received" (string x)

[<Test>]
let testServerRegistersCapabilitiesWithTheClient () =
    use client = activateFixture "genericProject"

    // Phase reaches Configured after the initialized handler completes and the
    // InitializedGate fires.  Wait for it rather than asserting immediately, since
    // GetDebugInfo can race with the initialized handler still running.
    waitUntilOrTimeout
        (TimeSpan.FromSeconds 5.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Configured")
        "workspace never reached Configured after initialized"

    let serverInfo = client.GetState().ServerInfo.Value
    ClassicAssert.AreEqual("csharp-ls", serverInfo.Name)

    let serverCaps = client.GetState().ServerCapabilities.Value

    ClassicAssert.AreEqual(
        { Change = Some TextDocumentSyncKind.Incremental
          OpenClose = Some true
          Save = Some(U2.C2 { IncludeText = Some true })
          WillSave = None
          WillSaveWaitUntil = None }
        |> U2<TextDocumentSyncOptions, TextDocumentSyncKind>.C1
        |> Some,
        serverCaps.TextDocumentSync
    )

    ClassicAssert.AreEqual(
        { WorkspaceFolders =
            Some
                { Supported = Some true
                  ChangeNotifications = U2.C2 true |> Some }
          FileOperations = None }
        |> Some,
        serverCaps.Workspace
    )

    ClassicAssert.AreEqual(true |> U2<bool, HoverOptions>.C1 |> Some, serverCaps.HoverProvider)

    ClassicAssert.AreEqual(
        true
        |> U3<bool, ImplementationOptions, ImplementationRegistrationOptions>.C1
        |> Some,
        serverCaps.ImplementationProvider
    )

    ClassicAssert.AreEqual(true |> U2<bool, DocumentSymbolOptions>.C1 |> Some, serverCaps.DocumentSymbolProvider)

    ClassicAssert.AreEqual(true |> U2<bool, DefinitionOptions>.C1 |> Some, serverCaps.DefinitionProvider)

    ClassicAssert.AreEqual(null, serverCaps.InlineValueProvider)

    let expectedDocumentSelector =
        [| U2.C1
               { Language = Some "csharp"
                 Scheme = Some "file"
                 Pattern = Some "**/*.cs" }
           U2.C1
               { Language = Some "razor"
                 Scheme = Some "file"
                 Pattern = Some "**/*.cshtml" } |]

    ClassicAssert.AreEqual(
        { DocumentSelector = Some expectedDocumentSelector
          WorkDoneProgress = None
          Identifier = None
          InterFileDependencies = false
          WorkspaceDiagnostics = true
          Id = None }
        |> U2<DiagnosticOptions, DiagnosticRegistrationOptions>.C2
        |> Some,
        serverCaps.DiagnosticProvider
    )

    ClassicAssert.AreEqual(true |> U2<bool, DocumentHighlightOptions>.C1 |> Some, serverCaps.DocumentHighlightProvider)

    ClassicAssert.AreEqual(
        { WorkDoneProgress = None
          TriggerCharacters = Some [| "."; "'" |]
          AllCommitCharacters = None
          ResolveProvider = Some true
          CompletionItem = None }
        |> Some,
        serverCaps.CompletionProvider
    )

    ClassicAssert.AreEqual(
        { WorkDoneProgress = None
          CodeActionKinds = None
          ResolveProvider = Some true }
        |> U2<bool, CodeActionOptions>.C2
        |> Some,
        serverCaps.CodeActionProvider
    )

    ClassicAssert.AreEqual(true |> U2<bool, RenameOptions>.C1 |> Some, serverCaps.RenameProvider)

    ClassicAssert.AreEqual(null, serverCaps.DeclarationProvider)

    ClassicAssert.AreEqual(
        true |> U2<bool, DocumentFormattingOptions>.C1 |> Some,
        serverCaps.DocumentFormattingProvider
    )

    ClassicAssert.AreEqual(true |> U2<bool, ReferenceOptions>.C1 |> Some, serverCaps.ReferencesProvider)

    ClassicAssert.AreEqual(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some, serverCaps.WorkspaceSymbolProvider)

    ClassicAssert.AreEqual(
        { WorkDoneProgress = None
          TriggerCharacters = Some [| "("; ","; "<"; "{"; "[" |]
          RetriggerCharacters = None }
        |> Some,
        serverCaps.SignatureHelpProvider
    )

    ClassicAssert.AreEqual(null, serverCaps.MonikerProvider)

    ClassicAssert.AreEqual(
        true
        |> U3<bool, FoldingRangeOptions, FoldingRangeRegistrationOptions>.C1
        |> Some,
        serverCaps.FoldingRangeProvider
    )

    ClassicAssert.IsTrue(client.ServerDidRespondTo "initialize")
    ClassicAssert.IsTrue(client.ClientDidSendNotification "initialized")

[<Test>]
let testSlnxSolutionFileWillBeFoundAndLoaded () =
    use client = activateFixture "projectWithSlnx"

    ClassicAssert.IsTrue(client.ServerDidRespondTo "initialize")
    ClassicAssert.IsTrue(client.ClientDidSendNotification "initialized")

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.MethodA(string arg)\n```"

    ClassicAssert.IsTrue(client.ServerMessageLogContains(fun m -> m.Contains "1 solution(s) found"))

[<Test>]
let testMultiTargetProjectLoads () =
    use client = activateFixture "multiTargetProject"

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.Method(string arg)\n```"

    ClassicAssert.IsTrue(client.ServerMessageLogContains(fun m -> m.Contains "loading project"))

[<Test>]
let testMultiTargetWorkspace () =
    let clientWorkspaceCaps: WorkspaceClientCapabilities =
        { defaultClientCapabilities.Workspace.Value with
            WorkspaceFolders = Some true }

    let clientCaps: ClientCapabilities =
        { defaultClientCapabilities with
            Workspace = Some clientWorkspaceCaps }

    let updateInitializeParamsWithWorkspaceFolders (initParams: InitializeParams) : InitializeParams =
        let rootUri = initParams.RootUri.Value

        { initParams with
            WorkspaceFolders =
                Some
                    [| { Name = "folder0"
                         Uri = rootUri + "/folder0" }
                       { Name = "folder1"
                         Uri = rootUri + "/folder1" } |] }

    use client =
        activateFixtureExt
            "multiFolderWorkspace"
            { defaultClientProfile with
                ClientCapabilities = clientCaps }
            emptyFixturePatch
            updateInitializeParamsWithWorkspaceFolders

    (*
    ClassicAssert.IsTrue(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder0"))
    )

    ClassicAssert.IsTrue(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder1"))
    )
*)

    //
    // actually check multiple folders work by dispatching requests to several folders
    //
    let testHoverOnClass filename expectedMethodName =
        use classFile = client.Open(filename)

        //
        // check hover at method name
        //
        let hover0Params: HoverParams =
            { TextDocument = { Uri = classFile.Uri }
              Position = { Line = 2u; Character = 16u }
              WorkDoneToken = None }

        let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

        match hover0 with
        | Some hover ->
            match hover.Contents with
            | U3.C1 c ->
                ClassicAssert.AreEqual(MarkupKind.Markdown, c.Kind)

                ClassicAssert.AreEqual(
                    sprintf "```csharp\n%s\n```" expectedMethodName,
                    c.Value.ReplaceLineEndings("\n")
                )
            | _ -> failwith "C1 was expected"

            ClassicAssert.IsTrue(hover.Range.IsNone)

        | _ -> failwith "Some (U3.C1 c) was expected"

    testHoverOnClass "folder0/Project/Class.cs" "void Class0.Method(string arg)"
    testHoverOnClass "folder1/Project/Class.cs" "void Class1.Method(string arg)"

[<Test>]
let testClientRegisterCapabilityIsNotSentWhenNoDynamicRegistrationsAreRequested () =
    // When the client advertises no dynamicRegistration flags, getDynamicRegistrations
    // returns an empty list and handleInitialized must skip client/registerCapability
    // entirely rather than sending an empty Registrations array.
    use client =
        activateFixtureExt
            "genericProject"
            { defaultClientProfile with
                ClientCapabilities = emptyClientCapabilities }
            emptyFixturePatch
            id

    ClassicAssert.IsFalse(
        client.ServerDidInvoke "client/registerCapability",
        "server must not send client/registerCapability when all dynamicRegistration flags are absent"
    )

[<Test>]
let testDynamicRegistrationsUsePulledWorkspaceConfiguration () =
    let diagnosticCapability: DiagnosticClientCapabilities =
        { DynamicRegistration = Some true
          RelatedDocumentSupport = None }

    let clientCaps =
        { defaultClientCapabilities with
            TextDocument =
                Some
                    { defaultClientCapabilities.TextDocument.Value with
                        Diagnostic = Some diagnosticCapability } }

    let profile =
        { defaultClientProfile with
            ClientCapabilities = clientCaps
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    razorSupport = Some true }
            // The test harness enables Razor on the CLI by default. Override it so
            // the workspace/configuration response is the only source enabling it.
            ExtraArgs = [ "--features"; "\"\"" ] }

    use client = activateFixtureExt "genericProject" profile emptyFixturePatch id

    // Synchronize with the initialized notification before inspecting the RPC log.
    waitUntilOrTimeout
        (TimeSpan.FromSeconds 5.0)
        (fun () -> client.ServerDidInvoke "client/registerCapability")
        "server never sent client/registerCapability after initialized"

    let registrationParams =
        client.GetRpcLog()
        |> Seq.find (fun message ->
            message.Source = Server
            && (Some message.Message |> jeStringProp "method") = Some "client/registerCapability")
        |> _.Message
        |> Some
        |> indexJE "params"
        |> Option.get
        |> LSPAny.fromJsonElement
        |> deserialize<RegistrationParams>

    let diagnosticRegistration =
        registrationParams.Registrations
        |> Array.find (fun registration -> registration.Method = "textDocument/diagnostic")

    let diagnosticOptions =
        diagnosticRegistration.RegisterOptions.Value
        |> deserialize<DiagnosticRegistrationOptions>

    let expectedDocumentSelector: DocumentSelector =
        [| csharpDocumentFilter |> U2.C1; razorCsharpDocumentFilter |> U2.C1 |]

    ClassicAssert.AreEqual(expectedDocumentSelector, diagnosticOptions.DocumentSelector.Value)

[<TestCase(false, TestName = "absent: not sent")>]
[<TestCase(true, TestName = "Some true: sent")>]
let testWorkspaceConfigurationCapabilityGate (configurationSupported: bool) =
    // When Workspace.Configuration = Some true the server must send workspace/configuration.
    // When it is absent the server must skip the call entirely.
    //
    // Synchronize via Shutdown(): handleInitialized runs asynchronously after "initialized",
    // but the sequential scheduler ensures it completes before the server responds to "shutdown",
    // so the RPC log is stable by the time we inspect it.
    let caps =
        { emptyClientCapabilities with
            Workspace =
                if configurationSupported then
                    Some
                        { defaultClientCapabilities.Workspace.Value with
                            Configuration = Some true }
                else
                    None }

    // Precondition: the caps we built actually reflect the parameter.
    let actualFlag = caps.Workspace |> Option.bind _.Configuration
    let actualSupported = actualFlag = Some true

    ClassicAssert.AreEqual(
        configurationSupported,
        actualSupported,
        "test precondition failed: built ClientCapabilities does not match configurationSupported parameter"
    )

    use client =
        activateFixtureExt
            "genericProject"
            { defaultClientProfile with
                ClientCapabilities = caps }
            emptyFixturePatch
            id

    client.Shutdown()

    ClassicAssert.AreEqual(
        configurationSupported,
        client.ServerDidInvoke "workspace/configuration",
        sprintf
            "workspace/configuration invocation expected=%b when Workspace.Configuration=%A"
            configurationSupported
            actualFlag
    )

[<Test>]
let testWorkspacePhaseTransitionConfiguredLoadingReadyShuttingDown () =
    // Drives the full Configured → Loading → Ready → ShuttingDown chain in one pass.
    //
    // solutionLoadDelay holds the server in Loading long enough to assert on it
    // without a race (5 s — well above any scheduling jitter, well below the 15 s
    // request timeout used everywhere else).
    //
    // To observe ShuttingDown we send "shutdown" and "exit" as separate steps with
    // a GetDebugInfo call in between, rather than using client.Shutdown() which
    // sends both back-to-back.  "shutdown" is ReadWrite, so the server drains the
    // queue, calls workspaceTeardown (Phase → ShuttingDown), then replies — the
    // phase is already ShuttingDown by the time SendShutdown returns.
    let loadDelayMs = 5000

    let profileWithDelay =
        { defaultClientProfile with
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    debug =
                        Some
                            { debugMode = Some true
                              solutionLoadDelay = Some loadDelayMs } } }

    use client =
        activateFixtureExt "genericProject" profileWithDelay emptyFixturePatch id

    // ── Configured ──────────────────────────────────────────────────────────────
    // Phase reaches Configured after initialized completes and the InitializedGate
    // fires (including the workspace/configuration round-trip delivering
    // solutionLoadDelay).  Opening a document then advances it to Loading; with
    // solutionLoadDelay we can catch it in Configured first.
    waitUntilOrTimeout
        (TimeSpan.FromSeconds 5.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Configured")
        "workspace never reached Configured after initialized"

    // ── Loading ──────────────────────────────────────────────────────────────────
    // Opening a document is the trigger that kicks off solution loading
    // (textDocument/didOpen → LoadWorkspaceFolder → ProcessSolutionAwaiters →
    // workspaceLoadingStarted).  With solutionLoadDelay the load task sleeps before
    // touching Roslyn, so the workspace stays in Loading while we poll.
    use _doc = client.Open "Project/Class.cs"

    waitUntilOrTimeout
        (TimeSpan.FromSeconds 5.0)
        (fun () ->
            let info = client.GetDebugInfo()
            info.workspace.phase = "Loading")
        "workspace never reached Loading phase after textDocument/didOpen"

    // ── Ready ────────────────────────────────────────────────────────────────────
    // After solutionLoadDelay expires and the solution finishes loading,
    // the workspace phase should advance to Ready.  We wait up to 15 s —
    // well above the 5 s delay and any Roslyn startup overhead.
    waitUntilOrTimeout
        (TimeSpan.FromSeconds 15.0)
        (fun () ->
            let info = client.GetDebugInfo()
            info.workspace.phase = "Ready")
        "workspace never reached Ready phase after solution load completed"

    let debugInfoReady = client.GetDebugInfo()
    ClassicAssert.AreEqual("Ready", debugInfoReady.workspace.phase, "phase after solution load")

    // ── ShuttingDown ─────────────────────────────────────────────────────────────
    client.SendShutdown()

    let debugInfoShuttingDown = client.GetDebugInfo()
    ClassicAssert.AreEqual("ShuttingDown", debugInfoShuttingDown.workspace.phase, "phase after shutdown")

    client.SendExit()

[<Test>]
let testInitializeSucceedsWhenRootPathIsNotAValidUri () =
    // Some LSP clients (e.g. crush/powernap) send RootPath as a path like "/E:/proj2/fracture"
    // which is not a valid URI on Windows. Since RootUri is valid, initialization should succeed
    // regardless of what RootPath contains — the RootPath fallback should only be evaluated when
    // RootUri is None.
    //
    // We use "::invalid" which is not a valid URI on any platform, to reproduce the issue
    // cross-platform.
    let setInvalidRootPath (initParams: InitializeParams) : InitializeParams =
        { initParams with
            RootPath = Some "::invalid" }

    use client =
        activateFixtureExt "genericProject" defaultClientProfile emptyFixturePatch setInvalidRootPath

    ClassicAssert.IsTrue(client.ServerDidRespondTo "initialize")
    ClassicAssert.IsTrue(client.ClientDidSendNotification "initialized")
