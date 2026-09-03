module CSharpLanguageServer.Tests.InitializationTests

open System

open NUnit.Framework

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures
open CSharpLanguageServer.Types

let assertHoverWorks (client: LspTestClient) file pos (expectedMarkupContent: string) =
    use classFile = client.Open(file)

    let hover0Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = pos
          WorkDoneToken = None }

    let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

    match hover0 with
    | Some { Contents = U3.C1 markupContent
             Range = None } ->
        Assert.That(markupContent.Kind, Is.EqualTo(MarkupKind.Markdown))
        Assert.That(markupContent.Value, Is.EqualTo(expectedMarkupContent))

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
    Assert.That(serverInfo.Name, Is.EqualTo("csharp-ls"))

    let serverCaps = client.GetState().ServerCapabilities.Value

    Assert.That(
        serverCaps.TextDocumentSync,
        Is.EqualTo(
            { Change = Some TextDocumentSyncKind.Incremental
              OpenClose = Some true
              Save = Some(U2.C2 { IncludeText = Some true })
              WillSave = None
              WillSaveWaitUntil = None }
            |> U2<TextDocumentSyncOptions, TextDocumentSyncKind>.C1
            |> Some
        )
    )

    Assert.That(
        serverCaps.Workspace,
        Is.EqualTo(
            { WorkspaceFolders =
                Some
                    { Supported = Some true
                      ChangeNotifications = U2.C2 true |> Some }
              FileOperations = None }
            |> Some
        )
    )

    Assert.That(serverCaps.HoverProvider, Is.EqualTo(true |> U2<bool, HoverOptions>.C1 |> Some))

    Assert.That(
        serverCaps.ImplementationProvider,
        Is.EqualTo(
            true
            |> U3<bool, ImplementationOptions, ImplementationRegistrationOptions>.C1
            |> Some
        )
    )

    Assert.That(serverCaps.DocumentSymbolProvider, Is.EqualTo(true |> U2<bool, DocumentSymbolOptions>.C1 |> Some))

    Assert.That(serverCaps.DefinitionProvider, Is.EqualTo(true |> U2<bool, DefinitionOptions>.C1 |> Some))

    Assert.That(serverCaps.InlineValueProvider, Is.EqualTo(null))

    let expectedDocumentSelector =
        [| U2.C1
               { Language = Some "csharp"
                 Scheme = Some "file"
                 Pattern = Some "**/*.cs" }
           U2.C1
               { Language = Some "razor"
                 Scheme = Some "file"
                 Pattern = Some "**/*.cshtml" } |]

    Assert.That(
        serverCaps.DiagnosticProvider,
        Is.EqualTo(
            { DocumentSelector = Some expectedDocumentSelector
              WorkDoneProgress = None
              Identifier = None
              InterFileDependencies = false
              WorkspaceDiagnostics = true
              Id = None }
            |> U2<DiagnosticOptions, DiagnosticRegistrationOptions>.C2
            |> Some
        )
    )

    Assert.That(serverCaps.DocumentHighlightProvider, Is.EqualTo(true |> U2<bool, DocumentHighlightOptions>.C1 |> Some))

    Assert.That(
        serverCaps.CompletionProvider,
        Is.EqualTo(
            { WorkDoneProgress = None
              TriggerCharacters = Some [| "."; "'" |]
              AllCommitCharacters = None
              ResolveProvider = Some true
              CompletionItem = None }
            |> Some
        )
    )

    Assert.That(
        serverCaps.CodeActionProvider,
        Is.EqualTo(
            { WorkDoneProgress = None
              CodeActionKinds = None
              ResolveProvider = Some true }
            |> U2<bool, CodeActionOptions>.C2
            |> Some
        )
    )

    Assert.That(serverCaps.RenameProvider, Is.EqualTo(true |> U2<bool, RenameOptions>.C1 |> Some))

    Assert.That(serverCaps.DeclarationProvider, Is.EqualTo(null))

    Assert.That(
        serverCaps.DocumentFormattingProvider,
        Is.EqualTo(true |> U2<bool, DocumentFormattingOptions>.C1 |> Some)
    )

    Assert.That(serverCaps.ReferencesProvider, Is.EqualTo(true |> U2<bool, ReferenceOptions>.C1 |> Some))

    Assert.That(serverCaps.WorkspaceSymbolProvider, Is.EqualTo(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some))

    Assert.That(
        serverCaps.SignatureHelpProvider,
        Is.EqualTo(
            { WorkDoneProgress = None
              TriggerCharacters = Some [| "("; ","; "<"; "{"; "[" |]
              RetriggerCharacters = None }
            |> Some
        )
    )

    Assert.That(serverCaps.MonikerProvider, Is.EqualTo(null))

    Assert.That(
        serverCaps.FoldingRangeProvider,
        Is.EqualTo(
            true
            |> U3<bool, FoldingRangeOptions, FoldingRangeRegistrationOptions>.C1
            |> Some
        )
    )

    Assert.That(client.ServerDidRespondTo "initialize", Is.True)
    Assert.That(client.ClientDidSendNotification "initialized", Is.True)

[<Test>]
let testSlnxSolutionFileWillBeFoundAndLoaded () =
    use client = activateFixture "projectWithSlnx"

    Assert.That(client.ServerDidRespondTo "initialize", Is.True)
    Assert.That(client.ClientDidSendNotification "initialized", Is.True)

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.MethodA(string arg)\n```"

    Assert.That(client.ServerMessageLogContains(fun m -> m.Contains "1 solution(s) found"), Is.True)

[<Test>]
let testMultiTargetProjectLoads () =
    use client = activateFixture "multiTargetProject"

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.Method(string arg)\n```"

    Assert.That(client.ServerMessageLogContains(fun m -> m.Contains "loading project"), Is.True)

/// Unlike the fixture above, this solution has no TFM common to every project, so the
/// workspace-global TargetFramework property (issues #75 / #198) does not engage and the
/// multi-targeted project genuinely loads as one Roslyn project per TFM flavor.  Document
/// lookup used to read those flavors as ambiguity and answer null to every position-based
/// request on the project.  The hovered method only exists under NET8_0, so this also
/// verifies the best-TFM flavor (net8.0, not net6.0) is the one answering.
[<Test>]
let testMultiTargetProjectWithNoCommonTfmAnswersFromBestFlavor () =
    use client = activateFixture "multiTargetProjectNoCommonTfm"

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 3u; Character = 16u }
        "```csharp\nvoid Class.Method(string arg)\n```"

/// Regression test for https://github.com/razzmatazz/csharp-language-server/issues/405:
/// a solution mixing a plain net10.0 project with a net10.0-windows one used to have the
/// platform-specific TFM applied as a workspace-global MSBuild property, overriding the
/// plain project's own TargetFramework.  Its project.assets.json has no net10.0-windows
/// target, so the design-time build failed with NETSDK1005, dropped every reference, and
/// the server reported phantom errors on code that compiles cleanly.
[<Test>]
let testPlatformSpecificTfmDoesNotBreakSiblingProjects () =
    // The restore is what makes this test meaningful: it produces a project.assets.json
    // for Project containing a net10.0 target and nothing else.
    let prebuild (solutionDir: string) =
        let exitCode, stdout, stderr = runDotnetBuild solutionDir

        if exitCode <> 0 then
            failwithf
                "Pre-build of platformSpecificTfm fixture failed (exit %d):\nstdout:\n%s\nstderr:\n%s"
                exitCode
                stdout
                stderr

    use client =
        activateFixtureExt "platformSpecificTfm" defaultClientProfile prebuild id

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 5u; Character = 17u }
        "```csharp\nvoid Class.MethodA(string arg)\n```"

    use classFile = client.Open("Project/Class.cs")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report with
    | Some(U2.C1 report) ->
        let errors =
            report.Items
            |> Array.filter (fun d -> d.Severity = Some DiagnosticSeverity.Error)

        let errorsStr = errors |> Array.map _.Message |> String.concat "; "

        Assert.That(errors.Length, Is.EqualTo(0), $"expected no errors on Project/Class.cs, got: {errorsStr}")
    | _ -> failwith "U2.C1 (full report) was expected"

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
    Assert.That(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder0")),
        Is.True
    )

    Assert.That(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder1")),
        Is.True
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
                Assert.That(c.Kind, Is.EqualTo(MarkupKind.Markdown))

                Assert.That(
                    c.Value.ReplaceLineEndings("\n"),
                    Is.EqualTo(sprintf "```csharp\n%s\n```" expectedMethodName)
                )
            | _ -> failwith "C1 was expected"

            Assert.That(hover.Range.IsNone, Is.True)

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

    Assert.That(
        client.ServerDidInvoke "client/registerCapability",
        Is.False,
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

    Assert.That(diagnosticOptions.DocumentSelector.Value, Is.EqualTo(box expectedDocumentSelector))

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

    Assert.That(
        actualSupported,
        Is.EqualTo(configurationSupported),
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

    Assert.That(
        client.ServerDidInvoke "workspace/configuration",
        Is.EqualTo(configurationSupported),
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
    Assert.That(debugInfoReady.workspace.phase, Is.EqualTo("Ready"), "phase after solution load")

    // ── ShuttingDown ─────────────────────────────────────────────────────────────
    client.SendShutdown()

    let debugInfoShuttingDown = client.GetDebugInfo()
    Assert.That(debugInfoShuttingDown.workspace.phase, Is.EqualTo("ShuttingDown"), "phase after shutdown")

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

    Assert.That(client.ServerDidRespondTo "initialize", Is.True)
    Assert.That(client.ClientDidSendNotification "initialized", Is.True)

/// Every report-kind `$/progress` value the server sent so far, as (message, percentage).
let loadProgressReports (client: LspTestClient) =
    client.GetRpcLog()
    |> Seq.filter (fun m ->
        m.Source = Server
        && (Some m.Message |> jeStringProp "method") = Some "$/progress")
    |> Seq.map (fun m -> Some m.Message |> indexJE "params" |> indexJE "value")
    |> Seq.filter (fun v -> (v |> jeStringProp "kind") = Some "report")
    |> Seq.map (fun v ->
        (v |> jeStringProp "message" |> Option.defaultValue ""),
        (v |> indexJE "percentage" |> Option.map _.GetUInt32()))
    |> List.ofSeq

[<Test>]
let testSolutionLoadReportsPerProjectProgress () =
    use client = activateFixture "nestedProjects"
    use scratchDocument = client.Open("App/Tests/Scratch.cs")

    // any position request blocks until the workspace load completes,
    // so once it answers the full load progress is in the rpc log
    let hoverParams: HoverParams =
        { TextDocument = { Uri = scratchDocument.Uri }
          Position = { Line = 0u; Character = 17u }
          WorkDoneToken = None }

    let _hover: Hover option = client.Request("textDocument/hover", hoverParams)

    let reports = loadProgressReports client

    // the fixture solution has two projects, so the load reports two
    // per-project completions with rising percentages
    let projectReports = reports |> List.filter (fun (msg, _) -> msg.EndsWith "/2)")

    Assert.That(List.length projectReports, Is.EqualTo(2), sprintf "all report-kind progress seen: %A" reports)

    let percentages = projectReports |> List.choose snd

    Assert.That((percentages = List.sort percentages), Is.True, "percentages should be monotonic")
    Assert.That(percentages |> List.forall (fun p -> p <= 100u), Is.True)
    Assert.That(List.last percentages, Is.EqualTo(100u))

/// A .slnf solution filter selects a subset of its parent solution.  The declared
/// project set used to be read from the parent, so on a filtered solution the load
/// progress counted every project of the parent (reports like "(74/358)" whose
/// percentage never reached 100) and the workspace-global TFM intersection ran over
/// the excluded projects too.
[<Test>]
let testSolutionLoadProgressCountsOnlySolutionFilterProjects () =
    let profile =
        { defaultClientProfile with
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    solutionPathOverride = Some "Partial.slnf" } }

    use client = activateFixtureExt "solutionWithFilter" profile emptyFixturePatch id
    use classDocument = client.Open("ProjectA/Class.cs")

    let hoverParams: HoverParams =
        { TextDocument = { Uri = classDocument.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None }

    let _hover: Hover option = client.Request("textDocument/hover", hoverParams)

    let reports = loadProgressReports client

    // Full.sln declares three projects; Partial.slnf keeps two of them
    let projectReports = reports |> List.filter (fun (msg, _) -> msg.EndsWith "/2)")

    Assert.That(List.length projectReports, Is.EqualTo(2), sprintf "all report-kind progress seen: %A" reports)

    Assert.That(
        reports |> List.exists (fun (msg, _) -> msg.EndsWith "/3)"),
        Is.False,
        "the filter's parent solution must not set the progress denominator"
    )

    Assert.That(List.last (projectReports |> List.choose snd), Is.EqualTo(100u))

[<Test>]
let testSolutionLoadProgressStaysWithinBoundsWithOutOfSolutionReferences () =
    // The solution lists one project, but it references another project that
    // is not a solution member; LoadMetadataForReferencedProjects makes the
    // workspace load that one too, so more projects load than the solution
    // file declares. Progress must still stay monotonic and within 100%.
    use client = activateFixture "solutionWithOutOfSlnReference"
    use programDocument = client.Open("App/Program.cs")

    let hoverParams: HoverParams =
        { TextDocument = { Uri = programDocument.Uri }
          Position = { Line = 4u; Character = 26u }
          WorkDoneToken = None }

    let _hover: Hover option = client.Request("textDocument/hover", hoverParams)

    let reports = loadProgressReports client

    let projectReports = reports |> List.filter (fun (msg, _) -> msg.Contains "/")

    Assert.That(List.length projectReports, Is.GreaterThanOrEqualTo(2), sprintf "reports seen: %A" reports)

    let percentages = projectReports |> List.choose snd
    Assert.That((percentages = List.sort percentages), Is.True, "percentages should be monotonic")

    Assert.That(
        percentages |> List.forall (fun p -> p <= 100u),
        Is.True,
        sprintf "percentages should never exceed 100, got %A" percentages
    )
