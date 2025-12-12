module CSharpLanguageServer.Tests.InitializationTests

open NUnit.Framework

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

let assertHoverWorks (client: ClientController) file pos expectedMarkupContent =
    use classFile = client.Open(file)

    let hover0Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = pos
          WorkDoneToken = None }

    let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

    match hover0 with
    | Some { Contents = U3.C1 markupContent
             Range = None } ->
        Assert.AreEqual(MarkupKind.Markdown, markupContent.Kind)
        Assert.AreEqual(expectedMarkupContent, markupContent.Value)

    | x -> failwithf "'{ Contents = U3.C1 markupContent; Range = None }' was expected but '%s' received" (string x)

[<Test>]
let testServerRegistersCapabilitiesWithTheClient () =
    use client = activateFixture "genericProject"

    let serverInfo = client.GetState().ServerInfo.Value
    Assert.AreEqual("csharp-ls", serverInfo.Name)

    let serverCaps = client.GetState().ServerCapabilities.Value

    Assert.AreEqual(
        { Change = Some TextDocumentSyncKind.Incremental
          OpenClose = Some true
          Save = Some(U2.C2 { IncludeText = Some true })
          WillSave = None
          WillSaveWaitUntil = None }
        |> U2<TextDocumentSyncOptions, TextDocumentSyncKind>.C1
        |> Some,
        serverCaps.TextDocumentSync
    )

    Assert.AreEqual(
        { WorkspaceFolders =
            Some
                { Supported = Some true
                  ChangeNotifications = None }
          FileOperations = None }
        |> Some,
        serverCaps.Workspace
    )

    Assert.AreEqual(true |> U2<bool, HoverOptions>.C1 |> Some, serverCaps.HoverProvider)

    Assert.AreEqual(
        true
        |> U3<bool, ImplementationOptions, ImplementationRegistrationOptions>.C1
        |> Some,
        serverCaps.ImplementationProvider
    )

    Assert.AreEqual(true |> U2<bool, DocumentSymbolOptions>.C1 |> Some, serverCaps.DocumentSymbolProvider)

    Assert.AreEqual(true |> U2<bool, DefinitionOptions>.C1 |> Some, serverCaps.DefinitionProvider)

    Assert.AreEqual(null, serverCaps.InlineValueProvider)

    Assert.AreEqual(
        { DocumentSelector =
            Some
                [| U2.C1
                       { Language = Some "csharp"
                         Scheme = Some "file"
                         Pattern = Some "**/*.cs" } |]
          WorkDoneProgress = None
          Identifier = None
          InterFileDependencies = false
          WorkspaceDiagnostics = true
          Id = None }
        |> U2<DiagnosticOptions, DiagnosticRegistrationOptions>.C2
        |> Some,
        serverCaps.DiagnosticProvider
    )

    Assert.AreEqual(true |> U2<bool, DocumentHighlightOptions>.C1 |> Some, serverCaps.DocumentHighlightProvider)

    Assert.AreEqual(
        { WorkDoneProgress = None
          TriggerCharacters = Some [| "."; "'" |]
          AllCommitCharacters = None
          ResolveProvider = Some true
          CompletionItem = None }
        |> Some,
        serverCaps.CompletionProvider
    )

    Assert.AreEqual(
        { WorkDoneProgress = None
          CodeActionKinds = None
          ResolveProvider = Some true }
        |> U2<bool, CodeActionOptions>.C2
        |> Some,
        serverCaps.CodeActionProvider
    )

    Assert.AreEqual(true |> U2<bool, RenameOptions>.C1 |> Some, serverCaps.RenameProvider)

    Assert.AreEqual(null, serverCaps.DeclarationProvider)

    Assert.AreEqual(true |> U2<bool, DocumentFormattingOptions>.C1 |> Some, serverCaps.DocumentFormattingProvider)

    Assert.AreEqual(true |> U2<bool, ReferenceOptions>.C1 |> Some, serverCaps.ReferencesProvider)

    Assert.AreEqual(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some, serverCaps.WorkspaceSymbolProvider)

    Assert.AreEqual(
        { WorkDoneProgress = None
          TriggerCharacters = Some [| "("; ","; "<"; "{"; "[" |]
          RetriggerCharacters = None }
        |> Some,
        serverCaps.SignatureHelpProvider
    )

    Assert.AreEqual(null, serverCaps.MonikerProvider)

    Assert.IsTrue(client.ServerDidRespondTo "initialize")
    Assert.IsTrue(client.ServerDidRespondTo "initialized")

[<Test>]
let testSlnxSolutionFileWillBeFoundAndLoaded () =
    use client = activateFixture "projectWithSlnx"

    Assert.IsTrue(client.ServerMessageLogContains(fun m -> m.Contains "1 solution(s) found"))

    Assert.IsTrue(client.ServerDidRespondTo "initialize")
    Assert.IsTrue(client.ServerDidRespondTo "initialized")

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.MethodA(string arg)\n```"

[<Test>]
let testMultiTargetProjectLoads () =
    use client = activateFixture "multiTargetProject"

    Assert.IsTrue(client.ServerMessageLogContains(fun m -> m.Contains "loading project"))

    assertHoverWorks
        client
        "Project/Class.cs"
        { Line = 2u; Character = 16u }
        "```csharp\nvoid Class.Method(string arg)\n```"

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

    Assert.IsTrue(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder0"))
    )

    Assert.IsTrue(
        client.ServerProgressLogContains(fun str ->
            str.Contains("Finished loading workspace folder") && str.Contains("/folder1"))
    )

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
                Assert.AreEqual(MarkupKind.Markdown, c.Kind)
                Assert.AreEqual(sprintf "```csharp\n%s\n```" expectedMethodName, c.Value.ReplaceLineEndings("\n"))
            | _ -> failwith "C1 was expected"

            Assert.IsTrue(hover.Range.IsNone)

        | _ -> failwith "Some (U3.C1 c) was expected"

    testHoverOnClass "folder0/Project/Class.cs" "void Class0.Method(string arg)"
    testHoverOnClass "folder1/Project/Class.cs" "void Class1.Method(string arg)"
