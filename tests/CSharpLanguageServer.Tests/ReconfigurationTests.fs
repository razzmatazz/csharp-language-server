module CSharpLanguageServer.Tests.ReconfigurationTests

open System

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
[<Retry(3)>]
let testReadyToReconfiguringToConfiguredPhaseTransition () =
    // Supply solutionPathOverride via workspace/configuration (ServerConfig) rather
    // than --solution CLI arg.  The InitializedGate ensures handleInitialized's
    // ConfigurationChange is folded in before the first solution load starts, so
    // this now correctly selects SolutionA for the initial load.
    let profile =
        { defaultClientProfile with
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    solutionPathOverride = Some "SolutionA.sln" } }

    use client = activateFixtureExt "twoSolutions" profile emptyFixturePatch id

    let projectAUri = "ProjectA/SomeClass.cs" |> fileUriForProjectDir client.SolutionDir
    let projectBUri = "ProjectB/SomeClass.cs" |> fileUriForProjectDir client.SolutionDir

    // Load SolutionA and wait for Ready
    use _docA = client.Open "ProjectA/SomeClass.cs"

    waitUntilOrTimeout
        (TimeSpan.FromSeconds 30.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Ready")
        "workspace never reached Ready (SolutionA)"

    // SolutionA has a "hello_A" diagnostic
    let diagsA0 = getWorkspaceDiagnosticsForUri client projectAUri
    ClassicAssert.IsTrue(diagsA0 |> List.exists (fun d -> d.Message.Contains "hello_A"))

    // Switch to SolutionB
    client.Notify(
        "workspace/didChangeConfiguration",
        {| settings = {| csharp = {| solutionPathOverride = "SolutionB.sln" |} |} |}
    )

    let folderChangeEvent: DidChangeWorkspaceFoldersParams =
        { Event =
            { Added = Array.empty
              Removed = Array.empty } }

    client.Notify("workspace/didChangeWorkspaceFolders", folderChangeEvent)

    // Load SolutionB and wait for Ready
    use _docB = client.Open "ProjectB/SomeClass.cs"

    waitUntilOrTimeout
        (TimeSpan.FromSeconds 60.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Ready")
        "workspace never reached Ready (SolutionB)"

    // SolutionB has a "42_B" diagnostic; ProjectA has none
    let diagsB = getWorkspaceDiagnosticsForUri client projectBUri
    ClassicAssert.IsTrue(diagsB |> List.exists (fun d -> d.Message.Contains "42_B"))

    let diagsA1 = getWorkspaceDiagnosticsForUri client projectAUri
    ClassicAssert.AreEqual(0, diagsA1.Length)

[<Test>]
[<Retry(3)>]
let testDidChangeConfigurationAloneTriggersSolutionReload () =
    // Regression test: workspace/didChangeConfiguration with a changed
    // solutionPathOverride must reload the solution WITHOUT requiring a
    // subsequent workspace/didChangeWorkspaceFolders notification.
    let profile =
        { defaultClientProfile with
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    solutionPathOverride = Some "SolutionA.sln" } }

    use client = activateFixtureExt "twoSolutions" profile emptyFixturePatch id

    let projectAUri = "ProjectA/SomeClass.cs" |> fileUriForProjectDir client.SolutionDir
    let projectBUri = "ProjectB/SomeClass.cs" |> fileUriForProjectDir client.SolutionDir

    // Load SolutionA and wait for Ready
    use _docA = client.Open "ProjectA/SomeClass.cs"

    waitUntilOrTimeout
        (TimeSpan.FromSeconds 30.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Ready")
        "workspace never reached Ready (SolutionA)"

    // SolutionA has a "hello_A" diagnostic
    let diagsA0 = getWorkspaceDiagnosticsForUri client projectAUri
    ClassicAssert.IsTrue(diagsA0 |> List.exists (fun d -> d.Message.Contains "hello_A"))

    // Switch to SolutionB via didChangeConfiguration ONLY — no didChangeWorkspaceFolders
    client.Notify(
        "workspace/didChangeConfiguration",
        {| settings = {| csharp = {| solutionPathOverride = "SolutionB.sln" |} |} |}
    )

    // Load SolutionB and wait for Ready
    use _docB = client.Open "ProjectB/SomeClass.cs"

    waitUntilOrTimeout
        (TimeSpan.FromSeconds 60.0)
        (fun () -> client.GetDebugInfo().workspace.phase = "Ready")
        "workspace never reached Ready (SolutionB) after didChangeConfiguration alone"

    // SolutionB has a "42_B" diagnostic; ProjectA has none
    let diagsB = getWorkspaceDiagnosticsForUri client projectBUri
    ClassicAssert.IsTrue(diagsB |> List.exists (fun d -> d.Message.Contains "42_B"))

    let diagsA1 = getWorkspaceDiagnosticsForUri client projectAUri
    ClassicAssert.AreEqual(0, diagsA1.Length)

[<Test>]
let testRemovingFinalWorkspaceFolderLeavesResponsiveConfiguredWorkspace () =
    use client = activateFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    let rootFolder: WorkspaceFolder =
        { Name = "root"
          Uri = client.SolutionDir |> Uri |> string }

    let folderChangeEvent: DidChangeWorkspaceFoldersParams =
        { Event =
            { Added = Array.empty
              Removed = [| rootFolder |] } }

    client.Notify("workspace/didChangeWorkspaceFolders", folderChangeEvent)

    let hoverParams: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let _: Hover option = client.Request("textDocument/hover", hoverParams)

    let workspace = client.GetDebugInfo().workspace
    ClassicAssert.AreEqual("Configured", workspace.phase)
    ClassicAssert.IsTrue(workspace.folders.IsEmpty)
