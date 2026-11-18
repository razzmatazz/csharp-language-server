module CSharpLanguageServer.Tests.RestoreTests

open System.IO

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

// Projects that were never restored, or were restored by an SDK other than the one now
// evaluating them, load in MSBuildWorkspace without failing: their design-time build just
// silently drops every package reference, or every framework reference (the SDK disables
// its missing-targeting-pack error for design-time builds); e.g. a net10.0 project
// restored with SDK 10 loses all of System once SDK 11 is installed.

let private documentErrorCodes (client: LspTestClient) (filename: string) : string list =
    use file = client.Open(filename)

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = file.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report with
    | Some(U2.C1 report) ->
        report.Items
        |> Array.filter (fun d -> d.Severity = Some DiagnosticSeverity.Error)
        |> Array.map (fun d ->
            match d.Code with
            | Some(U2.C2 code) -> code
            | Some(U2.C1 code) -> string code
            | None -> d.Message)
        |> List.ofArray
    | _ -> failwith "U2.C1 (full report) was expected"

let private assetsFilePath solutionDir =
    Path.Combine(solutionDir, "Project", "obj", "project.assets.json")

let private assertNotRestored (solutionDir: string) =
    Assert.That(
        File.Exists(assetsFilePath solutionDir),
        Is.False,
        "precondition: projectNotRestored must have no project.assets.json before the server loads it"
    )

// projectRestoredWithoutTargetingPack restores into its own packages folder; see the
// fixture's Project.csproj for how SimulateOlderSdkRestore leaves this pack out.
let private targetingPackPath solutionDir =
    Path.Combine(solutionDir, "packages", "microsoft.netcore.app.ref", "10.0.2")

let private restoreWithoutTargetingPack (solutionDir: string) =
    let exitCode, stdout, stderr =
        runDotnetRestore "-p:SimulateOlderSdkRestore=true" (Path.Combine(solutionDir, "Project"))

    if exitCode <> 0 then
        failwithf "Pre-restore failed (exit %d):\nstdout:\n%s\nstderr:\n%s" exitCode stdout stderr

    Assert.That(
        File.Exists(assetsFilePath solutionDir),
        Is.True,
        "precondition: the pre-restore must have produced project.assets.json"
    )

    Assert.That(
        Directory.Exists(targetingPackPath solutionDir),
        Is.False,
        "precondition: the pre-restore must not have downloaded the targeting pack"
    )

[<Test>]
let testUnrestoredProjectIsRestoredOnLoad () =
    use client =
        activateFixtureExt "projectNotRestored" defaultClientProfile assertNotRestored id

    let errors = documentErrorCodes client "Project/Class.cs"

    Assert.That(errors, Is.Empty, "expected no errors once the project has been restored")
    Assert.That(File.Exists(assetsFilePath client.SolutionDir), Is.True, "expected the server to have restored")

[<Test>]
let testProjectMissingItsTargetingPackIsRestoredOnLoad () =
    use client =
        activateFixtureExt "projectRestoredWithoutTargetingPack" defaultClientProfile restoreWithoutTargetingPack id

    let errors = documentErrorCodes client "Project/Class.cs"

    Assert.That(errors, Is.Empty, "expected no errors once the targeting pack has been restored")

    Assert.That(
        Directory.Exists(targetingPackPath client.SolutionDir),
        Is.True,
        "expected the server's restore to have downloaded the targeting pack"
    )
