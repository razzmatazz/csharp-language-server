module CSharpLanguageServer.Tests.AnalyzerTests

open System
open System.IO
open System.Threading

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

// Client profile with pull diagnostics enabled (both textDocument and workspace)
// and analyzers explicitly turned on.
let private analyzerClientProfile =
    { defaultClientProfile with
        ServerConfig =
            { defaultClientProfile.ServerConfig with
                analyzersEnabled = Some true }
        ClientCapabilities =
            { defaultClientCapabilities with
                TextDocument =
                    Some
                        { defaultClientCapabilities.TextDocument.Value with
                            Diagnostic =
                                Some
                                    { DynamicRegistration = Some true
                                      RelatedDocumentSupport = None } }
                Workspace =
                    Some
                        { defaultClientCapabilities.Workspace.Value with
                            Diagnostics = Some { RefreshSupport = Some true } } } }

/// Pre-build the project so that obj/project.assets.json exists and MSBuildWorkspace
/// can resolve AnalyzerReferences from the project file.
let private prebuildProject (solutionDir: string) =
    let projectDir = Path.Combine(solutionDir, "Project")
    let exitCode, stdout, stderr = runDotnetBuild projectDir

    // Exit code 1 is acceptable here — it means warnings-as-errors fired (the IDE
    // diagnostics we want to test), but the restore + assets file are still produced.
    // We only fail on truly unexpected exit codes (e.g. restore failure).
    if exitCode <> 0 && exitCode <> 1 then
        failwithf "Pre-build of analyzer fixture failed (exit %d):\nstdout:\n%s\nstderr:\n%s" exitCode stdout stderr

[<Test>]
let testPullDiagnosticsIncludeEditorConfigAnalyzerRules () =
    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" analyzerClientProfile prebuildProject id

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
        let codes =
            report.Items
            |> Array.choose (fun d -> d.Code |> Option.map (fun c -> string c))
            |> Set.ofArray

        let codesStr = codes |> String.concat "; "
        ClassicAssert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040 in pull diagnostics, got: {codesStr}")
        ClassicAssert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051 in pull diagnostics, got: {codesStr}")
        ClassicAssert.IsTrue(codes.Contains("IDE0032"), $"Expected IDE0032 in pull diagnostics, got: {codesStr}")

    | _ -> failwith "U2.C1 (full report) was expected"


(*
[<Test>]
let testPushDiagnosticsIncludeEditorConfigAnalyzerRules () =
    // Use a push-diagnostics profile (no Diagnostic capability) with analyzers on.
    let pushAnalyzerProfile =
        { defaultClientProfile with
            ServerConfig =
                { defaultClientProfile.ServerConfig with
                    analyzersEnabled = Some true } }

    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" pushAnalyzerProfile prebuildProject id

    use classFile = client.Open("Project/Class.cs")

    // Poll until push diagnostics arrive rather than using a fixed sleep, so the
    // test passes as soon as the server publishes and only fails after the full budget.
    waitUntilOrTimeout
        (TimeSpan.FromSeconds(30.0))
        (fun () -> client.GetState().PushDiagnostics |> Map.containsKey classFile.Uri)
        "Expected push diagnostics for Project/Class.cs"

    let state = client.GetState()
    let _version, diagnosticList = state.PushDiagnostics[classFile.Uri]

    let codes =
        diagnosticList
        |> Array.choose (fun d -> d.Code |> Option.map (fun c -> string c))
        |> Set.ofArray

    let codesStr = codes |> String.concat "; "
    ClassicAssert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040 in push diagnostics, got: {codesStr}")
    ClassicAssert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051 in push diagnostics, got: {codesStr}")
*)

[<Test>]
let testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics () =
    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" analyzerClientProfile prebuildProject id

    // Open a file to ensure the solution is fully loaded and the workspace/configuration
    // SettingsChange (analyzersEnabled = true) has been applied before we fire
    // workspace/diagnostic. textDocument/didOpen is ReadWrite and blocks until it
    // completes, by which point ServerState.Config is settled.
    use _classFile = client.Open("Project/Class.cs")

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let report: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    match report with
    | Some report ->
        let allCodes =
            report.Items
            |> Array.collect (fun item ->
                match item with
                | U2.C1 fullReport ->
                    fullReport.Items
                    |> Array.choose (fun d -> d.Code |> Option.map (fun c -> string c))
                | _ -> [||])
            |> Set.ofArray

        ClassicAssert.IsTrue(
            allCodes.Contains("IDE0040"),
            $"Expected IDE0040 in workspace diagnostics, got: {allCodes}"
        )

    | _ -> failwith "'Some' was expected"

[<Test>]
let testWorkspaceDiagnosticResultIdChangesAfterDocumentEdit () =
    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" analyzerClientProfile prebuildProject id

    use classFile = client.Open("Project/Class.cs")
    let consumerUri = "Project/Consumer.cs" |> fileUriForProjectDir client.SolutionDir

    let initialParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let initialReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", initialParams)

    let initialFullReports =
        match initialReport with
        | Some report ->
            report.Items
            |> Array.choose (function
                | U2.C1 fullReport -> Some fullReport
                | U2.C2 _ -> None)
        | None -> failwith "workspace diagnostics were expected"

    let initialClassReport =
        initialFullReports |> Array.find (fun report -> report.Uri = classFile.Uri)

    let initialConsumerReport =
        initialFullReports |> Array.find (fun report -> report.Uri = consumerUri)

    let initialResultId = initialClassReport.ResultId |> Option.get

    Assert.That(initialClassReport.Items |> Array.choose _.Code |> Array.map string, Does.Contain("IDE0051"))
    Assert.That(initialConsumerReport.Items |> Array.choose _.Code |> Array.map string, Does.Contain("CS0122"))
    Assert.That(initialConsumerReport.ResultId, Is.EqualTo(Some initialResultId))

    classFile.Change(
        """public class MyClass
{
    public int Value { get; set; }
}
"""
    )

    let documentParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = Some initialResultId }

    let updatedDocumentReport: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", documentParams)

    let updatedDocumentResultId =
        match updatedDocumentReport with
        | Some(U2.C1 fullReport) -> fullReport.ResultId |> Option.get
        | _ -> failwith "updated document diagnostics were expected"

    Assert.That(updatedDocumentResultId, Is.Not.EqualTo(initialResultId))

    let updatedParams: WorkspaceDiagnosticParams =
        { initialParams with
            PreviousResultIds =
                [| { Uri = classFile.Uri
                     Value = updatedDocumentResultId }
                   { Uri = consumerUri
                     Value = initialResultId } |] }

    let updatedReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", updatedParams)

    match updatedReport with
    | Some report ->
        match
            report.Items
            |> Array.tryFind (function
                | U2.C1 fullReport -> fullReport.Uri = consumerUri
                | U2.C2 unchangedReport -> unchangedReport.Uri = consumerUri)
        with
        | Some(U2.C1 fullReport) ->
            Assert.That(fullReport.ResultId, Is.EqualTo(Some updatedDocumentResultId))
            Assert.That(fullReport.Items |> Array.choose _.Code |> Array.map string, Does.Not.Contain("CS0122"))
        | Some(U2.C2 _) -> failwith "Consumer.cs diagnostics must not be reported as unchanged"
        | None -> failwith "updated Consumer.cs diagnostics were expected"
    | None -> failwith "updated workspace diagnostics were expected"

[<Test>]
let testAnalyzerPipelineDoesNotCrashWhenNoAnalyzersPresent () =
    // Verify that the analyzer pipeline is robust when a project has no analyzer references
    // configured with diagnostic severity rules. Uses the genericProject fixture which has
    // no .editorconfig analyzer rules.
    use client =
        activateFixtureExt "genericProject" analyzerClientProfile emptyFixturePatch id

    use classFile = client.Open("Project/Class.cs")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    // Should get a full report (possibly with zero items) — not a crash
    match report with
    | Some(U2.C1 _) -> ()
    | _ -> failwith "U2.C1 (full report) was expected"
