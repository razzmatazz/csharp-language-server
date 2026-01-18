module CSharpLanguageServer.Tests.DiagnosticTests

open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testPushDiagnosticsWork () =
    use client = activateFixture "testDiagnosticsWork"

    //
    // open Class.cs file and wait for diagnostics to be pushed
    //
    use classFile = client.Open("Project/Class.cs")

    Thread.Sleep(4000)

    let state = client.GetState()
    let version0, diagnosticList0 = state.PushDiagnostics |> Map.find classFile.Uri

    Assert.AreEqual(None, version0)

    Assert.AreEqual(3, diagnosticList0.Length)

    let diagnostic0 = diagnosticList0.[0]
    Assert.AreEqual("Identifier expected", diagnostic0.Message)
    Assert.AreEqual(Some DiagnosticSeverity.Error, diagnostic0.Severity)
    Assert.AreEqual(0, diagnostic0.Range.Start.Line)
    Assert.AreEqual(3, diagnostic0.Range.Start.Character)

    let diagnostic1 = diagnosticList0.[1]
    Assert.AreEqual("; expected", diagnostic1.Message)

    let diagnostic2 = diagnosticList0.[2]

    Assert.AreEqual(
        "The type or namespace name 'XXX' could not be found (are you missing a using directive or an assembly reference?)",
        diagnostic2.Message
    )

    //
    // now change the file to contain no content (and thus no diagnostics)
    //
    classFile.DidChange("")

    Thread.Sleep(4000)

    let state = client.GetState()
    let version1, diagnosticList1 = state.PushDiagnostics |> Map.find classFile.Uri

    Assert.AreEqual(None, version1)

    Assert.AreEqual(0, diagnosticList1.Length)
    ()

[<Test>]
let testPullDiagnosticsWork () =
    use client = activateFixture "testDiagnosticsWork"
    use classFile = client.Open("Project/Class.cs")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report0: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report0 with
    | Some(U2.C1 report) ->
        Assert.AreEqual("full", report.Kind)
        Assert.AreEqual(None, report.ResultId)
        Assert.AreEqual(3, report.Items.Length)

        let diagnostic0 = report.Items.[0]
        Assert.AreEqual(0, diagnostic0.Range.Start.Line)
        Assert.AreEqual(3, diagnostic0.Range.Start.Character)
        Assert.AreEqual(Some DiagnosticSeverity.Error, diagnostic0.Severity)
        Assert.AreEqual("Identifier expected", diagnostic0.Message)

        Assert.AreEqual(
            "https://msdn.microsoft.com/query/roslyn.query?appId=roslyn&k=k(CS1001)",
            diagnostic0.CodeDescription.Value.Href
        )

        let diagnostic1 = report.Items.[1]
        Assert.AreEqual("; expected", diagnostic1.Message)

        let diagnostic2 = report.Items.[2]

        Assert.AreEqual(
            "The type or namespace name 'XXX' could not be found (are you missing a using directive or an assembly reference?)",
            diagnostic2.Message
        )
    | _ -> failwith "U2.C1 is expected"

    //
    // now try to do the same but with file fixed to contain no content (and thus no diagnostics)
    //
    classFile.DidChange("")

    let report1: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report1 with
    | Some(U2.C1 report) ->
        Assert.AreEqual("full", report.Kind)
        Assert.AreEqual(None, report.ResultId)
        Assert.AreEqual(0, report.Items.Length)
    | _ -> failwith "U2.C1 is expected"


[<Test>]
let testPullDiagnosticsWorkForRazorFiles () =
    use client = activateFixture "aspnetProject"
    use cshtmlFile = client.Open("Project/Views/Test/ViewFileWithErrors.cshtml")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = cshtmlFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report0: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report0 with
    | Some(U2.C1 report) ->
        Assert.AreEqual("full", report.Kind)
        Assert.AreEqual(None, report.ResultId)
        Assert.AreEqual(1, report.Items.Length)

        let reportItems = report.Items |> Array.sortBy _.Range

        let diagnostic0 = reportItems[0]
        Assert.AreEqual(1, diagnostic0.Range.Start.Line)
        Assert.AreEqual(7, diagnostic0.Range.Start.Character)
        Assert.AreEqual(Some DiagnosticSeverity.Error, diagnostic0.Severity)
        Assert.IsTrue(diagnostic0.Message.StartsWith("'IndexViewModel' does not contain a definition for 'XXX' and no"))

    | _ -> failwith "U2.C1 was expected"

[<Test>]
let testWorkspaceDiagnosticsWork () =
    use client = activateFixture "testDiagnosticsWork"

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let report0: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    match report0 with
    | Some report0 ->
        Assert.AreEqual(3, report0.Items.Length)

        match report0.Items[0] with
        | U2.C1 fullReport ->
            Assert.AreEqual("full", fullReport.Kind)
            Assert.AreEqual(None, fullReport.ResultId)
            Assert.AreEqual(3, fullReport.Items.Length)

            let diagnostic0 = fullReport.Items[0]
            Assert.AreEqual(true, diagnostic0.Code.IsSome)
            Assert.AreEqual("Identifier expected", diagnostic0.Message)

        | _ -> failwith "'U2.C1' was expected"

    | _ -> failwith "'Some' was expected"

[<Test>]
let testWorkspaceDiagnosticsWorkWithStreaming () =
    use client = activateFixture "testDiagnosticsWork"

    Thread.Sleep(1000)

    let partialResultToken: ProgressToken = System.Guid.NewGuid() |> string |> U2.C2

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = Some partialResultToken
          Identifier = None
          PreviousResultIds = Array.empty }

    let report0: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    // report should have 0 results, all of them streamed to lsp client via $/progress instead
    match report0 with
    | Some report0 -> Assert.AreEqual(0, report0.Items.Length)
    | _ -> failwith "'Some' was expected"

    let progress = client.GetProgressParams partialResultToken
    Assert.AreEqual(3, progress.Length)

    let report0 = progress[0].Value |> deserialize<WorkspaceDiagnosticReport>
    Assert.AreEqual(1, report0.Items.Length)

    match report0.Items[0] with
    | U2.C1 fullReport ->
        Assert.AreEqual("full", fullReport.Kind)
        Assert.AreEqual(None, fullReport.ResultId)
        Assert.AreEqual(3, fullReport.Items.Length)

        let diagnostic0 = fullReport.Items[0]
        Assert.AreEqual(true, diagnostic0.Code.IsSome)
        Assert.AreEqual("Identifier expected", diagnostic0.Message)

    | _ -> failwith "'U2.C1' was expected"

    let report1 =
        progress[1].Value |> deserialize<WorkspaceDiagnosticReportPartialResult>

    Assert.AreEqual(1, report1.Items.Length)
