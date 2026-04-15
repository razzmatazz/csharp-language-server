module CSharpLanguageServer.Tests.DiagnosticTests

open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

// This test documents a race: workspace/diagnostic is called while the solution is still
// loading (simulated via a 3-second SolutionLoadDelay). The server should return no items
// in that window, but ideally it should block or return a retriable error instead — so the
// assertion below is intentionally wrong (expects 1 document) to make the test fail and
// highlight the bug.
[<Test>]
let testWorkspaceDiagnosticsWhileSolutionIsLoading () =
    let profile =
        { defaultClientProfile with
            SolutionLoadDelay = Some 3000 }

    use client = activateFixtureExt "testDiagnosticsWork" profile emptyFixturePatch id

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    // Fire workspace/diagnostic while the solution is still loading.
    let report: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    // The server returns 0 items because the solution is not yet loaded — this assertion
    // intentionally fails to expose the race condition.
    match report with
    | Some report -> Assert.AreEqual(1, report.Items.Length)
    | None -> Assert.Fail("Expected Some WorkspaceDiagnosticReport but got None")

[<Test>]
[<Retry(3)>]
let testPushDiagnosticsWork () =
    use client = activateFixture "testDiagnosticsWork"

    // open Class.cs file and wait for diagnostics to be pushed
    use classFile = client.Open("Project/Class.cs")

    Thread.Sleep(8000)

    let state = client.GetState()
    let diag0 = state.PushDiagnostics |> Map.tryFind classFile.Uri
    Assert.IsTrue(diag0.IsSome)

    let version0, diagnosticList0 = diag0.Value
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
    classFile.Change("")

    Thread.Sleep(4000)

    let state = client.GetState()
    let version1, diagnosticList1 = state.PushDiagnostics |> Map.find classFile.Uri

    Assert.AreEqual(None, version1)

    Assert.AreEqual(0, diagnosticList1.Length)

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
    classFile.Change("")

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

    Thread.Sleep(250) // TODO: work around race for Razor support

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
        Assert.AreEqual(1, report0.Items.Length)

        match report0.Items[0] with
        | U2.C1 fullReport ->
            Assert.AreEqual("full", fullReport.Kind)
            Assert.IsTrue(fullReport.ResultId.IsSome)
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

    // The final response body contains unchanged-kind stubs (uri+resultId) for each
    // document that was streamed via $/progress, so the client can build previousResultIds.
    // The actual diagnostic content was delivered via $/progress, not here.
    match report0 with
    | Some report0 ->
        Assert.AreEqual(1, report0.Items.Length)

        match report0.Items[0] with
        | U2.C2 stub -> Assert.AreEqual("unchanged", stub.Kind)
        | U2.C1 _ -> failwith "expected unchanged stub in final response body, got full report"
    | _ -> failwith "'Some' was expected"

    let progress = client.GetProgressParams partialResultToken
    Assert.AreEqual(1, progress.Length)

    let report0 = progress[0].Value |> deserialize<WorkspaceDiagnosticReport>
    Assert.AreEqual(1, report0.Items.Length)

    match report0.Items[0] with
    | U2.C1 fullReport ->
        Assert.AreEqual("full", fullReport.Kind)
        Assert.IsTrue(fullReport.ResultId.IsSome)
        Assert.AreEqual(3, fullReport.Items.Length)

        let diagnostic0 = fullReport.Items[0]
        Assert.AreEqual(true, diagnostic0.Code.IsSome)
        Assert.AreEqual("Identifier expected", diagnostic0.Message)

    | _ -> failwith "'U2.C1' was expected"

[<Test>]
let testWorkspaceDiagnosticsStreamingReturnUnchangedOnSecondPoll () =
    // Regression: when the client sends previousResultIds that it collected from
    // $/progress notifications of a previous streaming poll, the server must respond
    // with WorkspaceUnchangedDocumentDiagnosticReport for all unchanged documents.
    //
    // The bug: the streaming path returned { items: [] } as the final response body,
    // so the client had no resultIds to send back and every subsequent poll triggered
    // a full re-scan (busy loop visible in csharp-ls-rpc.log).
    //
    // The fix: the final response body must echo back at least the uri+resultId of each
    // document that was streamed, so the client can populate previousResultIds.
    use client = activateFixture "testDiagnosticsWork"

    // First streaming poll — collect resultIds from $/progress notifications
    let token1: ProgressToken = System.Guid.NewGuid() |> string |> U2.C2

    let firstParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = Some token1
          Identifier = None
          PreviousResultIds = Array.empty }

    let firstResponse: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", firstParams)

    // Collect all document items from $/progress for token1
    let progressItems1 =
        client.GetProgressParams token1
        |> List.collect (fun pp ->
            // The first notification uses WorkspaceDiagnosticReport,
            // subsequent ones use WorkspaceDiagnosticReportPartialResult;
            // both have an `items` array at the top level.
            let items = pp.Value["items"] |> Option.ofObj

            match items with
            | Some items -> items |> deserialize<WorkspaceDocumentDiagnosticReport[]> |> List.ofArray
            | None -> [])

    Assert.IsTrue(progressItems1.Length > 0, "expected $/progress items from first streaming poll")

    // The final response body must now contain the resultId stubs so the client
    // can build previousResultIds — this is the assertion that fails before the fix.
    let previousResultIds =
        match firstResponse with
        | Some report ->
            report.Items
            |> Array.choose (fun item ->
                match item with
                | U2.C1 full -> full.ResultId |> Option.map (fun rid -> { Uri = full.Uri; Value = rid })
                | U2.C2 unchanged ->
                    Some
                        { Uri = unchanged.Uri
                          Value = unchanged.ResultId })
        | None ->
            Assert.Fail("expected Some from first workspace/diagnostic")
            Array.empty

    Assert.IsTrue(
        previousResultIds.Length > 0,
        "expected the final response body to contain resultId stubs for each streamed document, \
         so the client can send previousResultIds on the next poll (busy-loop fix)"
    )

    // Second streaming poll — send back the resultIds we collected
    let token2: ProgressToken = System.Guid.NewGuid() |> string |> U2.C2

    let secondParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = Some token2
          Identifier = None
          PreviousResultIds = previousResultIds }

    let _secondResponse: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", secondParams)

    // All $/progress items for the second poll must be Unchanged
    let progressItems2 =
        client.GetProgressParams token2
        |> List.collect (fun pp ->
            let items = pp.Value["items"] |> Option.ofObj

            match items with
            | Some items -> items |> deserialize<WorkspaceDocumentDiagnosticReport[]> |> List.ofArray
            | None -> [])

    Assert.IsTrue(progressItems2.Length > 0, "expected $/progress items from second streaming poll")

    for item in progressItems2 do
        match item with
        | U2.C2 unchanged -> Assert.AreEqual("unchanged", unchanged.Kind)
        | U2.C1 full ->
            Assert.Fail(
                sprintf
                    "expected Unchanged on second streaming poll but got full report for %s — \
                     server is not honouring previousResultIds in the streaming path"
                    full.Uri
            )

[<Test>]
let testWorkspaceDiagnosticsReturnResultId () =
    // Fix 1: the server must populate resultId on full reports so that VS Code
    // can send previousResultIds on subsequent polls and receive Unchanged responses.
    use client = activateFixture "testDiagnosticsWork"

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let report: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    match report with
    | Some report ->
        Assert.IsTrue(report.Items.Length > 0, "expected at least one document in workspace diagnostic report")

        for item in report.Items do
            match item with
            | U2.C1 fullReport ->
                Assert.IsTrue(
                    fullReport.ResultId.IsSome,
                    sprintf "expected resultId to be populated for uri %s" fullReport.Uri
                )
            | U2.C2 _ ->
                // Unchanged is not valid on the first poll (no previousResultIds were sent)
                Assert.Fail("expected full report on first poll, got Unchanged")
    | None -> Assert.Fail("expected Some WorkspaceDiagnosticReport")

[<Test>]
let testWorkspaceDiagnosticsReturnFullWhenCacheWarmButClientHasNoResultIds () =
    // Regression: when the diagnostics cache is warm (from a previous poll) but the
    // client sends empty previousResultIds (e.g. fresh client, or client discarded its
    // state), the server must return full reports — not unchanged.  The cache-hit path
    // must consult knownResultIds before deciding to emit unchanged.
    use client = activateFixture "testDiagnosticsWork"

    // First poll — warms the server-side diagnostics cache
    let warmupParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let warmupReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", warmupParams)

    // Sanity: first poll should succeed with full reports
    match warmupReport with
    | Some report -> Assert.IsTrue(report.Items.Length > 0, "expected items from first poll to warm the cache")
    | None -> Assert.Fail("expected first workspace/diagnostic to succeed")

    // Second poll — deliberately empty previousResultIds, simulating a client that
    // does not hold any result IDs (even though the server cache is now warm)
    let secondParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let secondReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", secondParams)

    match secondReport with
    | Some report ->
        Assert.IsTrue(report.Items.Length > 0, "expected items from second poll")

        for item in report.Items do
            match item with
            | U2.C1 fullReport ->
                Assert.AreEqual("full", fullReport.Kind)
                Assert.IsTrue(fullReport.ResultId.IsSome, sprintf "expected resultId for %s" fullReport.Uri)
                // The client asked for everything (no previousResultIds), so the full
                // report must contain actual diagnostic items, not be empty
                Assert.IsTrue(fullReport.Items.Length > 0, sprintf "expected diagnostics for %s" fullReport.Uri)
            | U2.C2 unchangedReport ->
                Assert.Fail(
                    sprintf
                        "expected full report for %s but got Unchanged — server should not return Unchanged when client sent empty previousResultIds"
                        unchangedReport.Uri
                )
    | None -> Assert.Fail("expected Some WorkspaceDiagnosticReport on second poll")

[<Test>]
let testWorkspaceDiagnosticsReturnUnchangedOnSecondPoll () =
    // Fix 1: a second poll that sends back the resultIds from the first poll should
    // receive WorkspaceUnchangedDocumentDiagnosticReport (U2.C2) for every document
    // that has not changed.
    use client = activateFixture "testDiagnosticsWork"

    let firstParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let firstReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", firstParams)

    let previousResultIds =
        match firstReport with
        | Some report ->
            report.Items
            |> Array.choose (fun item ->
                match item with
                | U2.C1 full -> full.ResultId |> Option.map (fun rid -> { Uri = full.Uri; Value = rid })
                | U2.C2 _ -> None)
        | None ->
            Assert.Fail("expected first workspace/diagnostic to succeed")
            Array.empty

    Assert.IsTrue(previousResultIds.Length > 0, "expected resultIds from first poll")

    let secondParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = previousResultIds }

    let secondReport: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", secondParams)

    match secondReport with
    | Some report ->
        for item in report.Items do
            match item with
            | U2.C2 unchangedReport -> Assert.AreEqual("unchanged", unchangedReport.Kind)
            | U2.C1 fullReport ->
                Assert.Fail(sprintf "expected Unchanged on second poll but got full report for %s" fullReport.Uri)
    | None -> Assert.Fail("expected Some WorkspaceDiagnosticReport on second poll")
