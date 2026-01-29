module CSharpLanguageServer.Tests.DocumentSyncTests

open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testDidCloseNotificationWillRevertFileToStateOnDisk () =
    use client = activateFixture "genericProject"

    let mutable diagnosticParams: option<DocumentDiagnosticParams> = None

    do
        use classFile = client.Open("Project/Class.cs")

        diagnosticParams <-
            { WorkDoneToken = None
              PartialResultToken = None
              TextDocument = { Uri = classFile.Uri }
              Identifier = None
              PreviousResultId = None }
            |> Some

    // update the file (and NOT save it to disk) -- then send the "textDocument/didClose"
    // notification (classFile.Dispose() will do that for us)
    do
        use classFile = client.Open("Project/Class.cs")

        let report0: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report0 with
        | Some(U2.C1 report) ->
            Assert.AreEqual("full", report.Kind)
            Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

        // now change file to contain "xxx" to trigger diagnostics
        classFile.Change("xxx")

        let report1: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report1 with
        | Some(U2.C1 report) -> Assert.AreEqual(3, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

    // test the file has been reverted on the in-memory solution by pulling
    // the diagnostics for the file and validating there are no errors
    do
        let report2: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report2 with
        | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

    // ok, now open the file again and do save the file to disk this time
    do
        use classFile = client.Open("Project/Class.cs")

        let report3: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report3 with
        | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

        // now change file to contain "xxx" to trigger diagnostics
        classFile.Change("xxx")

        let report4: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report4 with
        | Some(U2.C1 report) -> Assert.AreEqual(3, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

        classFile.Save()

        let report5: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report5 with
        | Some(U2.C1 report) -> Assert.AreEqual(3, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

    do
        let report6: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report6 with
        | Some(U2.C1 report) -> Assert.AreEqual(3, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

[<Test>]
let testDidCloseNotificationWillRevertCshtmlFileToStateOnDisk () =
    use client = activateFixture "aspnetProject"

    let mutable diagnosticParams: option<DocumentDiagnosticParams> = None

    do
        use cshtmlFile = client.Open("Project/Views/Test/Index.cshtml")

        diagnosticParams <-
            { WorkDoneToken = None
              PartialResultToken = None
              TextDocument = { Uri = cshtmlFile.Uri }
              Identifier = None
              PreviousResultId = None }
            |> Some

    // update the file (and NOT save it to disk) -- then send the "textDocument/didClose"
    // notification (cshtmlFile.Dispose() will do that for us)
    do
        use cshtmlFile = client.Open("Project/Views/Test/Index.cshtml")

        let report0: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report0 with
        | Some(U2.C1 report) ->
            Assert.AreEqual("full", report.Kind)
            Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

        // now change file to contain invalid razor code to trigger diagnostics
        cshtmlFile.Change("@model Project.Models.Test.IndexViewModel\n@Model.InvalidProperty")

        let report1: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report1 with
        | Some(U2.C1 report) -> Assert.GreaterOrEqual(report.Items.Length, 1, "Expected at least 1 diagnostic error")
        | _ -> failwith "U2.C1 is expected"

    // test the file has been reverted on the in-memory solution by pulling
    // the diagnostics for the file and validating there are no errors
    do
        let report2: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report2 with
        | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

    // ok, now open the file again and do save the file to disk this time
    do
        use cshtmlFile = client.Open("Project/Views/Test/Index.cshtml")

        let report3: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report3 with
        | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
        | _ -> failwith "U2.C1 is expected"

        // now change file to contain invalid razor code to trigger diagnostics
        cshtmlFile.Change("@model Project.Models.Test.IndexViewModel\n@Model.InvalidProperty")

        let report4: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report4 with
        | Some(U2.C1 report) -> Assert.GreaterOrEqual(report.Items.Length, 1, "Expected at least 1 diagnostic error")
        | _ -> failwith "U2.C1 is expected"

        cshtmlFile.Save()

        let report5: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report5 with
        | Some(U2.C1 report) ->
            Assert.GreaterOrEqual(report.Items.Length, 1, "Expected at least 1 diagnostic error after save")
        | _ -> failwith "U2.C1 is expected"

    do
        let report6: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", diagnosticParams)

        match report6 with
        | Some(U2.C1 report) ->
            Assert.GreaterOrEqual(report.Items.Length, 1, "Expected at least 1 diagnostic error after file close")
        | _ -> failwith "U2.C1 is expected"
