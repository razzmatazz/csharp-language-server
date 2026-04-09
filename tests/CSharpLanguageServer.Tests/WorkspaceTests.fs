module CSharpLanguageServer.Tests.WorkspaceTests

open System
open System.IO
open System.Text
open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

/// Send a workspace/didChangeWatchedFiles notification for a single URI + change type.
let private notifyFileChanged (client: LspTestClient) (uri: string) (changeType: FileChangeType) =
    let p: DidChangeWatchedFilesParams =
        { Changes = [| { Uri = uri; Type = changeType } |] }

    client.Notify("workspace/didChangeWatchedFiles", p)

// ---------------------------------------------------------------------------
// .cs tests (genericProject fixture)
// ---------------------------------------------------------------------------

[<Test>]
let testDidChangeWatchedFilesChangedCsFileReloadsDocument () =
    use client = activateFixture "genericProject"

    let csFilePath = Path.Combine(client.SolutionDir, "Project", "Class.cs")
    let csFileUri = "Project/Class.cs" |> fileUriForProjectDir client.SolutionDir

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = csFileUri }
          Identifier = None
          PreviousResultId = None }

    // Baseline: no errors
    let report0: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report0 with
    | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
    | _ -> failwith "U2.C1 is expected"

    // Overwrite the file on disk with broken C# and notify the server
    File.WriteAllText(csFilePath, "xxx", Encoding.UTF8)
    notifyFileChanged client csFileUri FileChangeType.Changed

    let timeout = TimeSpan.FromSeconds(10L)

    waitUntilOrTimeout
        timeout
        (fun () ->
            let r: DocumentDiagnosticReport option =
                client.Request("textDocument/diagnostic", diagnosticParams)

            match r with
            | Some(U2.C1 report) -> report.Items.Length > 0
            | _ -> false)
        "Expected diagnostics to appear after .cs file was overwritten and Changed notification sent"

[<Test>]
let testDidChangeWatchedFilesDeletedCsFileRemovesDocument () =
    use client = activateFixture "genericProject"

    let csFilePath = Path.Combine(client.SolutionDir, "Project", "Class.cs")
    let csFileUri = "Project/Class.cs" |> fileUriForProjectDir client.SolutionDir

    let timeout = TimeSpan.FromSeconds(10L)

    // Inject errors into the file so the server emits diagnostics for it.
    // This is important for the post-delete assertion: if the document were
    // still registered after deletion we would see those diagnostics, so
    // "no diagnostics" unambiguously means the document was removed.
    File.WriteAllText(csFilePath, "xxx", Encoding.UTF8)
    notifyFileChanged client csFileUri FileChangeType.Changed

    // Wait until the server has actually processed the error so the
    // subsequent delete starts from a known state (diagnostics present).
    waitUntilOrTimeout
        timeout
        (fun () ->
            let items = csFileUri |> getWorkspaceDiagnosticsForUri client
            items.Length > 0)
        "Expected diagnostics to appear after .cs file was overwritten with errors"

    // Delete the file from disk and send Deleted notification.
    File.Delete(csFilePath)
    notifyFileChanged client csFileUri FileChangeType.Deleted

    // After deletion the document should be removed from the solution entirely,
    // so workspace/diagnostic should return no items for this URI.
    waitUntilOrTimeout
        timeout
        (fun () ->
            let items = csFileUri |> getWorkspaceDiagnosticsForUri client
            items.IsEmpty)
        "Expected no workspace diagnostics for deleted .cs file"

[<Test>]
let testDidChangeWatchedFilesCreatedCsFileAddsDocument () =
    use client = activateFixture "genericProject"

    let newFilePath = Path.Combine(client.SolutionDir, "Project", "BrokenNewClass.cs")

    let newFileUri =
        "Project/BrokenNewClass.cs" |> fileUriForProjectDir client.SolutionDir

    let timeout = TimeSpan.FromSeconds(10L)

    // Verify the new file is not yet in the solution
    let itemsBefore = newFileUri |> getWorkspaceDiagnosticsForUri client
    Assert.AreEqual(0, itemsBefore.Length)

    // Write a broken C# file to disk and send Created notification.
    // Using a file with errors means the server will emit a diagnostic entry,
    // giving us a reliable observable to poll.
    File.WriteAllText(newFilePath, "namespace Project { public class BrokenNewClass { xxx } }", Encoding.UTF8)

    notifyFileChanged client newFileUri FileChangeType.Created

    // The file should now be part of the solution and its compile error reported
    waitUntilOrTimeout
        timeout
        (fun () ->
            let items = newFileUri |> getWorkspaceDiagnosticsForUri client
            items.Length > 0)
        "Expected diagnostics for newly Created .cs file to appear in workspace/diagnostic"

// ---------------------------------------------------------------------------
// .cshtml tests (aspnetProject fixture)
// ---------------------------------------------------------------------------

[<Test>]
let testDidChangeWatchedFilesChangedCshtmlFileReloadsDocument () =
    use client = activateFixture "aspnetProject"

    let cshtmlPath =
        Path.Combine(client.SolutionDir, "Project", "Views", "Test", "Index.cshtml")

    let cshtmlUri =
        "Project/Views/Test/Index.cshtml" |> fileUriForProjectDir client.SolutionDir

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = cshtmlUri }
          Identifier = None
          PreviousResultId = None }

    // Baseline: no errors
    let report0: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report0 with
    | Some(U2.C1 report) -> Assert.AreEqual(0, report.Items.Length)
    | _ -> failwith "U2.C1 is expected"

    // Overwrite the .cshtml file with invalid Razor referencing a missing property
    File.WriteAllText(cshtmlPath, "@model Project.Models.Test.IndexViewModel\n@Model.InvalidProperty", Encoding.UTF8)

    notifyFileChanged client cshtmlUri FileChangeType.Changed

    let timeout = TimeSpan.FromSeconds(10L)

    waitUntilOrTimeout
        timeout
        (fun () ->
            let r: DocumentDiagnosticReport option =
                client.Request("textDocument/diagnostic", diagnosticParams)

            match r with
            | Some(U2.C1 report) -> report.Items.Length > 0
            | _ -> false)
        "Expected diagnostics to appear after .cshtml file was overwritten and Changed notification sent"

[<Test>]
let testDidChangeWatchedFilesDeletedCshtmlFileRemovesDocument () =
    use client = activateFixture "aspnetProject"

    let cshtmlPath =
        Path.Combine(client.SolutionDir, "Project", "Views", "Test", "Index.cshtml")

    let cshtmlUri =
        "Project/Views/Test/Index.cshtml" |> fileUriForProjectDir client.SolutionDir

    let timeout = TimeSpan.FromSeconds(10L)

    // Inject errors into the file so the server emits diagnostics for it.
    // This is important for the post-delete assertion: if the document were
    // still registered after deletion we would see those diagnostics, so
    // "no diagnostics" unambiguously means the document was removed.
    File.WriteAllText(cshtmlPath, "@model Project.Models.Test.IndexViewModel\n@Model.InvalidProperty", Encoding.UTF8)

    notifyFileChanged client cshtmlUri FileChangeType.Changed

    // Wait until the server has actually processed the error so the
    // subsequent delete starts from a known state (diagnostics present).
    waitUntilOrTimeout
        timeout
        (fun () ->
            let items = cshtmlUri |> getWorkspaceDiagnosticsForUri client
            items.Length > 0)
        "Expected diagnostics to appear after .cshtml file was overwritten with errors"

    // Delete the file from disk and notify the server.
    File.Delete(cshtmlPath)
    notifyFileChanged client cshtmlUri FileChangeType.Deleted

    // After deletion the document should be removed from the solution entirely,
    // so workspace/diagnostic should return no items for this URI.
    waitUntilOrTimeout
        timeout
        (fun () ->
            let items = cshtmlUri |> getWorkspaceDiagnosticsForUri client
            items.IsEmpty)
        "Expected no workspace diagnostics for deleted .cshtml file"
