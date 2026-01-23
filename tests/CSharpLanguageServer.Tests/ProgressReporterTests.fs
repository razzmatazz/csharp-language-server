module CSharpLanguageServer.Tests.ProgressReporterTests

open System
open NUnit.Framework
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Tests.Tooling

/// Client stub that tracks whether WindowWorkDoneProgressCreate was called
type TrackingLspClientStub() =
    let mutable createCalled = false

    member _.WasCreateCalled = createCalled

    interface System.IDisposable with
        member _.Dispose() = ()

    interface ILspClient with
        member _.WindowWorkDoneProgressCreate(_: WorkDoneProgressCreateParams) =
            createCalled <- true
            async { return LspResult.Ok() }

        member _.Progress(_: ProgressParams) = async { return () }
        member _.WindowShowMessage(_) = async { return () }
        member _.WindowLogMessage(_) = async { return () }
        member _.TelemetryEvent(_) = async { return () }
        member _.TextDocumentPublishDiagnostics(_) = async { return () }
        member _.LogTrace(_) = async { return () }
        member _.CancelRequest(_) = async { return () }
        member _.WorkspaceWorkspaceFolders() = async { return LspResult.Ok None }
        member _.WorkspaceConfiguration(_) = async { return LspResult.Ok [||] }
        member _.WorkspaceSemanticTokensRefresh() = async { return LspResult.Ok() }
        member _.WindowShowDocument(_) = async { return LspResult.Ok { Success = false } }
        member _.WorkspaceInlineValueRefresh() = async { return LspResult.Ok() }
        member _.WorkspaceInlayHintRefresh() = async { return LspResult.Ok() }
        member _.WorkspaceDiagnosticRefresh() = async { return LspResult.Ok() }
        member _.ClientRegisterCapability(_) = async { return LspResult.Ok() }
        member _.ClientUnregisterCapability(_) = async { return LspResult.Ok() }
        member _.WindowShowMessageRequest(_) = async { return LspResult.Ok None }
        member _.WorkspaceCodeLensRefresh() = async { return LspResult.Ok() }
        member _.WorkspaceApplyEdit(_) = async {
            return LspResult.Ok { Applied = false; FailureReason = None; FailedChange = None }
        }

let capabilitiesWithWorkDoneProgress (supported: bool): ClientCapabilities =
    let windowCaps: WindowClientCapabilities =
        { WorkDoneProgress = Some supported
          ShowMessage = None
          ShowDocument = None }

    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = Some windowCaps
      General = None
      Experimental = None }

[<Test>]
let ``ProgressReporter does not call WindowWorkDoneProgressCreate when capability not supported`` () =
    let client = new TrackingLspClientStub()
    let reporter = new ProgressReporter(client, emptyClientCapabilities)

    reporter.Begin("Test Title") |> Async.RunSynchronously

    Assert.That(client.WasCreateCalled, Is.False, "WindowWorkDoneProgressCreate should not be called when capability is not supported")

[<Test>]
let ``ProgressReporter does not call WindowWorkDoneProgressCreate when WorkDoneProgress is false`` () =
    let client = new TrackingLspClientStub()
    let caps = capabilitiesWithWorkDoneProgress false
    let reporter = new ProgressReporter(client, caps)

    reporter.Begin("Test Title") |> Async.RunSynchronously

    Assert.That(client.WasCreateCalled, Is.False, "WindowWorkDoneProgressCreate should not be called when WorkDoneProgress is false")

[<Test>]
let ``ProgressReporter calls WindowWorkDoneProgressCreate when capability is supported`` () =
    let client = new TrackingLspClientStub()
    let caps = capabilitiesWithWorkDoneProgress true
    let reporter = new ProgressReporter(client, caps)

    reporter.Begin("Test Title") |> Async.RunSynchronously

    Assert.That(client.WasCreateCalled, Is.True, "WindowWorkDoneProgressCreate should be called when WorkDoneProgress is true")

[<Test>]
let ``ProgressReporter Report and End are no-ops when capability not supported`` () =
    let client = new TrackingLspClientStub()
    let reporter = new ProgressReporter(client, emptyClientCapabilities)

    // Begin with unsupported capability
    reporter.Begin("Test Title") |> Async.RunSynchronously

    // Report and End should not throw
    Assert.DoesNotThrowAsync(fun () ->
        reporter.Report(message = "Progress") |> Async.StartAsTask :> System.Threading.Tasks.Task
    )
    Assert.DoesNotThrowAsync(fun () ->
        reporter.End(message = "Done") |> Async.StartAsTask :> System.Threading.Tasks.Task
    )
