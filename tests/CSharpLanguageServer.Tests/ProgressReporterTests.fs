module CSharpLanguageServer.Tests.ProgressReporterTests

open System
open NUnit.Framework
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open CSharpLanguageServer.Lsp

/// A test stub that throws an exception when WindowWorkDoneProgressCreate is called,
/// simulating a client that doesn't support this method.
type ThrowingLspClientStub() =
    interface System.IDisposable with
        member _.Dispose() = ()

    interface ILspClient with
        member _.WindowWorkDoneProgressCreate(_: WorkDoneProgressCreateParams) =
            async { return raise (Exception("Method not supported")) }

        member _.Progress(_: ProgressParams) = async { return () }

        // Other required interface members return default values
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

[<Test>]
let ``ProgressReporter Begin does not throw when client throws exception`` () =
    let client = new ThrowingLspClientStub()
    let reporter = new ProgressReporter(client)

    // Should not throw - exception should be caught internally
    Assert.DoesNotThrowAsync(fun () ->
        reporter.Begin("Test Title") |> Async.StartAsTask :> System.Threading.Tasks.Task
    )

[<Test>]
let ``ProgressReporter Report and End are no-ops after failed Begin`` () =
    let client = new ThrowingLspClientStub()
    let reporter = new ProgressReporter(client)

    // Begin with a throwing client
    reporter.Begin("Test Title") |> Async.RunSynchronously

    // Report and End should not throw (they should be no-ops since canReport is false)
    Assert.DoesNotThrowAsync(fun () ->
        reporter.Report(message = "Progress") |> Async.StartAsTask :> System.Threading.Tasks.Task
    )
    Assert.DoesNotThrowAsync(fun () ->
        reporter.End(message = "Done") |> Async.StartAsTask :> System.Threading.Tasks.Task
    )
