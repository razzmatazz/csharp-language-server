namespace CSharpLanguageServer

open System.IO
open Microsoft.Extensions.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp

module Diagnostics =
    let private logger = Logging.getLoggerByName "Diagnostics"

    type LspClientStub() =
        interface System.IDisposable with
            member _.Dispose() = ()

        interface ILspClient with
            member _.WindowShowMessage(p: ShowMessageParams) = async {
                logger.LogDebug("WindowShowMessage: {message}", p.Message)
                return ()
            }

            member _.WindowLogMessage(p: LogMessageParams) = async {
                logger.LogDebug("WindowLogMessage: {message}", p.Message)
                return ()
            }

            member _.TelemetryEvent(_: LSPAny) = async { return () }
            member _.TextDocumentPublishDiagnostics(_: PublishDiagnosticsParams) = async { return () }
            member _.LogTrace(_: LogTraceParams) = async { return () }
            member _.CancelRequest(_: CancelParams) = async { return () }
            member _.Progress(_: ProgressParams) = async { return () }
            member _.WorkspaceWorkspaceFolders() = async { return LspResult.Ok None }
            member _.WorkspaceConfiguration(_: ConfigurationParams) = async { return LspResult.Ok [||] }
            member _.WindowWorkDoneProgressCreate(_: WorkDoneProgressCreateParams) = async { return LspResult.Ok() }
            member _.WorkspaceSemanticTokensRefresh() = async { return LspResult.Ok() }
            member _.WindowShowDocument(_: ShowDocumentParams) = async { return LspResult.Ok { Success = false } }
            member _.WorkspaceInlineValueRefresh() = async { return LspResult.Ok() }
            member _.WorkspaceInlayHintRefresh() = async { return LspResult.Ok() }
            member _.WorkspaceDiagnosticRefresh() = async { return LspResult.Ok() }
            member _.ClientRegisterCapability(_: RegistrationParams) = async { return LspResult.Ok() }
            member _.ClientUnregisterCapability(_: UnregistrationParams) = async { return LspResult.Ok() }
            member _.WindowShowMessageRequest(_: ShowMessageRequestParams) = async { return LspResult.Ok None }
            member _.WorkspaceCodeLensRefresh() = async { return LspResult.Ok() }

            member _.WorkspaceApplyEdit(_: ApplyWorkspaceEditParams) = async {
                return
                    LspResult.Ok
                        { Applied = false
                          FailureReason = None
                          FailedChange = None }
            }

    let diagnoseClientCapabilities =
        { emptyClientCapabilities with
            Window =
                Some
                    { WorkDoneProgress = Some true
                      ShowMessage = None
                      ShowDocument = None } }

    let diagnose (settings: ServerSettings) : Async<int> = async {
        logger.LogDebug("diagnose: settings={settings}", settings)

        initializeMSBuild ()

        logger.LogDebug("diagnose: loading solution..")

        let lspClient = new LspClientStub()
        let cwd = string (Directory.GetCurrentDirectory())
        let progressReporter = ProgressReporter(lspClient, diagnoseClientCapabilities)
        let! _sln = solutionLoadSolutionWithPathOrOnDir lspClient progressReporter None cwd

        logger.LogDebug("diagnose: done")

        let exitCode = 0
        return exitCode
    }
