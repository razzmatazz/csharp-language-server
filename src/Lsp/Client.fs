namespace CSharpLanguageServer.Lsp

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Common.Types

type CSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient()

    let mutable capabilities: ClientCapabilities option = None

    override __.WindowShowMessage(p) =
        sendServerNotification "window/showMessage" (box p) |> Async.Ignore

    override __.WindowShowMessageRequest(p) =
        sendServerRequest.Send "window/showMessageRequest" (box p)

    override __.WindowLogMessage(p) =
        sendServerNotification "window/logMessage" (box p) |> Async.Ignore

    override __.TelemetryEvent(p) =
        sendServerNotification "telemetry/event" (box p) |> Async.Ignore

    override __.ClientRegisterCapability(p) =
        sendServerRequest.Send "client/registerCapability" (box p)

    override __.ClientUnregisterCapability(p) =
        sendServerRequest.Send "client/unregisterCapability" (box p)

    override __.WorkspaceWorkspaceFolders() =
        sendServerRequest.Send "workspace/workspaceFolders" ()

    override __.WorkspaceConfiguration(p) =
        sendServerRequest.Send "workspace/configuration" (box p)

    override __.WorkspaceApplyEdit(p) =
        sendServerRequest.Send "workspace/applyEdit" (box p)

    override __.WorkspaceSemanticTokensRefresh() =
        sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    interface ICSharpLspClient with
        override this.Capabilities with get() = capabilities and set(v) = capabilities <- v
