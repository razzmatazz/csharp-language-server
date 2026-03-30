namespace CSharpLanguageServer.Lsp

open Newtonsoft.Json.Linq

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

type CSharpLspClient
    (
        sendServerNotification: string -> JToken -> Async<unit>,
        sendServerRequest_: string -> JToken -> Async<Result<JToken, JToken>>
    ) =
    inherit LspClient()

    let sendServerRequest m (p: JToken) : AsyncLspResult<'TResult> = async {
        let! result = sendServerRequest_ m p

        return
            match result with
            | Result.Ok jtoken -> jtoken |> deserialize |> Result.Ok
            | Result.Error err -> failwith "TODO"
    }

    override __.WindowShowMessage p =
        sendServerNotification "window/showMessage" (serialize p)

    // TODO: Send notifications / requests to client only if client support it

    override __.WindowShowMessageRequest p : AsyncLspResult<Types.MessageActionItem option> =
        sendServerRequest "window/showMessageRequest" (serialize p)

    override __.WindowLogMessage p =
        sendServerNotification "window/logMessage" (serialize p)

    override __.TelemetryEvent p =
        sendServerNotification "telemetry/event" (serialize p)

    override __.ClientRegisterCapability p =
        sendServerRequest "client/registerCapability" (serialize p)

    override __.ClientUnregisterCapability p =
        sendServerRequest "client/unregisterCapability" (serialize p)

    override __.WorkspaceWorkspaceFolders() : AsyncLspResult<Types.WorkspaceFolder[] option> =
        sendServerRequest "workspace/workspaceFolders" (JValue.CreateNull())

    override __.WorkspaceConfiguration p : AsyncLspResult<JToken[]> =
        sendServerRequest "workspace/configuration" (serialize p)

    override __.WorkspaceApplyEdit p : AsyncLspResult<Types.ApplyWorkspaceEditResult> =
        sendServerRequest "workspace/applyEdit" (serialize p)

    override __.WorkspaceSemanticTokensRefresh() =
        sendServerRequest "workspace/semanticTokens/refresh" (JValue.CreateNull())

    override __.TextDocumentPublishDiagnostics p =
        sendServerNotification "textDocument/publishDiagnostics" (serialize p)

    override __.WindowWorkDoneProgressCreate createParams =
        sendServerRequest "window/workDoneProgress/create" (serialize createParams)

    override __.LogTrace p =
        sendServerNotification "$/logTrace" (serialize p)

    override __.Progress progressParams =
        sendServerNotification "$/progress" (serialize progressParams)
