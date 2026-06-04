module CSharpLanguageServer.Lsp.Client

open System.Text.Json
open Microsoft.Extensions.Logging

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Types
open CSharpLanguageServer.Util
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "Lsp.Client"

type CSharpLspClient
    (
        sendServerNotification: string -> JsonElement -> Async<unit>,
        sendServerRequest_: string -> JsonElement -> Async<Result<JsonElement, JsonElement>>
    ) =
    inherit LspClient()

    let sendServerRequest m (p: JsonElement) : AsyncLspResult<'TResult> = async {
        let! result = sendServerRequest_ m p

        return
            match result with
            | Result.Ok je -> je |> deserialize |> Result.Ok
            | Result.Error errEl ->
                let code =
                    match errEl.TryGetProperty("code") with
                    | true, v ->
                        match v.TryGetInt32() with
                        | true, i -> i
                        | _ -> -32603
                    | _ -> -32603

                let message =
                    match errEl.TryGetProperty("message") with
                    | true, v -> v.GetString()
                    | _ -> "Unknown error"

                let data =
                    match errEl.TryGetProperty("data") with
                    | true, v -> Some v
                    | _ -> None

                Result.Error
                    { Code = code
                      Message = message
                      Data = data }
    }

    override __.WindowShowMessage p =
        sendServerNotification "window/showMessage" (serialize p)

    // Note: CSharpLspClient is a pure transport adapter. It does not gate calls on
    // ClientCapabilities — that is the responsibility of callers, who have access to
    // capabilities via RequestContext. See ProgressReporter for the reference pattern.

    override __.WindowShowMessageRequest p : AsyncLspResult<Types.MessageActionItem option> =
        sendServerRequest "window/showMessageRequest" (serialize p)

    override __.WindowLogMessage p =
        sendServerNotification "window/logMessage" (serialize p)

    override __.TelemetryEvent p =
        sendServerNotification "telemetry/event" (p)

    override __.ClientRegisterCapability p =
        sendServerRequest "client/registerCapability" (serialize p)

    override __.ClientUnregisterCapability p =
        sendServerRequest "client/unregisterCapability" (serialize p)

    override __.WorkspaceWorkspaceFolders() : AsyncLspResult<Types.WorkspaceFolder[] option> =
        sendServerRequest "workspace/workspaceFolders" nullJE

    override __.WorkspaceConfiguration p : AsyncLspResult<JsonElement[]> =
        sendServerRequest "workspace/configuration" (serialize p)

    override __.WorkspaceApplyEdit p : AsyncLspResult<Types.ApplyWorkspaceEditResult> =
        sendServerRequest "workspace/applyEdit" (serialize p)

    override __.WorkspaceSemanticTokensRefresh() =
        sendServerRequest "workspace/semanticTokens/refresh" nullJE

    override __.WorkspaceDiagnosticRefresh() =
        sendServerRequest "workspace/diagnostic/refresh" nullJE

    override __.TextDocumentPublishDiagnostics p =
        sendServerNotification "textDocument/publishDiagnostics" (serialize p)

    override __.WindowWorkDoneProgressCreate createParams =
        sendServerRequest "window/workDoneProgress/create" (serialize createParams)

    override __.LogTrace p =
        sendServerNotification "$/logTrace" (serialize p)

    override __.Progress progressParams =
        sendServerNotification "$/progress" (serialize progressParams)

    /// Query the client for the `csharp` workspace configuration section.
    /// Returns `None` when the call fails or the response cannot be deserialized.
    static member TryPullCSharpConfig(lspClient: ILspClient) : Async<CSharpConfiguration option> = async {
        try
            let! (result: Result<JsonElement[], _>) =
                lspClient.WorkspaceConfiguration(
                    { Items =
                        [| { Section = Some "csharp"
                             ScopeUri = None } |] }
                )

            return
                result
                |> Option.fromResult
                |> Option.bind Seq.tryHead
                |> Option.bind (fun je -> deserialize<CSharpConfiguration option> je)
        with ex ->
            logger.LogWarning("could not retrieve `csharp` workspace configuration section: {error}", ex |> string)

            return None
    }
