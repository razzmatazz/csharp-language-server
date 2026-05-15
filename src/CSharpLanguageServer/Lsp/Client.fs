module CSharpLanguageServer.Lsp.Client

open System.Text.Json
open Newtonsoft.Json.Linq
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
            | Result.Ok je -> je.GetRawText() |> JToken.Parse |> deserialize |> Result.Ok
            | Result.Error errEl ->
                let errToken = JToken.Parse(errEl.GetRawText())

                let code =
                    errToken.SelectToken("code")
                    |> Option.ofObj
                    |> Option.bind (fun t ->
                        t.ToString()
                        |> System.Int32.TryParse
                        |> function
                            | true, v -> Some v
                            | _ -> None)
                    |> Option.defaultValue -32603 // -32603 = JSON-RPC "Internal error"

                let message =
                    errToken.SelectToken("message")
                    |> Option.ofObj
                    |> Option.map _.ToObject<string>()
                    |> Option.defaultValue "Unknown error"

                let data = errToken.SelectToken("data") |> Option.ofObj

                Result.Error
                    { Code = code
                      Message = message
                      Data = data }
    }

    override __.WindowShowMessage p =
        sendServerNotification "window/showMessage" (serialize p |> jtokenToJe)

    // Note: CSharpLspClient is a pure transport adapter. It does not gate calls on
    // ClientCapabilities — that is the responsibility of callers, who have access to
    // capabilities via RequestContext. See ProgressReporter for the reference pattern.

    override __.WindowShowMessageRequest p : AsyncLspResult<Types.MessageActionItem option> =
        sendServerRequest "window/showMessageRequest" (serialize p |> jtokenToJe)

    override __.WindowLogMessage p =
        sendServerNotification "window/logMessage" (serialize p |> jtokenToJe)

    override __.TelemetryEvent p =
        sendServerNotification "telemetry/event" (serialize p |> jtokenToJe)

    override __.ClientRegisterCapability p =
        sendServerRequest "client/registerCapability" (serialize p |> jtokenToJe)

    override __.ClientUnregisterCapability p =
        sendServerRequest "client/unregisterCapability" (serialize p |> jtokenToJe)

    override __.WorkspaceWorkspaceFolders() : AsyncLspResult<Types.WorkspaceFolder[] option> =
        sendServerRequest "workspace/workspaceFolders" nullJe

    override __.WorkspaceConfiguration p : AsyncLspResult<JToken[]> =
        sendServerRequest "workspace/configuration" (serialize p |> jtokenToJe)

    override __.WorkspaceApplyEdit p : AsyncLspResult<Types.ApplyWorkspaceEditResult> =
        sendServerRequest "workspace/applyEdit" (serialize p |> jtokenToJe)

    override __.WorkspaceSemanticTokensRefresh() =
        sendServerRequest "workspace/semanticTokens/refresh" nullJe

    override __.WorkspaceDiagnosticRefresh() =
        sendServerRequest "workspace/diagnostic/refresh" nullJe

    override __.TextDocumentPublishDiagnostics p =
        sendServerNotification "textDocument/publishDiagnostics" (serialize p |> jtokenToJe)

    override __.WindowWorkDoneProgressCreate createParams =
        sendServerRequest "window/workDoneProgress/create" (serialize createParams |> jtokenToJe)

    override __.LogTrace p =
        sendServerNotification "$/logTrace" (serialize p |> jtokenToJe)

    override __.Progress progressParams =
        sendServerNotification "$/progress" (serialize progressParams |> jtokenToJe)

    /// Query the client for the `csharp` workspace configuration section.
    /// Returns `None` when the call fails or the response cannot be deserialized.
    static member TryPullCSharpConfig(lspClient: ILspClient) : Async<CSharpConfiguration option> = async {
        try
            let! (result: Result<JToken[], _>) =
                lspClient.WorkspaceConfiguration(
                    { Items =
                        [| { Section = Some "csharp"
                             ScopeUri = None } |] }
                )

            return
                result
                |> Option.fromResult
                |> Option.bind Seq.tryHead
                |> Option.bind deserialize<CSharpConfiguration option>
        with ex ->
            logger.LogWarning("could not retrieve `csharp` workspace configuration section: {error}", ex |> string)

            return None
    }
