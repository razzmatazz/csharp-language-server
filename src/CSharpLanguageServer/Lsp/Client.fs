module CSharpLanguageServer.Lsp.Client

open System.Text.Json
open Microsoft.Extensions.Logging

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
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
            | Result.Ok je -> je |> LSPAny.fromJsonElement |> deserialize |> Result.Ok
            | Result.Error errEl ->
                let tryGetProperty (name: string) =
                    match errEl.ValueKind with
                    | JsonValueKind.Object ->
                        match errEl.TryGetProperty(name) with
                        | true, v -> Some v
                        | false, _ -> None
                    | _ -> None

                let code =
                    tryGetProperty "code"
                    |> Option.bind (fun v ->
                        match v.TryGetInt32() with
                        | true, i -> Some i
                        | false, _ -> None)
                    |> Option.defaultValue -32603 // -32603 = JSON-RPC "Internal error"

                let message =
                    tryGetProperty "message"
                    |> Option.bind (fun v ->
                        if v.ValueKind = JsonValueKind.String then
                            Some(v.GetString())
                        else
                            None)
                    |> Option.defaultValue "Unknown error"

                // Note: `Ionide.LanguageServerProtocol.JsonRpc.Error.Data` is typed as
                // `System.Text.Json.JsonElement option` by the vendored library, which is
                // outside the scope of this transport adapter. Nothing in csharp-ls reads
                // this field for client-originated transport errors, so it is always `None`
                // here.
                Result.Error
                    { Code = code
                      Message = message
                      Data = None }
    }

    override __.WindowShowMessage p =
        sendServerNotification "window/showMessage" ((serialize p).JsonElement)

    // Note: CSharpLspClient is a pure transport adapter. It does not gate calls on
    // ClientCapabilities — that is the responsibility of callers, who have access to
    // capabilities via RequestContext. See ProgressReporter for the reference pattern.

    override __.WindowShowMessageRequest p : AsyncLspResult<Types.MessageActionItem option> =
        sendServerRequest "window/showMessageRequest" ((serialize p).JsonElement)

    override __.WindowLogMessage p =
        sendServerNotification "window/logMessage" ((serialize p).JsonElement)

    override __.TelemetryEvent p =
        sendServerNotification "telemetry/event" (p.JsonElement)

    override __.ClientRegisterCapability p =
        sendServerRequest "client/registerCapability" ((serialize p).JsonElement)

    override __.ClientUnregisterCapability p =
        sendServerRequest "client/unregisterCapability" ((serialize p).JsonElement)

    override __.WorkspaceWorkspaceFolders() : AsyncLspResult<Types.WorkspaceFolder[] option> =
        sendServerRequest "workspace/workspaceFolders" nullJE

    override __.WorkspaceConfiguration p : AsyncLspResult<LSPAny[]> =
        sendServerRequest "workspace/configuration" ((serialize p).JsonElement)

    override __.WorkspaceApplyEdit p : AsyncLspResult<Types.ApplyWorkspaceEditResult> =
        sendServerRequest "workspace/applyEdit" ((serialize p).JsonElement)

    override __.WorkspaceSemanticTokensRefresh() =
        sendServerRequest "workspace/semanticTokens/refresh" nullJE

    override __.WorkspaceDiagnosticRefresh() =
        sendServerRequest "workspace/diagnostic/refresh" nullJE

    override __.TextDocumentPublishDiagnostics p =
        sendServerNotification "textDocument/publishDiagnostics" ((serialize p).JsonElement)

    override __.WindowWorkDoneProgressCreate createParams =
        sendServerRequest "window/workDoneProgress/create" ((serialize createParams).JsonElement)

    override __.LogTrace p =
        sendServerNotification "$/logTrace" ((serialize p).JsonElement)

    override __.Progress progressParams =
        sendServerNotification "$/progress" ((serialize progressParams).JsonElement)

    /// Query the client for the `csharp` workspace configuration section.
    /// Returns `None` when the call fails or the response cannot be deserialized.
    static member TryPullCSharpConfig(lspClient: ILspClient) : Async<CSharpConfiguration option> = async {
        try
            let! (result: Result<LSPAny[], _>) =
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
