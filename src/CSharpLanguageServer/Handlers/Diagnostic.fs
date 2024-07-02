namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Diagnostic =
    let provider (clientCapabilities: ClientCapabilities) = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (context: ServerRequestContext) (def: DocumentDiagnosticParams) : AsyncLspResult<DocumentDiagnosticReport> =
        LspResult.notImplemented<DocumentDiagnosticReport> |> async.Return
