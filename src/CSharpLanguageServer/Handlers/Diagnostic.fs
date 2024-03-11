namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Diagnostic =
    let provider (clientCapabilities: ClientCapabilities option) = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None

    let handle (scope: ServerRequestScope) (def: DocumentDiagnosticParams) : AsyncLspResult<DocumentDiagnosticReport option> =
        LspResult.notImplemented<DocumentDiagnosticReport option> |> async.Return
