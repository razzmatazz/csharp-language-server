namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Diagnostic =
    let private logger = LogProvider.getLoggerByName "Diagnostic"

    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.Diagnostic)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities): U2<DiagnosticOptions, DiagnosticRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false ->
            let diagnosticOptions: DiagnosticRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None
                  Identifier = None
                  InterFileDependencies = false
                  WorkspaceDiagnostics = false
                  Id = None
                }

            Some (U2.C2 diagnosticOptions)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: DiagnosticRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None
                  Identifier = None
                  InterFileDependencies = false
                  WorkspaceDiagnostics = false
                  Id = None }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/diagnostic"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: DocumentDiagnosticParams) : AsyncLspResult<DocumentDiagnosticReport> = async {
        let emptyReport: RelatedFullDocumentDiagnosticReport =
            {
                Kind = "full"
                ResultId = None
                Items = [| |]
                RelatedDocuments = None
            }

        match context.GetDocument p.TextDocument.Uri with
        | None ->
            return emptyReport |> U2.C1 |> LspResult.success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModelMaybe = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            match semanticModelMaybe |> Option.ofObj with
            | Some semanticModel ->
                let diagnostics =
                    semanticModel.GetDiagnostics()
                    |> Seq.map Diagnostic.fromRoslynDiagnostic
                    |> Array.ofSeq

                return { emptyReport with Items = diagnostics }
                       |> U2.C1
                       |> LspResult.success

            | None ->
                return emptyReport |> U2.C1 |> LspResult.success
    }
