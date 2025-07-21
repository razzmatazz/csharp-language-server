namespace CSharpLanguageServer.Handlers

open System
open System.IO

open FSharp.Control
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

    let private registrationOptions: DiagnosticRegistrationOptions =
        { DocumentSelector = Some defaultDocumentSelector
          WorkDoneProgress = None
          Identifier = None
          InterFileDependencies = false
          WorkspaceDiagnostics = true
          Id = None
        }

    let provider (clientCapabilities: ClientCapabilities): U2<DiagnosticOptions, DiagnosticRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U2.C2 registrationOptions)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registration =
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/diagnostic"
                  RegisterOptions = registrationOptions |> serialize |> Some
                }

            Some registration

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

    let private getWorkspaceDiagnosticReports (solution: Microsoft.CodeAnalysis.Solution) : AsyncSeq<WorkspaceDocumentDiagnosticReport> =
        asyncSeq {
            for project in solution.Projects do
                let! compilation = project.GetCompilationAsync() |> Async.AwaitTask

                match compilation |> Option.ofObj with
                | None -> ()
                | Some compilation ->
                    let diagnostics = compilation.GetDiagnostics()

                    for d in diagnostics do
                        if d.Location.SourceTree = null then
                            let text = sprintf "getWorkspaceDiagnosticReports, d=%s has no sourcetree" (string d)
                            File.AppendAllText("/Users/bob/output.txt", text)

                    let diagnosticsByDocument =
                        diagnostics
//                        |> Seq.filter (fun d -> d.Location.SourceTree <> null)
                        |> Seq.groupBy (fun d -> d.Location.SourceTree.FilePath |> Path.toUri)

                    for (uri, docDiagnostics) in diagnosticsByDocument do
                        let fullDocumentReport: WorkspaceFullDocumentDiagnosticReport =
                            {
                                Kind = "full"
                                ResultId = None
                                Uri = uri
                                Items = docDiagnostics |> Seq.map Diagnostic.fromRoslynDiagnostic |> Array.ofSeq
                                Version = None
                            }

                        let documentReport: WorkspaceDocumentDiagnosticReport =
                            U2.C1 fullDocumentReport

                        yield documentReport
        }

    let handleWorkspaceDiagnostic (context: ServerRequestContext) (p: WorkspaceDiagnosticParams) : AsyncLspResult<WorkspaceDiagnosticReport> = async {
        let emptyWorkspaceDiagnosticReport: WorkspaceDiagnosticReport =
            { Items = Array.empty }

        match context.State.Solution, p.PartialResultToken with
        | None, _ ->
            return emptyWorkspaceDiagnosticReport |> LspResult.success

        | Some solution, None ->
            let! diagnosticReports =
                getWorkspaceDiagnosticReports solution
                |> AsyncSeq.toArrayAsync

            let workspaceDiagnosticReport: WorkspaceDiagnosticReport =
                { Items = diagnosticReports }

            return workspaceDiagnosticReport |> LspResult.success

        | Some solution, Some partialResultToken ->
            let sendWorkspaceDiagnosticReport (documentReport, index) = async {
                let progressParams =
                    if index = 0 then
                        let report: WorkspaceDiagnosticReport =
                            { Items = [| documentReport |] }

                        { Token = partialResultToken; Value = serialize report }
                    else
                        let reportPartialResult: WorkspaceDiagnosticReportPartialResult =
                            { Items = [| documentReport |] }

                        { Token = partialResultToken; Value = serialize reportPartialResult }

                let lspClient = context.State.LspClient.Value
                do! lspClient.Progress(progressParams)
            }

            do! AsyncSeq.ofSeq (Seq.initInfinite id)
                |> AsyncSeq.zip (getWorkspaceDiagnosticReports solution)
                |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

            return emptyWorkspaceDiagnosticReport |> LspResult.success
    }
