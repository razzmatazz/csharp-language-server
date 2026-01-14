namespace CSharpLanguageServer.Handlers

open System

open FSharp.Control
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module Diagnostic =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Diagnostic
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let private registrationOptions documentSelector : DiagnosticRegistrationOptions =
        { DocumentSelector = documentSelector |> Some
          WorkDoneProgress = None
          Identifier = None
          InterFileDependencies = false
          WorkspaceDiagnostics = true
          Id = None }

    let provider
        (settings: ServerSettings)
        (cc: ClientCapabilities)
        : U2<DiagnosticOptions, DiagnosticRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            let documentSelector = documentSelectorForCSharpAndRazorDocuments settings

            registrationOptions documentSelector |> U2.C2 |> Some

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let documentSelector = documentSelectorForCSharpAndRazorDocuments settings

            let registration =
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/diagnostic"
                  RegisterOptions = registrationOptions documentSelector |> serialize |> Some }

            Some registration

    let handle
        (context: ServerRequestContext)
        (p: DocumentDiagnosticParams)
        : AsyncLspResult<DocumentDiagnosticReport> =
        async {
            let emptyReport: RelatedFullDocumentDiagnosticReport =
                { Kind = "full"
                  ResultId = None
                  Items = [||]
                  RelatedDocuments = None }

            let! wf, semModel = p.TextDocument.Uri |> workspaceDocumentSemanticModel context.Workspace

            match wf, semModel with
            | Some wf, Some semanticModel ->
                let! ct = Async.CancellationToken

                let wfPathToUri = workspaceFolderPathToUri wf

                let diagnostics =
                    semanticModel.GetDiagnostics()
                    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                    |> Seq.map fst
                    |> Array.ofSeq

                return { emptyReport with Items = diagnostics } |> U2.C1 |> LspResult.success

            | _, _ -> return emptyReport |> U2.C1 |> LspResult.success
        }

    let private getWorkspaceDiagnosticReports (workspace: LspWorkspace) : AsyncSeq<WorkspaceDocumentDiagnosticReport> = asyncSeq {
        let! ct = Async.CancellationToken

        for wf in workspace.Folders do
            let pathToUri = workspaceFolderPathToUri wf

            let solutionProjects =
                wf.Solution |> Option.map _.Projects |> Option.defaultValue Seq.empty

            for project in solutionProjects do
                let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

                match compilation |> Option.ofObj with
                | None -> ()
                | Some compilation ->
                    let uriForDiagnostic (d: Microsoft.CodeAnalysis.Diagnostic) =
                        d.Location.SourceTree
                        |> Option.ofObj
                        |> Option.map _.FilePath
                        |> Option.map pathToUri

                    let diagnosticsByDocument =
                        compilation.GetDiagnostics(ct)
                        |> Seq.groupBy uriForDiagnostic
                        |> Seq.filter (fun (uri, _ds) -> uri.IsSome)
                        |> Seq.map (fun (uri, ds) -> uri.Value, ds)

                    for (uri, docDiagnostics) in diagnosticsByDocument do
                        let items =
                            docDiagnostics
                            |> Seq.map (Diagnostic.fromRoslynDiagnostic pathToUri)
                            |> Seq.map fst
                            |> Array.ofSeq

                        let fullDocumentReport: WorkspaceFullDocumentDiagnosticReport =
                            { Kind = "full"
                              ResultId = None
                              Uri = uri
                              Items = items
                              Version = None }

                        let documentReport: WorkspaceDocumentDiagnosticReport = U2.C1 fullDocumentReport

                        yield documentReport
    }

    let handleWorkspaceDiagnostic
        (context: ServerRequestContext)
        (p: WorkspaceDiagnosticParams)
        : AsyncLspResult<WorkspaceDiagnosticReport> =
        async {
            match p.PartialResultToken with
            | None ->
                let! diagnosticReports = getWorkspaceDiagnosticReports context.Workspace |> AsyncSeq.toArrayAsync

                let workspaceDiagnosticReport: WorkspaceDiagnosticReport =
                    { Items = diagnosticReports }

                return workspaceDiagnosticReport |> LspResult.success

            | Some partialResultToken ->
                let sendWorkspaceDiagnosticReport (documentReport, index) = async {
                    let progressParams =
                        if index = 0 then
                            let report: WorkspaceDiagnosticReport = { Items = [| documentReport |] }

                            { Token = partialResultToken
                              Value = serialize report }
                        else
                            let reportPartialResult: WorkspaceDiagnosticReportPartialResult =
                                { Items = [| documentReport |] }

                            { Token = partialResultToken
                              Value = serialize reportPartialResult }

                    let lspClient = context.State.LspClient.Value
                    do! lspClient.Progress(progressParams)
                }

                do!
                    AsyncSeq.ofSeq (Seq.initInfinite id)
                    |> AsyncSeq.zip (getWorkspaceDiagnosticReports context.Workspace)
                    |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

                let emptyWorkspaceDiagnosticReport: WorkspaceDiagnosticReport =
                    { Items = Array.empty }

                return emptyWorkspaceDiagnosticReport |> LspResult.success
        }
