namespace CSharpLanguageServer.Handlers

open System
open System.Threading.Channels

open FSharp.Control
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

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
        (config: CSharpConfiguration)
        (cc: ClientCapabilities)
        : U2<DiagnosticOptions, DiagnosticRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            let documentSelector = documentSelectorForCSharpAndRazorDocuments config

            registrationOptions documentSelector |> U2.C2 |> Some

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let documentSelector = documentSelectorForCSharpAndRazorDocuments config

            let registration =
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/diagnostic"
                  RegisterOptions = registrationOptions documentSelector |> serialize |> Some }

            Some registration

    let private diagnosticIsToBeListed (uri: string) (d: Microsoft.CodeAnalysis.Diagnostic) =
        if uri.EndsWith(".cshtml", StringComparison.OrdinalIgnoreCase) then
            // CS8019 (Unnecessary using directive) has no correct line-mapping on .cshtml
            // files and appears out of place there; suppress it
            d.Id <> "CS8019"
        else
            true

    let handle
        (context: RequestContext)
        (p: DocumentDiagnosticParams)
        : Async<LspResult<DocumentDiagnosticReport> * LspWorkspaceUpdate> =
        async {
            let emptyReport: RelatedFullDocumentDiagnosticReport =
                { Kind = "full"
                  ResultId = None
                  Items = [||]
                  RelatedDocuments = None }

            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

            match wf with
            | None -> return emptyReport |> U2.C1 |> LspResult.success, LspWorkspaceUpdate.Empty
            | Some wf ->
                let! semModel = workspaceFolderDocumentSemanticModel p.TextDocument.Uri wf

                match semModel with
                | Some semanticModel ->
                    let! ct = Async.CancellationToken

                    let wfPathToUri path = workspaceFolderPathToUri path wf

                    let diagnostics =
                        semanticModel.GetDiagnostics()
                        |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
                        |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                        |> Seq.map fst
                        |> Array.ofSeq

                    return
                        { emptyReport with Items = diagnostics } |> U2.C1 |> LspResult.success, LspWorkspaceUpdate.Empty

                | None -> return emptyReport |> U2.C1 |> LspResult.success, LspWorkspaceUpdate.Empty
        }

    type WorkspaceDiagnosticsReportsChannelItem =
        | DiagnosticsReport of WorkspaceDocumentDiagnosticReport
        | ReportingDoneForProject

    let private getWorkspaceDiagnosticReports
        (knownResultIds: Map<string, string>)
        (workspaceFolders: LspWorkspaceFolder list)
        : AsyncSeq<WorkspaceDocumentDiagnosticReport> =
        asyncSeq {
            let channel = Channel.CreateBounded<WorkspaceDiagnosticsReportsChannelItem>(256)

            let writeToChannel item =
                channel.Writer.WriteAsync(item) |> _.AsTask() |> Async.AwaitTask

            let generateProjectDiagnosticReports' wf (project: Microsoft.CodeAnalysis.Project) = async {
                let resultId = string project.Version

                // Collect URIs the client already holds for this exact project version.
                // If any exist, the client received the full set on a previous poll —
                // emit Unchanged for each and skip Roslyn entirely.
                let clientKnownUris =
                    knownResultIds
                    |> Map.toSeq
                    |> Seq.choose (fun (uri, knownId) -> if knownId = resultId then Some uri else None)
                    |> Array.ofSeq

                if clientKnownUris.Length > 0 then
                    for uri in clientKnownUris do
                        let documentReport: WorkspaceDocumentDiagnosticReport =
                            U2.C2
                                { Kind = "unchanged"
                                  ResultId = resultId
                                  Uri = uri
                                  Version = None }

                        do! writeToChannel (DiagnosticsReport documentReport)
                else
                    let! ct = Async.CancellationToken
                    let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

                    match compilation |> Option.ofObj with
                    | None -> ()
                    | Some compilation ->
                        let pathToUri path = workspaceFolderPathToUri path wf

                        let diagnosticsByDocument =
                            compilation.GetDiagnostics(ct)
                            |> Seq.filter (fun d ->
                                let uri = d.Location.GetMappedLineSpan().Path |> pathToUri
                                diagnosticIsToBeListed uri d)
                            |> Seq.map (Diagnostic.fromRoslynDiagnostic pathToUri)
                            |> Seq.groupBy snd

                        for uri, uriItems in diagnosticsByDocument do
                            let items = uriItems |> Seq.map fst |> Array.ofSeq

                            if items.Length > 0 then
                                let documentReport: WorkspaceDocumentDiagnosticReport =
                                    U2.C1
                                        { Kind = "full"
                                          ResultId = Some resultId
                                          Uri = uri
                                          Items = items
                                          Version = None }

                                do! writeToChannel (DiagnosticsReport documentReport)
            }

            let generateProjectDiagnosticReports wf (project: Microsoft.CodeAnalysis.Project) = async {
                try
                    do! generateProjectDiagnosticReports' wf project
                with _ ->
                    ()

                do! writeToChannel ReportingDoneForProject
            }

            // generate project diagnostics in parallel
            let mutable numProjectsBeingProcessed = 0

            for wf in workspaceFolders do
                let solutionProjects =
                    match wf.Solution with
                    | Ready(_, solution) -> solution.Projects |> List.ofSeq
                    | _ -> []

                numProjectsBeingProcessed <- numProjectsBeingProcessed + solutionProjects.Length

                for project in solutionProjects do
                    Async.Start(generateProjectDiagnosticReports wf project)

            // if there were no projects at all, complete the channel immediately so the consumer loop below doesn't hang
            if numProjectsBeingProcessed = 0 then
                channel.Writer.Complete()

            // eat from channel and yield diagnostic reports as they come
            while! channel.Reader.WaitToReadAsync() |> _.AsTask() |> Async.AwaitTask do
                let mutable item = ReportingDoneForProject

                while channel.Reader.TryRead(&item) do
                    match item with
                    | DiagnosticsReport r -> yield r
                    | ReportingDoneForProject ->
                        numProjectsBeingProcessed <- numProjectsBeingProcessed - 1

                        if numProjectsBeingProcessed = 0 then
                            channel.Writer.Complete()
        }

    let handleWorkspaceDiagnostic
        (context: RequestContext)
        (p: WorkspaceDiagnosticParams)
        : Async<LspResult<WorkspaceDiagnosticReport> * LspWorkspaceUpdate> =
        async {
            let knownResultIds =
                p.PreviousResultIds |> Seq.map (fun r -> r.Uri, r.Value) |> Map.ofSeq

            match p.PartialResultToken with
            | None ->
                let! workspaceFolders = context.GetWorkspaceFolderList(withSolutionReady = true)

                let! diagnosticReports =
                    getWorkspaceDiagnosticReports knownResultIds workspaceFolders
                    |> AsyncSeq.toArrayAsync

                let fullReport: WorkspaceDiagnosticReport = { Items = diagnosticReports }
                return fullReport |> LspResult.success, LspWorkspaceUpdate.Empty

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

                    do! context.LspClient.Progress(progressParams)
                }

                let! workspaceFolders = context.GetWorkspaceFolderList(withSolutionReady = true)

                do!
                    AsyncSeq.ofSeq (Seq.initInfinite id)
                    |> AsyncSeq.zip (getWorkspaceDiagnosticReports knownResultIds workspaceFolders)
                    |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

                let emptyReport: WorkspaceDiagnosticReport = { Items = Array.empty }
                return emptyReport |> LspResult.success, LspWorkspaceUpdate.Empty
        }
