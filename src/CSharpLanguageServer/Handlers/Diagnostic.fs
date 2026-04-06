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

    let handle (context: RequestContext) (p: DocumentDiagnosticParams) : AsyncLspResult<DocumentDiagnosticReport> = async {
        let emptyReport: RelatedFullDocumentDiagnosticReport =
            { Kind = "full"
              ResultId = None
              Items = [||]
              RelatedDocuments = None }

        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        match wf with
        | None -> return emptyReport |> U2.C1 |> LspResult.success
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

                return { emptyReport with Items = diagnostics } |> U2.C1 |> LspResult.success

            | None -> return emptyReport |> U2.C1 |> LspResult.success
    }

    type WorkspaceDiagnosticsReportsChannelItem =
        | DiagnosticsReport of WorkspaceDocumentDiagnosticReport
        | ReportingDoneForProject

    let private getWorkspaceDiagnosticReports
        (context: RequestContext)
        (knownResultIds: Map<string, string>)
        (workspaceFolders: LspWorkspaceFolder list)
        : AsyncSeq<WorkspaceDocumentDiagnosticReport> =
        asyncSeq {

            let channel = Channel.CreateBounded<WorkspaceDiagnosticsReportsChannelItem>(256)

            let generateProjectDiagnosticReports' wf (project: Microsoft.CodeAnalysis.Project) = async {
                let! ct = Async.CancellationToken

                let projectKey = project.FilePath
                let cachedEntry = wf.DiagnosticsCacheByProject |> Map.tryFind projectKey

                match cachedEntry with
                | Some cached when cached.Version = project.Version ->
                    // Cache hit — skip all Roslyn work and emit directly from cache
                    for KeyValue(uri, (resultId, _items)) in cached.ByUri do
                        let documentReport: WorkspaceDocumentDiagnosticReport =
                            U2.C2
                                { Kind = "unchanged"
                                  ResultId = resultId
                                  Uri = uri
                                  Version = None }

                        do!
                            channel.Writer.WriteAsync(DiagnosticsReport documentReport)
                            |> _.AsTask()
                            |> Async.AwaitTask

                | _ ->
                    // Cache miss or stale — recompute from Roslyn
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

                        let mutable newByUri = Map.empty

                        let resultId = string project.Version

                        for uri, uriItems in diagnosticsByDocument do
                            let items = uriItems |> Seq.map fst |> Array.ofSeq

                            // always store in cache (even empty — so we know we checked)
                            newByUri <- newByUri |> Map.add uri (resultId, items)

                            let documentReportOpt: WorkspaceDocumentDiagnosticReport option =
                                match Map.tryFind uri knownResultIds with
                                | Some knownId when knownId = resultId ->
                                    Some(
                                        U2.C2
                                            { Kind = "unchanged"
                                              ResultId = resultId
                                              Uri = uri
                                              Version = None }
                                    )
                                | _ when items.Length > 0 ->
                                    Some(
                                        U2.C1
                                            { Kind = "full"
                                              ResultId = Some resultId
                                              Uri = uri
                                              Items = items
                                              Version = None }
                                    )
                                | _ -> None

                            match documentReportOpt with
                            | Some documentReport ->
                                do!
                                    channel.Writer.WriteAsync(DiagnosticsReport documentReport)
                                    |> _.AsTask()
                                    |> Async.AwaitTask
                            | None -> ()

                        // persist new cache entry for this project
                        let newEntry =
                            { Version = project.Version
                              ByUri = newByUri }

                        context.PostFolderCacheUpdate(wf.Uri, wf.Generation, fun m -> m |> Map.add projectKey newEntry)
            }

            let generateProjectDiagnosticReports wf (project: Microsoft.CodeAnalysis.Project) = async {
                try
                    do! generateProjectDiagnosticReports' wf project
                with _ ->
                    ()

                do!
                    channel.Writer.WriteAsync(ReportingDoneForProject)
                    |> _.AsTask()
                    |> Async.AwaitTask
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
        : AsyncLspResult<WorkspaceDiagnosticReport> =
        async {
            let knownResultIds =
                p.PreviousResultIds |> Seq.map (fun r -> r.Uri, r.Value) |> Map.ofSeq

            match p.PartialResultToken with
            | None ->
                let! workspaceFolders = context.GetWorkspaceFolderList(withSolutionReady = true)

                let! diagnosticReports =
                    getWorkspaceDiagnosticReports context knownResultIds workspaceFolders
                    |> AsyncSeq.toArrayAsync

                let fullReport: WorkspaceDiagnosticReport = { Items = diagnosticReports }
                return fullReport |> LspResult.success

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
                    |> AsyncSeq.zip (getWorkspaceDiagnosticReports context knownResultIds workspaceFolders)
                    |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

                let emptyReport: WorkspaceDiagnosticReport = { Items = Array.empty }
                return emptyReport |> LspResult.success
        }
