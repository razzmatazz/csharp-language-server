namespace CSharpLanguageServer.Handlers

open System
open System.Threading.Channels

open FSharp.Control
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Roslyn.Analyzers
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

    let private projectResultId (analyzersEnabled: bool) (project: Microsoft.CodeAnalysis.Project) =
        sprintf "%s/%b" (string project.Version) analyzersEnabled

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

                    let project =
                        workspaceFolderDocument AnyDocument p.TextDocument.Uri wf
                        |> Option.map _.Project

                    let analyzersEnabled = context.Config.analyzersEnabled |> Option.defaultValue false

                    let! allDiags =
                        match project, analyzersEnabled with
                        | Some project, true -> getDocumentDiagnosticsWithAnalyzers project semanticModel
                        | _ -> async {
                            let diags = semanticModel.GetDiagnostics(cancellationToken = ct)
                            return diags |> List.ofSeq
                          }

                    let diagnostics =
                        allDiags
                        |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
                        |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                        |> Seq.map fst
                        |> Array.ofSeq

                    // Populate resultId so that VS Code's diagnostic client can include it in
                    // previousResultIds on the next workspace/diagnostic poll.  Without this,
                    // VS Code substitutes the document-level pull state (which has resultId=None)
                    // over the workspace pull state for any open file, causing previousResultIds
                    // to be empty on every poll and triggering a perpetual full re-scan.
                    let resultId = project |> Option.map (projectResultId analyzersEnabled)

                    return
                        { emptyReport with
                            Items = diagnostics
                            ResultId = resultId }
                        |> U2.C1
                        |> LspResult.success,
                        LspWorkspaceUpdate.Empty

                | None -> return emptyReport |> U2.C1 |> LspResult.success, LspWorkspaceUpdate.Empty
        }

    type WorkspaceDiagnosticsReportsChannelItem =
        | DiagnosticsReport of WorkspaceDocumentDiagnosticReport
        | ReportingDoneForProject

    let private getWorkspaceDiagnosticReports
        (config: CSharpConfiguration)
        (knownResultIds: Map<string, string>)
        (workspaceFolders: LspWorkspaceFolder list)
        : AsyncSeq<WorkspaceDocumentDiagnosticReport> =
        asyncSeq {
            let channel = Channel.CreateBounded<WorkspaceDiagnosticsReportsChannelItem>(256)

            let writeToChannel item =
                channel.Writer.WriteAsync(item) |> _.AsTask() |> Async.AwaitTask

            let generateProjectDiagnosticReports' wf (project: Microsoft.CodeAnalysis.Project) = async {
                let analyzersEnabled = config.analyzersEnabled |> Option.defaultValue false
                // Include analyzersEnabled in the resultId so that toggling the setting
                // invalidates any cached "unchanged" result the client holds.
                let resultId = project |> projectResultId analyzersEnabled

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

                        // Collect file paths of source-generated documents (IIncrementalGenerator
                        // output) so we can suppress their diagnostics. Users have no control over
                        // generated code, so surfacing those diagnostics would only cause noise.
                        let! generatedDocs = project.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask

                        let sourceGeneratedFilePaths =
                            generatedDocs
                            |> Seq.choose (fun (d: Microsoft.CodeAnalysis.SourceGeneratedDocument) ->
                                d.FilePath |> Option.ofObj)
                            |> Set.ofSeq

                        // MSBuild also injects auto-generated files (AssemblyInfo.cs,
                        // AssemblyAttributes.cs, …) that live under the project's obj/
                        // directory as ordinary Documents.  Users cannot edit those either.
                        let sep = string System.IO.Path.DirectorySeparatorChar
                        let isUnderObjDir (path: string) = path.Contains(sep + "obj" + sep)

                        let! allDiags =
                            if analyzersEnabled then
                                getCompilationDiagnosticsWithAnalyzers project compilation
                            else
                                async { return compilation.GetDiagnostics() |> List.ofSeq }

                        let diagnosticsByDocument =
                            allDiags
                            |> Seq.filter (fun d ->
                                let path = d.Location.GetMappedLineSpan().Path
                                not (sourceGeneratedFilePaths.Contains(path)) && not (isUnderObjDir path))
                            |> Seq.filter (fun d ->
                                let uri = d.Location.GetMappedLineSpan().Path |> pathToUri
                                diagnosticIsToBeListed uri d)
                            |> Seq.map (Diagnostic.fromRoslynDiagnostic pathToUri)
                            |> Seq.groupBy snd
                            |> Map.ofSeq

                        // Emit full reports for every URI that has diagnostics.
                        for KeyValue(uri, uriItems) in diagnosticsByDocument do
                            let items = uriItems |> Seq.map fst |> Array.ofSeq

                            let documentReport: WorkspaceDocumentDiagnosticReport =
                                U2.C1
                                    { Kind = "full"
                                      ResultId = Some resultId
                                      Uri = uri
                                      Items = items
                                      Version = None }

                            do! writeToChannel (DiagnosticsReport documentReport)

                        // Also emit an explicit empty full report for any URI the client
                        // previously knew about (via previousResultIds) that now has zero
                        // diagnostics.  Without this, VS Code never clears stale diagnostics
                        // for documents that become clean after a config change (e.g. toggling
                        // analyzersEnabled off removes analyzer diagnostics from project2 but
                        // the server previously emitted nothing for it, leaving the old count).
                        let clientKnownUrisForProject =
                            knownResultIds
                            |> Map.toSeq
                            |> Seq.map fst
                            |> Seq.filter (fun uri -> not (diagnosticsByDocument.ContainsKey uri))

                        for uri in clientKnownUrisForProject do
                            let documentReport: WorkspaceDocumentDiagnosticReport =
                                U2.C1
                                    { Kind = "full"
                                      ResultId = Some resultId
                                      Uri = uri
                                      Items = [||]
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
                    | Loaded(_, solution) -> solution.Projects |> List.ofSeq
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
                    getWorkspaceDiagnosticReports context.Config knownResultIds workspaceFolders
                    |> AsyncSeq.toArrayAsync

                let fullReport: WorkspaceDiagnosticReport = { Items = diagnosticReports }
                return fullReport |> LspResult.success, LspWorkspaceUpdate.Empty

            | Some partialResultToken ->
                let mutable sentReports: WorkspaceDocumentDiagnosticReport list = []

                let sendWorkspaceDiagnosticReport (documentReport, index) = async {
                    sentReports <- documentReport :: sentReports

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
                    |> AsyncSeq.zip (getWorkspaceDiagnosticReports context.Config knownResultIds workspaceFolders)
                    |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

                // Echo back an unchanged-kind stub for every document we streamed via
                // $/progress. This gives the client the uri+resultId pairs it needs to
                // populate previousResultIds on the next poll, breaking the busy loop
                // where previousResultIds was always empty.
                let stubItems =
                    sentReports
                    |> List.rev
                    |> Array.ofList
                    |> Array.map (fun r ->
                        match r with
                        | U2.C1 full ->
                            U2.C2
                                { Kind = "unchanged"
                                  ResultId = full.ResultId |> Option.defaultValue ""
                                  Uri = full.Uri
                                  Version = None }
                        | U2.C2 unchanged -> U2.C2 unchanged)

                let finalReport: WorkspaceDiagnosticReport = { Items = stubItems }
                return finalReport |> LspResult.success, LspWorkspaceUpdate.Empty
        }
