namespace CSharpLanguageServer.Handlers

open System
open System.Threading.Channels

open FSharp.Control
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Runtime
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
        (acquireContext: ActivateServerRequest)
        (p: DocumentDiagnosticParams)
        : AsyncLspResult<DocumentDiagnosticReport> =
        async {
            Console.Error.WriteLine("Diagnostic.handle: p={0}", p)

            let! context = acquireContext ReadOnlySequential (Some p.TextDocument.Uri)

            Console.Error.WriteLine("Diagnostic.handle: context={0}", context)

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

                let diagnosticIsToBeListed (d: Microsoft.CodeAnalysis.Diagnostic) =
                    let documentIsCshtml = p.TextDocument.Uri.EndsWith(".cshtml")

                    match documentIsCshtml with
                    | true ->
                        // this particular diagnostic (CS8019: Unnecessary using directive.) does not
                        // have proper line mapping information and appears out of place on cshtml files
                        d.Id <> "CS8019"

                    | false -> true

                let diagnostics =
                    semanticModel.GetDiagnostics()
                    |> Seq.filter diagnosticIsToBeListed
                    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                    |> Seq.map fst
                    |> Array.ofSeq

                return { emptyReport with Items = diagnostics } |> U2.C1 |> LspResult.success

            | _, _ -> return emptyReport |> U2.C1 |> LspResult.success
        }

    type WorkspaceDiagnosticsReportsChannelItem =
        | DiagnosticsReport of WorkspaceDocumentDiagnosticReport
        | ReportingDoneForProject

    let private getWorkspaceDiagnosticReports (workspace: LspWorkspace) : AsyncSeq<WorkspaceDocumentDiagnosticReport> = asyncSeq {
        let channel = Channel.CreateBounded<WorkspaceDiagnosticsReportsChannelItem>(256)

        let generateProjectDiagnosticReports' wf (project: Microsoft.CodeAnalysis.Project) = async {
            let! ct = Async.CancellationToken

            let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

            match compilation |> Option.ofObj with
            | None -> ()
            | Some compilation ->
                let pathToUri = workspaceFolderPathToUri wf

                let diagnosticsByDocument =
                    compilation.GetDiagnostics(ct)
                    |> Seq.map (Diagnostic.fromRoslynDiagnostic pathToUri)
                    |> Seq.groupBy snd

                for uri, items in diagnosticsByDocument do
                    let items = items |> Seq.map fst |> Array.ofSeq

                    let fullDocumentReport: WorkspaceFullDocumentDiagnosticReport =
                        { Kind = "full"
                          ResultId = None
                          Uri = uri
                          Items = items
                          Version = None }

                    let documentReport: WorkspaceDocumentDiagnosticReport = U2.C1 fullDocumentReport

                    do!
                        channel.Writer.WriteAsync(DiagnosticsReport documentReport)
                        |> _.AsTask()
                        |> Async.AwaitTask
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

        for wf in workspace.Folders do
            let projectList =
                match wf.Solution with
                | Loaded sln -> sln.Projects |> List.ofSeq
                | _ -> List.empty

            numProjectsBeingProcessed <- numProjectsBeingProcessed + projectList.Length

            for project in projectList do
                Async.Start(generateProjectDiagnosticReports wf project)

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
        (acquireContext: ActivateServerRequest)
        (p: WorkspaceDiagnosticParams)
        : AsyncLspResult<WorkspaceDiagnosticReport> =
        async {
            let! context = acquireContext ReadOnlyBackground None

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

                    do! context.LspClient.Value.Progress progressParams
                }

                do!
                    AsyncSeq.ofSeq (Seq.initInfinite id)
                    |> AsyncSeq.zip (getWorkspaceDiagnosticReports context.Workspace)
                    |> AsyncSeq.iterAsync sendWorkspaceDiagnosticReport

                let emptyWorkspaceDiagnosticReport: WorkspaceDiagnosticReport =
                    { Items = Array.empty }

                return emptyWorkspaceDiagnosticReport |> LspResult.success
        }
