module CSharpLanguageServer.Runtime.PushDiagnostics

open System
open System.Threading.Tasks

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

let private logger = Logging.getLoggerByName "Runtime.PushDiagnostics"

type PushDiagnosticsState =
    { DocumentBacklog: string list
      CurrentDocTask: (string * Task) option }

    static member Empty =
        { DocumentBacklog = []
          CurrentDocTask = None }

/// Rebuilds the backlog from all open documents, sorted by most-recently-touched first.
let handleBacklogUpdate (workspace: LspWorkspace) (state: PushDiagnosticsState) : PushDiagnosticsState =
    let newBacklog =
        workspace.OpenDocs
        |> Seq.sortByDescending (fun kv -> kv.Value.Touched)
        |> Seq.map (fun kv -> kv.Key)
        |> List.ofSeq

    { state with
        DocumentBacklog = newBacklog }

/// Pops the next document from the backlog and starts a background resolution task (if not already busy).
/// - `diagnosticPullSupported`: whether the client supports pull diagnostics (disables push when true)
/// - `postResolution`: callback to post the resolved diagnostics result back into the event loop
/// - `postProcessPending`: callback to chain the next ProcessPendingDocuments event
let handleProcessPending
    (workspace: LspWorkspace)
    (clientCapabilities: ClientCapabilities)
    (postResolution: Result<(string * int option * Diagnostic array), Exception> -> unit)
    (state: PushDiagnosticsState)
    : Async<PushDiagnosticsState> =
    async {
        match state.CurrentDocTask with
        | Some _ ->
            // another document is still being processed, do nothing
            return state
        | None ->
            // try to pull next doc from the backlog to process
            let nextDocUri, newBacklog =
                match state.DocumentBacklog with
                | [] -> (None, [])
                | uri :: remainder -> (Some uri, remainder)

            // push diagnostics is enabled only if pull diagnostics is
            // not reported to be supported by the client
            let diagnosticPullSupported =
                clientCapabilities.TextDocument
                |> Option.map _.Diagnostic
                |> Option.map _.IsSome
                |> Option.defaultValue false

            match diagnosticPullSupported, nextDocUri with
            | false, Some docUri ->
                let newState =
                    { state with
                        DocumentBacklog = newBacklog }

                let wf, docForUri = docUri |> workspaceDocument workspace AnyDocument
                let wfPathToUri = workspaceFolderPathToUri wf.Value

                match wf, docForUri with
                | Some wf, None ->
                    let cshtmlPath = workspaceFolderUriToPath wf docUri |> _.Value

                    match! solutionGetRazorDocumentForPath wf.Solution.Value cshtmlPath with
                    | Some(_, compilation, cshtmlTree) ->
                        let semanticModelMaybe = compilation.GetSemanticModel cshtmlTree |> Option.ofObj

                        match semanticModelMaybe with
                        | None -> Error(Exception "could not GetSemanticModelAsync") |> postResolution

                        | Some semanticModel ->
                            let diagnostics =
                                semanticModel.GetDiagnostics()
                                |> Seq.map (Diagnostic.fromRoslynDiagnostic (workspaceFolderPathToUri wf))
                                |> Seq.filter (fun (_, uri) -> uri = docUri)
                                |> Seq.map fst
                                |> Array.ofSeq

                            Ok(docUri, None, diagnostics) |> postResolution

                    | None ->
                        // could not find document for this enqueued uri
                        logger.LogDebug(
                            "handleProcessPending: could not find document w/ uri \"{docUri}\"",
                            string docUri
                        )

                        ()

                    return newState

                | Some wf, Some doc ->
                    let resolveDocumentDiagnostics () : Task = task {
                        let! semanticModelMaybe = doc.GetSemanticModelAsync()

                        match semanticModelMaybe |> Option.ofObj with
                        | None -> Error(Exception("could not GetSemanticModelAsync")) |> postResolution

                        | Some semanticModel ->
                            let diagnostics =
                                semanticModel.GetDiagnostics()
                                |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
                                |> Seq.map fst
                                |> Array.ofSeq

                            Ok(docUri, None, diagnostics) |> postResolution
                    }

                    let newTask = Task.Run(resolveDocumentDiagnostics)

                    return
                        { newState with
                            CurrentDocTask = Some(docUri, newTask) }

                | _, _ -> return newState

            | _, _ ->
                // backlog is empty or pull diagnostics is enabled instead -- nothing to do
                return state
    }

/// Clears the in-flight task, publishes diagnostics to the client, and chains the next process-pending cycle.
/// - `postProcessPending`: callback to chain the next ProcessPendingDocuments event
let handleResolution
    (lspClient: ILspClient option)
    (postProcessPending: unit -> unit)
    (result: Result<(string * int option * Diagnostic array), Exception>)
    (state: PushDiagnosticsState)
    : Async<PushDiagnosticsState> =
    async {
        // always chain: try to process the next doc in the backlog
        postProcessPending ()

        let newState = { state with CurrentDocTask = None }

        match result with
        | Error exn ->
            logger.LogDebug("handleResolution: {exn}", exn)
            return newState

        | Ok(docUri, version, diagnostics) ->
            match lspClient with
            | None -> return newState

            | Some lspClient ->
                let resolvedDocumentDiagnostics =
                    { Uri = docUri
                      Version = version
                      Diagnostics = diagnostics }

                do! lspClient.TextDocumentPublishDiagnostics(resolvedDocumentDiagnostics)
                return newState
    }
