namespace CSharpLanguageServer.Handlers

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types
open CSharpLanguageServer.Runtime.RequestScheduling

/// The `$/csharp/debugInfo` endpoint bypasses the normal request-scheduling
/// pipeline entirely. `Server.fs` posts a `GetDebugInfo` event directly to the
/// state actor (which always replies regardless of queue mode or workspace
/// phase) and then calls `Debug.handle` with the raw config + workspace pair.
[<RequireQualifiedAccess>]
module Debug =
    let private toDebugWorkspaceFolderInfo (wf: LspWorkspaceFolder) : DebugWorkspaceFolderInfo =
        let solutionState =
            match wf.Solution with
            | Uninitialized -> "Uninitialized"
            | Loading -> "Loading"
            | Loaded _ -> "Loaded"
            | Defunct _ -> "Defunct"

        { uri = wf.Uri
          name = wf.Name
          solutionState = solutionState }

    let private toDebugRequestInfo (ordinal: int64) (r: RequestInfo) : DebugRequestInfo =
        let mode =
            match r.Mode with
            | ReadOnly -> "ReadOnly"
            | ReadWrite -> "ReadWrite"
            | ReadOnlyBackground -> "ReadOnlyBackground"

        let phase =
            match r.Phase with
            | Pending -> "Pending"
            | Running -> "Running"
            | Finished -> "Finished"

        { ordinal = ordinal
          name = r.Name
          mode = mode
          phase = phase }

    let handle (config: CSharpConfiguration) (ws: LspWorkspace) (rq: RequestQueue) : DebugInfo option =
        let debugMode = config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        if not debugMode then
            None
        else
            let queueMode =
                match rq.Mode with
                | Dispatching -> "Dispatching"
                | DrainingUpTo ord -> $"DrainingUpTo({ord})"

            Some
                { workspace =
                    { phase = ws.Phase |> string
                      folders = ws.Folders |> List.map toDebugWorkspaceFolderInfo }
                  requestQueue =
                    { mode = queueMode
                      requests = rq.Requests |> Map.toList |> List.map (fun (ord, r) -> toDebugRequestInfo ord r) } }
