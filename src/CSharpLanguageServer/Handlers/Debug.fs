namespace CSharpLanguageServer.Handlers

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

/// The `$/csharp/debugInfo` endpoint bypasses the normal request-scheduling
/// pipeline entirely. `Server.fs` posts a `GetDebugInfo` event directly to the
/// state actor (which always replies regardless of queue mode or workspace
/// phase) and then calls `Debug.handle` with the raw config + workspace pair.
[<RequireQualifiedAccess>]
module Debug =
    let private toDebugWorkspaceFolderInfo (wf: LspWorkspaceFolder) : DebugWorkspaceFolderInfo =
        let solutionState, _solutionFilePath =
            match wf.Solution with
            | Uninitialized -> "Uninitialized", None
            | Loading -> "Loading", None
            | Loaded(_, solution) -> "Loaded", Some solution.FilePath
            | Defunct _ -> "Defunct", None

        { uri = wf.Uri
          name = wf.Name
          solutionState = solutionState }

    let handle (config: CSharpConfiguration) (ws: LspWorkspace) : DebugInfo option =
        let debugMode = config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        if not debugMode then
            None
        else
            Some
                { workspace =
                    { phase = "(None)"
                      folders = ws.Folders |> List.map toDebugWorkspaceFolderInfo } }
