namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Debug =
    let private toDebugWorkspaceFolderInfo (wf: LspWorkspaceFolder) : DebugWorkspaceFolderInfo =
        let solutionState, loadedSolutionPath =
            match wf.Solution with
            | Uninitialized -> "Uninitialized", None
            | Loading _ -> "Loading", None
            | Ready(_, solution) -> "Ready", Some solution.FilePath
            | Defunct _ -> "Defunct", None

        { uri = wf.Uri
          name = wf.Name
          solutionState = solutionState }

    let private makeDebugInfo (ws: LspWorkspace) : DebugInfo =
        { workspace =
            { phase = ws.Phase |> string
              folders = ws.Folders |> List.map toDebugWorkspaceFolderInfo } }

    let handle (context: RequestContext) (_: unit) : Async<LspResult<DebugInfo option> * LspWorkspaceUpdate> = async {
        let debugMode =
            context.Config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        if not debugMode then
            return LspResult.success None, LspWorkspaceUpdate.Empty
        else
            let! ws = context.GetWorkspaceSnapshot()
            let response = makeDebugInfo ws
            return LspResult.success (Some response), LspWorkspaceUpdate.Empty
    }
