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
          solutionPathOverride = wf.SolutionPathOverride
          solutionState = solutionState
          loadedSolutionPath = loadedSolutionPath
          openDocumentUris = wf.OpenDocs |> Map.keys |> List.ofSeq }

    let handle (context: RequestContext) (_: unit) : Async<LspResult<DebugStateResponse option> * LspWorkspaceUpdate> = async {
        let debugMode =
            context.Config.debug |> Option.bind _.debugMode |> Option.defaultValue false

        if not debugMode then
            return LspResult.success None, LspWorkspaceUpdate.Empty
        else
            let! wfs = context.GetWorkspaceFolderList(withSolutionReady = false)

            let response = { workspaceFolders = wfs |> List.map toDebugWorkspaceFolderInfo }

            return LspResult.success (Some response), LspWorkspaceUpdate.Empty
    }
