namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

[<RequireQualifiedAccess>]
module CSharpMetadata =
    let handle
        (context: RequestContext)
        (p: CSharpMetadataParams)
        : Async<LspResult<CSharpMetadataResponse option> * LspWorkspaceUpdate> =
        async {
            let! ct = Async.CancellationToken

            let! wf, sln = p.TextDocument.Uri |> context.GetWorkspaceFolderReadySolution

            match wf, sln with
            | Some wf, Some solution ->
                let projectAndSymbolFromUri =
                    p.TextDocument.Uri
                    |> string
                    |> fun uri -> workspaceFolderParseMetadataSymbolSourceViewUri uri wf

                match projectAndSymbolFromUri with
                | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
                | Some(projectPath, symbolMetadataName) ->
                    let project = solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectPath)

                    match project with
                    | Some project ->
                        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
                        let symbol = compilation.GetTypeByMetadataName(symbolMetadataName) |> Option.ofObj

                        match symbol with
                        | Some symbol ->
                            let! symbolMetadata, wfUpdates = workspaceFolderDocumentFromMetadata project symbol wf

                            let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)
                            let lspResult = symbolMetadata.Metadata |> Some |> LspResult.success
                            return lspResult, wsUpdate

                        | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
                    | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
