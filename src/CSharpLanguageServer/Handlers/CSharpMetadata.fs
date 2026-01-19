namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

[<RequireQualifiedAccess>]
module CSharpMetadata =
    let handle
        (context: ServerRequestContext)
        (p: CSharpMetadataParams)
        : AsyncLspResult<CSharpMetadataResponse option> =
        async {
            let! ct = Async.CancellationToken

            let wf = p.TextDocument.Uri |> workspaceFolder context.Workspace

            match wf with
            | Some wf ->
                let projectAndSymbolFromUri =
                    p.TextDocument.Uri
                    |> string
                    |> workspaceFolderParseMetadataSymbolSourceViewUri wf

                match wf.Solution, projectAndSymbolFromUri with
                | Some solution, Some(projectPath, symbolMetadataName) ->
                    let project = solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectPath)

                    match project with
                    | Some project ->
                        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
                        let symbol = compilation.GetTypeByMetadataName(symbolMetadataName) |> Option.ofObj

                        match symbol with
                        | Some symbol ->
                            let! updatedWf, symbolMetadata = workspaceFolderWithDocumentFromMetadata wf project symbol

                            context.Emit(WorkspaceFolderChange updatedWf)

                            return symbolMetadata.Metadata |> Some |> LspResult.success

                        | None -> return None |> LspResult.success
                    | None -> return None |> LspResult.success
                | _, _ -> return None |> LspResult.success
            | None -> return None |> LspResult.success
        }
