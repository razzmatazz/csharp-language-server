namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

[<RequireQualifiedAccess>]
module CSharpMetadata =
    let handle (context: RequestContext) (p: CSharpMetadataParams) : AsyncLspResult<CSharpMetadataResponse option> = async {
        let! ct = Async.CancellationToken

        let! wf, sln = p.TextDocument.Uri |> context.GetWorkspaceFolderReadySolution

        match wf, sln with
        | Some wf, Some solution ->
            let projectAndSymbolFromUri =
                p.TextDocument.Uri
                |> string
                |> workspaceFolderParseMetadataSymbolSourceViewUri wf

            match projectAndSymbolFromUri with
            | None -> return None |> LspResult.success
            | Some(projectPath, symbolMetadataName) ->
                let project = solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectPath)

                match project with
                | Some project ->
                    let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
                    let symbol = compilation.GetTypeByMetadataName(symbolMetadataName) |> Option.ofObj

                    match symbol with
                    | Some symbol ->
                        let! updatedWf, symbolMetadata = workspaceFolderWithDocumentFromMetadata wf project symbol

                        context.UpdateEffects(_.WithWorkspaceFolderChange(updatedWf))

                        return symbolMetadata.Metadata |> Some |> LspResult.success

                    | None -> return None |> LspResult.success
                | None -> return None |> LspResult.success

        | _, _ -> return None |> LspResult.success
    }
