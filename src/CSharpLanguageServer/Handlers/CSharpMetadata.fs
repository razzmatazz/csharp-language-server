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

                match workspaceFolderParseCSharpDocumentUri (string p.TextDocument.Uri) wf with
                | UnrecognizedDocumentUri -> return None |> LspResult.success, LspWorkspaceUpdate.Empty

                // --- Path A: source-generated document ---
                | GeneratedDocumentUri(projectFilePath, hintName) ->
                    let project =
                        solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectFilePath)

                    match project with
                    | Some project ->
                        let! generatedDocs = project.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask
                        let doc = generatedDocs |> Seq.tryFind (fun d -> d.HintName = hintName)

                        match doc with
                        | Some doc ->
                            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                            let metadata: CSharpMetadataInformation =
                                { ProjectName = project.Name
                                  AssemblyName = project.AssemblyName
                                  SymbolName = hintName
                                  Source = sourceText.ToString() }

                            return Some metadata |> LspResult.success, LspWorkspaceUpdate.Empty

                        | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
                    | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty

                // --- Path B: decompiled metadata symbol ---
                | DecompiledDocumentUri(projectFilePath, symbolMetadataName) ->
                    let project =
                        solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectFilePath)

                    match project with
                    | Some project ->
                        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
                        let symbol = compilation.GetTypeByMetadataName(symbolMetadataName) |> Option.ofObj

                        match symbol with
                        | Some symbol ->
                            let! symbolMetadata, wfUpdates =
                                workspaceFolderDecompiledDocumentFromMetadata project symbol wf

                            let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)
                            let lspResult = symbolMetadata.Metadata |> Some |> LspResult.success
                            return lspResult, wsUpdate

                        | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
                    | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
