namespace CSharpLanguageServer.Workspace

open System.Collections.Concurrent
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.MSBuild
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FSharpPlus

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Workspace.Util

type WorkspaceManager(lspClient: ILspClient) =

    let workspaces: ConcurrentDictionary<DocumentUri, Workspace> =
        ConcurrentDictionary()

    let logger = LogProvider.getLoggerByName "WorkspaceManager"

    member private this.loadWorkspace (workspace: MSBuildWorkspace) (uri: DocumentUri) : Async<unit> = async {
        workspace.LoadMetadataForReferencedProjects <- true

        let root = Uri.toPath uri

        match findInteresting root with
        | None ->
            // TODO: Create a sln/csproj file so that it can be used for single cs file?
            logger.info (
                Log.setMessage "Can't find .sln/.csproj files under {path}"
                >> Log.addContext "path" root
            )
        | Some(Solution sln) ->
            logger.info (Log.setMessage "Start to load {slnPath}..." >> Log.addContext "slnPath" sln)
            // TODO: Report progress to client?
            do! workspace.OpenSolutionAsync(sln) |> Async.AwaitTask |> Async.Ignore
            logger.info (Log.setMessage "Finish to load {slnPath}" >> Log.addContext "slnPath" sln)
        | Some(Projects projs) ->
            logger.info (
                Log.setMessage "Start to load {projNum} projects..."
                >> Log.addContext "projNum" projs.Length
            )

            do!
                projs
                |> map (fun proj -> async {
                    logger.info (Log.setMessage "Start to load {proj}..." >> Log.addContext "proj" proj)

                    try
                        do! workspace.OpenProjectAsync(proj) |> Async.AwaitTask |> Async.Ignore
                    with ex ->
                        logger.info (
                            Log.setMessage "Exception during loading {proj}:"
                            >> Log.addContext "proj" proj
                            >> Log.addException ex
                        )

                    logger.info (Log.setMessage "Finish to load {proj}" >> Log.addContext "proj" proj)
                })
                |> Async.Parallel
                |> Async.Ignore

            logger.info (
                Log.setMessage "Finish to load {projNum} projects"
                >> Log.addContext "projNum" projs.Length
            )
    }

    member this.ChangeWorkspaceFolders (added: WorkspaceFolder[]) (removed: WorkspaceFolder[]) : Async<unit> = async {
        // TODO: Add a lock to provide atomic? A group of atomic operations is not atomic.
        removed
        |> map (fun workspaceFolder -> workspaceFolder.Uri)
        |> map workspaces.TryRemove
        |> ignore

        do!
            added
            |> map (fun add -> async {
                let workspace = MSBuildWorkspace.Create()
                workspaces.[add.Uri] <- workspace
                do! this.loadWorkspace workspace add.Uri
            })
            |> Async.Parallel
            |> Async.Ignore
    }

    interface IWorkspaceManager with
        override this.ChangeWorkspaceFolders added removed =
            this.ChangeWorkspaceFolders added removed

        override this.Initialize(workspaceFolders: WorkspaceFolder list) =
            MSBuildLocator.RegisterDefaults() |> ignore

            this.ChangeWorkspaceFolders (List.toArray workspaceFolders) Array.empty |> Async.Start
