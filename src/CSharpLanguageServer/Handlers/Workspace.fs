namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.Util
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

[<RequireQualifiedAccess>]
module Workspace =
    let private logger = Logging.getLoggerByName "Workspace"

    let provider (_cc: ClientCapabilities) : ServerCapabilitiesWorkspace option =
        { WorkspaceFolders =
            Some
                { Supported = Some true
                  ChangeNotifications = U2.C2 true |> Some }
          FileOperations = None }
        |> Some

    let dynamicRegistrationForDidChangeWatchedFiles (cc: ClientCapabilities) =
        cc.Workspace
        |> Option.bind _.DidChangeWatchedFiles
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let didChangeWatchedFilesRegistration
        (_config: CSharpConfiguration)
        (cc: ClientCapabilities)
        : Registration option =
        match dynamicRegistrationForDidChangeWatchedFiles cc with
        | false -> None
        | true ->
            let fileSystemWatcher =
                { GlobPattern = U2.C1 "**/*.{cs,csproj,sln,slnx}"
                  Kind = Some(WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }

            let registerOptions: DidChangeWatchedFilesRegistrationOptions =
                { Watchers = [| fileSystemWatcher |] }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private tryReloadDocumentOnUri logger (context: RequestContext) uri : Async<LspWorkspaceUpdate> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(uri)

        let doc = wf |> Option.bind (workspaceFolderDocument UserDocument uri)

        match wf, doc with
        | Some wf, Some doc ->
            let docFilePathMaybe = workspaceFolderUriToPath uri wf

            match docFilePathMaybe with
            | Some docFilePath ->
                let fileText = docFilePath |> File.ReadAllText
                let updatedDoc = fileText |> SourceText.From |> doc.WithText

                let updatedWf =
                    workspaceFolderWithReadySolutionReplaced updatedDoc.Project.Solution wf

                return LspWorkspaceUpdate.Empty.WithWorkspaceFolderChange(updatedWf)
            | None -> return LspWorkspaceUpdate.Empty

        | Some wf, None ->
            let docFilePathMaybe = workspaceFolderUriToPath uri wf

            match docFilePathMaybe with
            | Some docFilePath ->
                // ok, this document is not on solution, register a new one
                let fileText = docFilePath |> File.ReadAllText

                let! updatedWf, newDocMaybe = workspaceFolderWithDocumentAdded docFilePath fileText wf

                match newDocMaybe with
                | Some _ -> return LspWorkspaceUpdate.Empty.WithWorkspaceFolderChange(updatedWf)
                | None -> return LspWorkspaceUpdate.Empty
            | None -> return LspWorkspaceUpdate.Empty

        | _, _ -> return LspWorkspaceUpdate.Empty
    }

    let private removeDocument (context: RequestContext) uri : Async<LspWorkspaceUpdate> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(uri)

        let doc = wf |> Option.bind (workspaceFolderDocument UserDocument uri)

        match wf, doc with
        | Some wf, Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            let updatedWf = workspaceFolderWithReadySolutionReplaced updatedProject.Solution wf

            return LspWorkspaceUpdate.Empty.WithWorkspaceFolderChange(updatedWf).WithDocumentClosed(uri)

        | _, _ -> return LspWorkspaceUpdate.Empty
    }

    let didChangeWatchedFiles
        (context: RequestContext)
        (p: DidChangeWatchedFilesParams)
        : Async<LspResult<unit> * LspWorkspaceUpdate> =

        let windowShowMessage (m: string) =
            context.LspClient.WindowShowMessage(
                { Type = MessageType.Info
                  Message = sprintf "csharp-ls: %s" m }
            )

        async {
            let mutable wsUpdate = LspWorkspaceUpdate.Empty

            for change in p.Changes do
                match Path.GetExtension(change.Uri) with
                | ".csproj" ->
                    do! windowShowMessage "change to .csproj detected, will reload solution"
                    wsUpdate <- wsUpdate.WithWorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64))

                | ".sln"
                | ".slnx" ->
                    do! windowShowMessage "change to .sln(x) detected, will reload solution"
                    wsUpdate <- wsUpdate.WithWorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64))

                | ".cs" ->
                    match change.Type with
                    | FileChangeType.Created ->
                        let! e = tryReloadDocumentOnUri logger context change.Uri

                        wsUpdate <-
                            { wsUpdate with
                                WorkspaceFolderChange = wsUpdate.WorkspaceFolderChange @ e.WorkspaceFolderChange }
                    | FileChangeType.Changed ->
                        let! e = tryReloadDocumentOnUri logger context change.Uri

                        wsUpdate <-
                            { wsUpdate with
                                WorkspaceFolderChange = wsUpdate.WorkspaceFolderChange @ e.WorkspaceFolderChange }
                    | FileChangeType.Deleted ->
                        let! e = removeDocument context change.Uri

                        wsUpdate <-
                            { wsUpdate with
                                WorkspaceFolderChange = wsUpdate.WorkspaceFolderChange @ e.WorkspaceFolderChange
                                DocumentClosed = wsUpdate.DocumentClosed @ e.DocumentClosed }
                    | _ -> ()

                | ".cshtml" ->
                    // TODO: handle this
                    ()

                | _ -> ()

            return Ok(), wsUpdate
        }

    let didChangeConfiguration
        (context: RequestContext)
        (configParams: DidChangeConfigurationParams)
        : Async<LspResult<unit> * LspWorkspaceUpdate> =
        async {
            let csharpSettingsMaybe =
                configParams.Settings
                |> deserialize<DidChangeConfigurationSettingsDto>
                |> _.csharp

            let wsUpdate =
                match csharpSettingsMaybe with
                | None -> LspWorkspaceUpdate.Empty
                | Some csharpSettings ->
                    let newConfig = mergeCSharpConfiguration context.Config csharpSettings
                    LspWorkspaceUpdate.Empty.WithSettingsChange(newConfig)

            return Ok(), wsUpdate
        }

    let didChangeWorkspaceFolders
        (context: RequestContext)
        (p: DidChangeWorkspaceFoldersParams)
        : Async<LspResult<unit> * LspWorkspaceUpdate> =
        async {
            let wfNotInRemovedList (wf: WorkspaceFolder) : bool =
                p.Event.Removed |> Seq.exists (fun r -> r.Uri = wf.Uri) |> not

            let! workspaceFolders = context.GetWorkspaceFolderList(withSolutionReady = false)

            let updatedWorkspaceFolders =
                workspaceFolders
                |> Seq.map (fun wf -> { Name = wf.Name; Uri = wf.Uri })
                |> Seq.filter wfNotInRemovedList
                |> Seq.append p.Event.Added
                |> List.ofSeq

            return Ok(), LspWorkspaceUpdate.Empty.WithWorkspaceConfigurationChanged(updatedWorkspaceFolders)
        }
