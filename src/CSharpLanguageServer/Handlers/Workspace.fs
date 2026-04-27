namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis.Text
open Microsoft.Extensions.Logging

open CSharpLanguageServer
open CSharpLanguageServer.Util
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Client
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
                { GlobPattern = U2.C1 "**/*.{cs,cshtml,csproj,sln,slnx}"
                  Kind = Some(WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }

            let registerOptions: DidChangeWatchedFilesRegistrationOptions =
                { Watchers = [| fileSystemWatcher |] }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private tryReloadDocumentOnUri logger wf uri : LspWorkspaceFolderUpdateFn list =
        let doc = wf |> workspaceFolderDocument UserDocument uri
        let docFilePathMaybe = wf |> workspaceFolderUriToPath uri

        match doc with
        | Some doc ->
            let docFilePathMaybe = workspaceFolderUriToPath uri wf

            match docFilePathMaybe with
            | None -> []
            | Some docFilePath ->
                let updateWf =
                    let fileText = docFilePath |> File.ReadAllText
                    let updatedDoc = fileText |> SourceText.From |> doc.WithText
                    workspaceFolderLoadedSolutionReplaced updatedDoc.Project.Solution

                [ updateWf ]

        | None ->
            match docFilePathMaybe with
            | None -> []
            | Some docFilePath ->
                // ok, this document is not on solution, register a new one
                let fileText = docFilePath |> File.ReadAllText

                let _, wfUpdates = wf |> workspaceFolderDocumentAdd docFilePath fileText

                wfUpdates

    let private removeDocument wf uri =
        let doc = wf |> workspaceFolderDocument UserDocument uri

        match doc with
        | None -> []
        | Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            let updateWf = workspaceFolderLoadedSolutionReplaced updatedProject.Solution
            let updateWf2 = workspaceFolderDocClosed uri

            [ updateWf; updateWf2 ]

    let private tryReloadCshtmlDocumentOnUri wf uri : LspWorkspaceFolderUpdateFn list =
        match workspaceFolderUriToPath uri wf with
        | None -> []
        | Some cshtmlPath ->
            let fileText = File.ReadAllText cshtmlPath
            let newSourceText = SourceText.From(fileText, Text.Encoding.UTF8)

            match workspaceFolderAdditionalTextDocumentForPath cshtmlPath wf with
            | Some doc ->
                // AdditionalDocument already in solution — update its text
                [ workspaceFolderAdditionalTextDocumentTextUpdated doc newSourceText ]
            | None ->
                // Not yet registered — add it
                let _, wfUpdates = workspaceFolderAdditionalTextDocumentAdd cshtmlPath fileText wf
                wfUpdates

    let private removeCshtmlDocument wf uri : LspWorkspaceFolderUpdateFn list =
        match workspaceFolderUriToPath uri wf with
        | None -> []
        | Some cshtmlPath ->
            match workspaceFolderAdditionalTextDocumentForPath cshtmlPath wf with
            | None -> []
            | Some _ -> [ workspaceFolderAdditionalDocumentRemoved uri ]

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

            let mutable currentWfByUri: Map<string, LspWorkspaceFolder> = Map.empty

            for change in p.Changes do
                let! wf, _ = context.GetWorkspaceFolderReadySolution(change.Uri)

                match wf, Path.GetExtension(change.Uri) with
                | Some wf, ".csproj" ->
                    do! windowShowMessage "change to .csproj detected, will reload solution"
                    wsUpdate <- wsUpdate.WithReloadRequested(TimeSpan.FromSeconds(5: int64))

                | Some wf, ".sln"
                | Some wf, ".slnx" ->
                    do! windowShowMessage "change to .sln(x) detected, will reload solution"
                    wsUpdate <- wsUpdate.WithReloadRequested(TimeSpan.FromSeconds(5: int64))

                | Some wf, ".cs" ->
                    let currentWf = currentWfByUri |> Map.tryFind wf.Uri |> Option.defaultValue wf

                    let wfUpdates =
                        match change.Type with
                        | FileChangeType.Created -> tryReloadDocumentOnUri logger currentWf change.Uri
                        | FileChangeType.Changed -> tryReloadDocumentOnUri logger currentWf change.Uri
                        | FileChangeType.Deleted -> removeDocument currentWf change.Uri
                        | _ -> []

                    let updatedWf = wfUpdates |> List.fold (|>) currentWf
                    currentWfByUri <- currentWfByUri |> Map.add wf.Uri updatedWf

                    wsUpdate <- wsUpdate.WithFolderUpdates(wf.Uri, wfUpdates)

                | Some wf, ".cshtml" ->
                    let currentWf = currentWfByUri |> Map.tryFind wf.Uri |> Option.defaultValue wf

                    let wfUpdates =
                        match change.Type with
                        | FileChangeType.Created -> tryReloadCshtmlDocumentOnUri currentWf change.Uri
                        | FileChangeType.Changed -> tryReloadCshtmlDocumentOnUri currentWf change.Uri
                        | FileChangeType.Deleted -> removeCshtmlDocument currentWf change.Uri
                        | _ -> []

                    let updatedWf = wfUpdates |> List.fold (|>) currentWf
                    currentWfByUri <- currentWfByUri |> Map.add wf.Uri updatedWf

                    wsUpdate <- wsUpdate.WithFolderUpdates(wf.Uri, wfUpdates)

                | _, _ -> ()

            return Ok(), wsUpdate
        }

    let didChangeConfiguration
        (context: RequestContext)
        (configParams: DidChangeConfigurationParams)
        : Async<LspResult<unit> * LspWorkspaceUpdate> =
        async {
            let pushedConfig =
                configParams.Settings
                |> Option.ofObj
                |> Option.bind deserialize<DidChangeConfigurationSettingsDto option>
                |> Option.map _.csharp
                |> Option.bind id // flatten option option, also guards against null from Newtonsoft

            let configurationSupported =
                context.ClientCapabilities.Workspace
                |> Option.bind _.Configuration
                |> Option.defaultValue false

            // When Settings is null the client expects us to pull the config via
            // workspace/configuration (if supported), rather than push it in the notification.
            let! pulledConfig = async {
                match pushedConfig, configurationSupported with
                | None, true -> return! CSharpLspClient.TryPullCSharpConfig context.LspClient
                | _ -> return None
            }

            let wsUpdate =
                match pushedConfig |> Option.orElse pulledConfig with
                | None -> LspWorkspaceUpdate.Empty
                | Some csharpSettings ->
                    let newConfig = mergeCSharpConfiguration context.Config csharpSettings

                    logger.LogInformation(
                        "didChangeConfiguration: csharp config updated: {config}",
                        newConfig |> string
                    )

                    LspWorkspaceUpdate.Empty.WithConfigurationChange(newConfig)

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

            return Ok(), LspWorkspaceUpdate.Empty.WithFolderReconfiguration(updatedWorkspaceFolders)
        }
