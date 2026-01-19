namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.Util
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
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

    let didChangeWatchedFilesRegistration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
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

    let private tryReloadDocumentOnUri logger (context: ServerRequestContext) uri = async {
        let wf, doc = uri |> workspaceDocument context.Workspace UserDocument

        match wf, doc with
        | Some wf, Some doc ->
            let docFilePathMaybe = uri |> workspaceFolderUriToPath wf

            match docFilePathMaybe with
            | Some docFilePath ->
                let fileText = docFilePath |> File.ReadAllText
                let updatedDoc = SourceText.From(fileText) |> doc.WithText

                let updatedWf =
                    { wf with
                        Solution = Some updatedDoc.Project.Solution }

                context.Emit(WorkspaceFolderChange updatedWf)
            | None -> ()

        | Some wf, None ->
            let docFilePathMaybe = uri |> workspaceFolderUriToPath wf

            match docFilePathMaybe with
            | Some docFilePath ->
                // ok, this document is not on solution, register a new one
                let fileText = docFilePath |> File.ReadAllText
                let! newDocMaybe = solutionTryAddDocument docFilePath fileText wf.Solution.Value

                match newDocMaybe with
                | Some newDoc ->
                    let updatedWf =
                        { wf with
                            Solution = Some newDoc.Project.Solution }

                    context.Emit(WorkspaceFolderChange updatedWf)
                | None -> ()
            | None -> ()

        | _, _ -> ()
    }

    let private removeDocument (context: ServerRequestContext) uri =
        let wf, doc = uri |> workspaceDocument context.Workspace UserDocument

        match wf, doc with
        | Some wf, Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            let updatedWf =
                { wf with
                    Solution = Some updatedProject.Solution }

            context.Emit(WorkspaceFolderChange updatedWf)
            context.Emit(DocumentClosed uri)

        | _, _ -> ()

    let didChangeWatchedFiles
        (context: ServerRequestContext)
        (p: DidChangeWatchedFilesParams)
        : Async<LspResult<unit>> =

        let windowShowMessage (m: string) =
            match context.State.LspClient with
            | Some lspClient ->
                lspClient.WindowShowMessage(
                    { Type = MessageType.Info
                      Message = sprintf "csharp-ls: %s" m }
                )
            | None -> async.Return()

        async {
            for change in p.Changes do
                match Path.GetExtension(change.Uri) with
                | ".csproj" ->
                    do! windowShowMessage "change to .csproj detected, will reload solution"
                    context.Emit(WorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64)))

                | ".sln"
                | ".slnx" ->
                    do! windowShowMessage "change to .sln(x) detected, will reload solution"
                    context.Emit(WorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64)))

                | ".cs" ->
                    match change.Type with
                    | FileChangeType.Created -> do! tryReloadDocumentOnUri logger context change.Uri
                    | FileChangeType.Changed -> do! tryReloadDocumentOnUri logger context change.Uri
                    | FileChangeType.Deleted -> do removeDocument context change.Uri
                    | _ -> ()

                | ".cshtml" ->
                    // TODO: handle this
                    ()

                | _ -> ()

            return Ok()
        }

    let didChangeConfiguration
        (context: ServerRequestContext)
        (configParams: DidChangeConfigurationParams)
        : Async<LspResult<unit>> =
        async {
            let csharpSettingsMaybe =
                configParams.Settings
                |> deserialize<DidChangeConfigurationSettingsDto>
                |> _.csharp

            match csharpSettingsMaybe with
            | None -> ()
            | Some csharpSettings ->
                let prevSettings = context.State.Settings

                let newSettings =
                    applyCSharpSectionConfigurationOnSettings prevSettings csharpSettings

                context.Emit(SettingsChange newSettings)

            return Ok()
        }

    let didChangeWorkspaceFolders
        (context: ServerRequestContext)
        (p: DidChangeWorkspaceFoldersParams)
        : Async<LspResult<unit>> =
        async {
            let wfNotInRemovedList (wf: WorkspaceFolder) : bool =
                p.Event.Removed |> Seq.exists (fun r -> r.Uri = wf.Uri) |> not

            let updatedWorkspaceFolders =
                context.State.Workspace.Folders
                |> Seq.map (fun wf -> { Name = wf.Name; Uri = wf.Uri })
                |> Seq.filter wfNotInRemovedList
                |> Seq.append p.Event.Added
                |> List.ofSeq

            context.Emit(WorkspaceConfigurationChanged updatedWorkspaceFolders)

            context.Emit(WorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64)))

            return Ok()
        }
