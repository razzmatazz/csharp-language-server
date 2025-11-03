namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace


[<RequireQualifiedAccess>]
module Workspace =
    let private logger = Logging.getLoggerByName "Workspace"

    let provider (_: ClientCapabilities) : ServerCapabilitiesWorkspace option =
        { WorkspaceFolders = None
          FileOperations = None }
        |> Some


    let dynamicRegistrationForDidChangeWatchedFiles (clientCapabilities: ClientCapabilities) =
        clientCapabilities.Workspace
        |> Option.bind _.DidChangeWatchedFiles
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false


    let didChangeWatchedFilesRegistration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistrationForDidChangeWatchedFiles clientCapabilities with
        | false -> None
        | true ->
            let fileSystemWatcher =
                { GlobPattern = U2.C1 "**/*.{cs,csproj,sln,slnx}"
                  Kind = Some(WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }

            let registerOptions: DidChangeWatchedFilesRegistrationOptions =
                { Watchers = [| fileSystemWatcher |] }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = registerOptions |> serialize |> Some }


    let private tryReloadDocumentOnUri logger (context: ServerRequestContext) uri = async {
        let wf, doc = uri |> workspaceDocument context.Workspace UserDocument

        match wf, doc with
        | Some wf, Some doc ->
            let fileText = uri |> Util.parseFileUri |> File.ReadAllText
            let updatedDoc = SourceText.From(fileText) |> doc.WithText

            let updatedWf =
                { wf with
                    Solution = Some updatedDoc.Project.Solution }

            context.Emit(WorkspaceFolderChange updatedWf)

        | Some wf, None ->
            let docFilePathMaybe = uri |> Util.tryParseFileUri

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
        async {
            for change in p.Changes do
                match Path.GetExtension(change.Uri) with
                | ".csproj" ->
                    do! context.WindowShowMessage "change to .csproj detected, will reload solution"
                    context.Emit(WorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64)))

                | ".sln"
                | ".slnx" ->
                    do! context.WindowShowMessage "change to .sln detected, will reload solution"
                    context.Emit(WorkspaceReloadRequested(TimeSpan.FromSeconds(5: int64)))

                | ".cs" ->
                    match change.Type with
                    | FileChangeType.Created -> do! tryReloadDocumentOnUri logger context change.Uri
                    | FileChangeType.Changed -> do! tryReloadDocumentOnUri logger context change.Uri
                    | FileChangeType.Deleted -> do removeDocument context change.Uri
                    | _ -> ()

                | _ -> ()

            return Ok()
        }


    let didChangeConfiguration
        (context: ServerRequestContext)
        (configParams: DidChangeConfigurationParams)
        : Async<LspResult<unit>> =
        async {

            let csharpSettings =
                configParams.Settings
                |> deserialize<ServerSettingsDto>
                |> (fun x -> x.csharp)
                |> Option.defaultValue ServerSettingsCSharpDto.Default

            let newServerSettings =
                { context.State.Settings with
                    SolutionPath = csharpSettings.solution
                    ApplyFormattingOptions = csharpSettings.applyFormattingOptions |> Option.defaultValue false }

            context.Emit(SettingsChange newServerSettings)

            return Ok()
        }
