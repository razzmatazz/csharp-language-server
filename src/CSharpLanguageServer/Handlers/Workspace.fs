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
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types

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
        match context.GetUserDocument uri with
        | Some doc ->
            let fileText = uri |> Util.parseFileUri |> File.ReadAllText
            let updatedDoc = SourceText.From(fileText) |> doc.WithText

            context.Emit(SolutionChange updatedDoc.Project.Solution)

        | None ->
            let docFilePathMaybe = uri |> Util.tryParseFileUri

            match docFilePathMaybe with
            | Some docFilePath ->
                // ok, this document is not on solution, register a new one
                let fileText = docFilePath |> File.ReadAllText
                let! newDocMaybe = solutionTryAddDocument docFilePath fileText context.Solution

                match newDocMaybe with
                | Some newDoc -> context.Emit(SolutionChange newDoc.Project.Solution)
                | None -> ()
            | None -> ()
    }

    let private removeDocument (context: ServerRequestContext) uri =
        match context.GetUserDocument uri with
        | Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            context.Emit(SolutionChange updatedProject.Solution)
            context.Emit(OpenDocRemove uri)
        | None -> ()

    let didChangeWatchedFiles
        (context: ServerRequestContext)
        (p: DidChangeWatchedFilesParams)
        : Async<LspResult<unit>> =
        async {
            for change in p.Changes do
                match Path.GetExtension(change.Uri) with
                | ".csproj" ->
                    do! context.WindowShowMessage "change to .csproj detected, will reload solution"
                    context.Emit(SolutionReloadRequest(TimeSpan.FromSeconds(5: int64)))

                | ".sln"
                | ".slnx" ->
                    do! context.WindowShowMessage "change to .sln detected, will reload solution"
                    context.Emit(SolutionReloadRequest(TimeSpan.FromSeconds(5: int64)))

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
