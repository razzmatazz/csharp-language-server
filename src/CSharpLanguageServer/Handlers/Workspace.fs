namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Workspace =
    let private logger = LogProvider.getLoggerByName "Workspace"

    let dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.Workspace)
        |> Option.bind (fun x -> x.DidChangeWatchedFiles)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let registration (clientCapabilities: ClientCapabilities option): Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let fileSystemWatcher =
                // TODO: Change it to U2 after Ionide.LanguageServerProtocol release a new version.
                { GlobPattern = U2.First "**/*.{cs,csproj,sln}"
                  Kind = Some (WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = { Watchers = [| fileSystemWatcher |] } |> serialize |> Some }

    let private tryReloadDocumentOnUri logger diagnosticsPost (scope: ServerRequestScope) uri = async {
        match scope.GetUserDocument uri with
        | Some doc ->
            let fileText = uri |> Util.parseFileUri |> File.ReadAllText
            let updatedDoc = SourceText.From(fileText) |> doc.WithText

            scope.Emit(SolutionChange updatedDoc.Project.Solution)
            diagnosticsPost(DocumentBacklogUpdate)

        | None ->
            let docFilePathMaybe = uri |> Util.tryParseFileUri
            match docFilePathMaybe with
            | Some docFilePath ->
                // ok, this document is not on solution, register a new one
                let fileText = docFilePath |> File.ReadAllText
                let! newDocMaybe = tryAddDocument logger
                                                    docFilePath
                                                    fileText
                                                    scope.Solution
                match newDocMaybe with
                | Some newDoc ->
                    scope.Emit(SolutionChange newDoc.Project.Solution)
                    diagnosticsPost(DocumentBacklogUpdate)
                | None -> ()
            | None -> ()
    }

    let private removeDocument diagnosticsPost (scope: ServerRequestScope) uri =
        match scope.GetUserDocument uri with
        | Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            scope.Emit(SolutionChange updatedProject.Solution)
            scope.Emit(OpenDocVersionRemove uri)

            diagnosticsPost(DocumentRemoval uri)
        | None -> ()

    let didChangeWatchedFiles (diagnosticsPost: DiagnosticsEvent -> unit)
                              (scope: ServerRequestScope)
                              (p: DidChangeWatchedFilesParams)
            : Async<LspResult<unit>> = async {
        for change in p.Changes do
            match Path.GetExtension(change.Uri) with
            | ".csproj" ->
                do! scope.WindowShowMessage "change to .csproj detected, will reload solution"
                scope.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".sln" ->
                do! scope.WindowShowMessage "change to .sln detected, will reload solution"
                scope.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    do! tryReloadDocumentOnUri logger diagnosticsPost scope change.Uri
                | FileChangeType.Changed ->
                    do! tryReloadDocumentOnUri logger diagnosticsPost scope change.Uri
                | FileChangeType.Deleted ->
                    do removeDocument diagnosticsPost scope change.Uri
                | _ -> ()

            | _ -> ()

        return Ok()
    }

    let didChangeConfiguration (scope: ServerRequestScope)
                               (configParams: DidChangeConfigurationParams)
            : Async<LspResult<unit>> = async {
        let csharpSettings =
            configParams.Settings
            |> deserialize<ServerSettingsDto>
            |> (fun x -> x.csharp)
            |> Option.defaultValue ServerSettingsCSharpDto.Default

        let newServerSettings = { scope.State.Settings with SolutionPath = csharpSettings.solution }
        scope.Emit(SettingsChange newServerSettings)

        return Ok()
    }
