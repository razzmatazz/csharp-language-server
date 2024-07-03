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

    let dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.Workspace
        |> Option.bind (fun x -> x.DidChangeWatchedFiles)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let registration (clientCapabilities: ClientCapabilities): Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let fileSystemWatcher =
                { GlobPattern = U2.C1 "**/*.{cs,csproj,sln}"
                  Kind = Some (WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }

            let registerOptions: DidChangeWatchedFilesRegistrationOptions =
                { Watchers = [| fileSystemWatcher |] }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private tryReloadDocumentOnUri logger diagnosticsPost (context: ServerRequestContext) uri = async {
        match context.GetUserDocument uri with
        | Some doc ->
            let fileText = uri |> Util.parseFileUri |> File.ReadAllText
            let updatedDoc = SourceText.From(fileText) |> doc.WithText

            context.Emit(SolutionChange updatedDoc.Project.Solution)
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
                                                    context.Solution
                match newDocMaybe with
                | Some newDoc ->
                    context.Emit(SolutionChange newDoc.Project.Solution)
                    diagnosticsPost(DocumentBacklogUpdate)
                | None -> ()
            | None -> ()
    }

    let private removeDocument diagnosticsPost (context: ServerRequestContext) uri =
        match context.GetUserDocument uri with
        | Some existingDoc ->
            let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

            context.Emit(SolutionChange updatedProject.Solution)
            context.Emit(OpenDocRemove uri)

            diagnosticsPost(DocumentRemoval uri)
        | None -> ()

    let didChangeWatchedFiles (diagnosticsPost: DiagnosticsEvent -> unit)
                              (context: ServerRequestContext)
                              (p: DidChangeWatchedFilesParams)
            : Async<LspResult<unit>> = async {
        for change in p.Changes do
            match Path.GetExtension(change.Uri) with
            | ".csproj" ->
                do! context.WindowShowMessage "change to .csproj detected, will reload solution"
                context.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".sln" ->
                do! context.WindowShowMessage "change to .sln detected, will reload solution"
                context.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    do! tryReloadDocumentOnUri logger diagnosticsPost context change.Uri
                | FileChangeType.Changed ->
                    do! tryReloadDocumentOnUri logger diagnosticsPost context change.Uri
                | FileChangeType.Deleted ->
                    do removeDocument diagnosticsPost context change.Uri
                | _ -> ()

            | _ -> ()

        return Ok()
    }

    let didChangeConfiguration (context: ServerRequestContext)
                               (configParams: DidChangeConfigurationParams)
            : Async<LspResult<unit>> = async {
        let csharpSettings =
            configParams.Settings
            |> deserialize<ServerSettingsDto>
            |> (fun x -> x.csharp)
            |> Option.defaultValue ServerSettingsCSharpDto.Default

        let newServerSettings = { context.State.Settings with SolutionPath = csharpSettings.solution }
        context.Emit(SettingsChange newServerSettings)

        return Ok()
    }
