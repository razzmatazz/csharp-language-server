namespace CSharpLanguageServer.Handlers

open System
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module Workspace =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.Workspace)
        |> Option.bind (fun x -> x.DidChangeWatchedFiles)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let fileChangeWatcher = { GlobPattern = U2.First "**/*.{cs,csproj,sln}"
                                      Kind = None }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = { Watchers = [| fileChangeWatcher |] } |> serialize |> Some
                }

    let didChangeWatchedFiles (logMessage: Util.AsyncLogFn)
                              (diagnosticsPost: DiagnosticsEvent -> unit)
                              (scope: ServerRequestScope)
                              (changeParams: DidChangeWatchedFilesParams)
            : Async<LspResult<unit>> = async {
        let tryReloadDocumentOnUri uri = async {
            match scope.GetDocumentForUriOfType UserDocument uri with
            | Some (doc, _) ->
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
                    let! newDocMaybe = tryAddDocument logMessage
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

        for change in changeParams.Changes do
            match Path.GetExtension(change.Uri) with
            | ".csproj" ->
                do! logMessage "change to .csproj detected, will reload solution"
                scope.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".sln" ->
                do! logMessage "change to .sln detected, will reload solution"
                scope.Emit(SolutionReloadRequest (TimeSpan.FromSeconds(5)))

            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Changed ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Deleted ->
                    match scope.GetDocumentForUriOfType UserDocument change.Uri with
                    | Some (existingDoc, _) ->
                        let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

                        scope.Emit(SolutionChange updatedProject.Solution)
                        scope.Emit(OpenDocVersionRemove change.Uri)

                        diagnosticsPost(DocumentRemoval change.Uri)
                    | None -> ()
                | _ -> ()

            | _ -> ()

        return LspResult.Ok()
    }

    let didChangeConfiguration (scope: ServerRequestScope)
                               (configParams: DidChangeConfigurationParams)
            : Async<LspResult<unit>> =
        async {
            let csharpSettings =
                configParams.Settings
                |> deserialize<ServerSettingsDto>
                |> (fun x -> x.csharp)
                |> Option.defaultValue ServerSettingsCSharpDto.Default

            let newServerSettings = { scope.State.Settings with SolutionPath = csharpSettings.solution }
            scope.Emit(SettingsChange newServerSettings)

            return LspResult.Ok ()
        }
