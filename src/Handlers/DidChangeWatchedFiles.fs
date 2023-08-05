namespace CSharpLanguageServer.Handlers

open System
open System.IO
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module DidChangeWatchedFiles =
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
                { GlobPattern = Pattern "**/*.{cs,csproj,sln}"
                  Kind = Some (WatchKind.Create ||| WatchKind.Change ||| WatchKind.Delete) }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = { Watchers = [| fileSystemWatcher |] } |> serialize |> Some }

    let handle (wm: IWorkspaceManager) (p: DidChangeWatchedFilesParams): Async<unit> = async {
        for change in p.Changes do
            match Path.GetExtension(change.Uri) with
            | ".sln" -> wm.OnSolutionUpdate change.Uri change.Type
            | ".csproj" -> wm.OnProjectUpdate change.Uri change.Type
            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    wm.SaveDocument change.Uri None |> Async.Start
                | FileChangeType.Deleted ->
                    wm.RemoveDocument change.Uri |> Async.Start
                | _ -> ()
            | _ -> ()
    }
