module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "Lsp.Workspace"

type LspWorkspaceDecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type LspWorkspaceFolder =
    { Uri: string
      Name: string
      RoslynWorkspace: Workspace option
      Solution: Solution option
      DecompiledMetadata: Map<string, LspWorkspaceDecompiledMetadataDocument> }

    static member Empty =
        { Uri = Directory.GetCurrentDirectory() |> Uri.fromPath
          Name = "(no name)"
          RoslynWorkspace = None
          Solution = None
          DecompiledMetadata = Map.empty }

type LspWorkspaceDocumentType =
    | UserDocument // user Document from solution, on disk
    | DecompiledDocument // Document decompiled from metadata, readonly
    | AnyDocument

let workspaceFolderWithDecompiledMetadataAdded uri md folder =
    let newDecompiledMd = Map.add uri md folder.DecompiledMetadata

    { folder with
        DecompiledMetadata = newDecompiledMd }

type LspWorkspaceOpenDocInfo = { Version: int; Touched: DateTime }

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      OpenDocs: Map<string, LspWorkspaceOpenDocInfo> }

    static member Empty = { Folders = []; OpenDocs = Map.empty }

    member this.SingletonFolder = this.Folders |> Seq.exactlyOne

    member this.Solution = this.Folders |> Seq.tryExactlyOne |> Option.bind _.Solution

    member this.WithSingletonFolderUpdated(update: LspWorkspaceFolder -> LspWorkspaceFolder) =
        let updatedFolders =
            this.Folders
            |> Seq.tryExactlyOne
            |> Option.defaultValue LspWorkspaceFolder.Empty
            |> update
            |> List.singleton

        { this with Folders = updatedFolders }

    member this.WithSolution(solution: Solution option) =
        this.WithSingletonFolderUpdated(fun f -> { f with Solution = solution })

let workspaceFrom (workspaceFolders: WorkspaceFolder list) =
    // TODO: currently only the first workspace folder is taken into account (see Seq.take 1)
    match workspaceFolders.Length with
    | 0 -> failwith "workspaceFrom: at least 1 workspace folder must be provided!"
    | 1 -> ()
    | _ -> logger.LogWarning("workspaceFrom: only the first WorkspaceFolder will be loaded!")

    let folders =
        workspaceFolders
        |> Seq.take 1
        |> Seq.map (fun f ->
            { LspWorkspaceFolder.Empty with
                Uri = f.Uri
                Name = f.Name })
        |> List.ofSeq

    { LspWorkspace.Empty with
        Folders = folders }

let workspaceFolder (workspace: LspWorkspace) _uri = Some workspace.SingletonFolder

let workspaceDocumentDetails (workspace: LspWorkspace) docType (u: string) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    let wf = workspaceFolder workspace uri
    let solution = wf |> Option.bind _.Solution

    match wf, solution with
    | Some wf, Some solution ->
        let matchingUserDocuments =
            solution.Projects
            |> Seq.collect (fun p -> p.Documents)
            |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri)
            |> List.ofSeq

        let matchingUserDocumentMaybe =
            match matchingUserDocuments with
            | [ d ] -> Some(d, UserDocument)
            | _ -> None

        let matchingDecompiledDocumentMaybe =
            wf.DecompiledMetadata
            |> Map.tryFind u
            |> Option.map (fun x -> x.Document, DecompiledDocument)

        match docType with
        | UserDocument -> matchingUserDocumentMaybe
        | DecompiledDocument -> matchingDecompiledDocumentMaybe
        | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

    | _, _ -> None

let workspaceDocument workspace docType u =
    workspaceDocumentDetails workspace docType u |> Option.map fst

let workspaceDocumentVersion workspace uri =
    uri |> workspace.OpenDocs.TryFind |> Option.map _.Version
