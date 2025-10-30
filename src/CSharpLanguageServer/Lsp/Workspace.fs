module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types

type LspWorkspaceDecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type LspWorkspaceFolder =
    { Uri: string
      Name: string option
      RoslynWorkspace: Workspace option
      Solution: Solution option
      DecompiledMetadata: Map<string, LspWorkspaceDecompiledMetadataDocument> }

    static member Empty =
        { Uri = Directory.GetCurrentDirectory() |> Uri.fromPath
          Name = None
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

    member this.RootPath = this.Folders |> Seq.exactlyOne |> _.Uri |> Uri.toPath

    member this.Solution = this.Folders |> Seq.tryExactlyOne |> Option.bind _.Solution

    member this.WithSingletonFolderUpdated(update: LspWorkspaceFolder -> LspWorkspaceFolder) =
        let updatedFolders =
            this.Folders
            |> Seq.tryExactlyOne
            |> Option.defaultValue LspWorkspaceFolder.Empty
            |> update
            |> List.singleton

        { this with Folders = updatedFolders }

    member this.WithRootPath(rootPath: string) =
        this.WithSingletonFolderUpdated(fun f ->
            { f with
                Uri = rootPath |> Uri.fromPath })

    member this.WithSolution(solution: Solution option) =
        this.WithSingletonFolderUpdated(fun f -> { f with Solution = solution })

let workspaceDocumentDetails (workspace: LspWorkspace) docType (u: string) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    match workspace.Solution with
    | Some solution ->
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
            workspace.SingletonFolder.DecompiledMetadata
            |> Map.tryFind u
            |> Option.map (fun x -> (x.Document, DecompiledDocument))

        match docType with
        | UserDocument -> matchingUserDocumentMaybe
        | DecompiledDocument -> matchingDecompiledDocumentMaybe
        | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe
    | None -> None

let workspaceDocument workspace docType u =
    workspaceDocumentDetails workspace docType u |> Option.map fst

let workspaceDocumentVersion workspace uri =
    uri |> workspace.OpenDocs.TryFind |> Option.map _.Version
