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


type LspWorkspaceDocumentType =
    | UserDocument // user Document from solution, on disk
    | DecompiledDocument // Document decompiled from metadata, readonly
    | AnyDocument


let workspaceFolderWithDecompiledMetadataAdded uri md folder =
    let newDecompiledMd = Map.add uri md folder.DecompiledMetadata

    { folder with
        DecompiledMetadata = newDecompiledMd }


type LspWorkspace =
    { Folders: LspWorkspaceFolder list }

    static member Empty = { Folders = [] }

    member this.SingletonFolder = this.Folders |> Seq.exactlyOne

    member this.RootPath = this.Folders |> Seq.exactlyOne |> _.Uri |> Uri.toPath

    member this.Solution = this.Folders |> Seq.tryExactlyOne |> Option.bind _.Solution

    member this.WithSingletonFolderUpdated(update: LspWorkspaceFolder -> LspWorkspaceFolder) =
        let emptyFolder =
            { Uri = Directory.GetCurrentDirectory() |> Uri.fromPath
              Name = None
              RoslynWorkspace = None
              Solution = None
              DecompiledMetadata = Map.empty }

        let updatedFolders =
            this.Folders
            |> Seq.tryExactlyOne
            |> Option.defaultValue emptyFolder
            |> update
            |> List.singleton

        { this with Folders = updatedFolders }

    member this.WithRootPath(rootPath: string) =
        this.WithSingletonFolderUpdated(fun f ->
            { f with
                Uri = rootPath |> Uri.fromPath })

    member this.WithSolution(solution: Solution option) =
        this.WithSingletonFolderUpdated(fun f -> { f with Solution = solution })
