module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Roslyn.Symbol
open CSharpLanguageServer.Roslyn.Conversions

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

let workspaceFolderResolveSymbolLocation
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (l: Microsoft.CodeAnalysis.Location)
    (folder: LspWorkspaceFolder)
    =
    async {
        match l.IsInMetadata, l.IsInSource with
        | true, _ ->
            let! ct = Async.CancellationToken
            let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

            let fullName =
                symbol |> symbolGetContainingTypeOrThis |> symbolGetFullReflectionName

            let containingAssemblyName =
                l.MetadataModule |> nonNull "l.MetadataModule" |> _.ContainingAssembly.Name

            let uri =
                $"csharp:/metadata/projects/{project.Name}/assemblies/{containingAssemblyName}/symbols/{fullName}.cs"

            let mdDocument, folder =
                match Map.tryFind uri folder.DecompiledMetadata with
                | Some value -> (value.Document, folder)
                | None ->
                    let (documentFromMd, text) = documentFromMetadata compilation project l fullName

                    let csharpMetadata =
                        { ProjectName = project.Name
                          AssemblyName = containingAssemblyName
                          SymbolName = fullName
                          Source = text }

                    let md =
                        { Metadata = csharpMetadata
                          Document = documentFromMd }

                    let updatedFolder =
                        { folder with
                            DecompiledMetadata = Map.add uri md folder.DecompiledMetadata }

                    (documentFromMd, updatedFolder)

            // figure out location on the document (approx implementation)
            let! syntaxTree = mdDocument.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, symbol)
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask
            collector.Visit(root)

            let fallbackLocationInMetadata =
                { Uri = uri
                  Range =
                    { Start = { Line = 0u; Character = 0u }
                      End = { Line = 0u; Character = 1u } } }

            return
                match collector.GetLocations() with
                | [] -> [ fallbackLocationInMetadata ], folder
                | ls -> ls, folder

        | false, true ->
            return
                match (Location.fromRoslynLocation l) with
                | Some loc -> [ loc ], folder
                | None -> [], folder

        | _, _ -> return [], folder
    }

/// The process of retrieving locations may update LspWorkspaceFolder itself,
/// thus return value is a pair of symbol location list * LspWorkspaceFolder
let workspaceFolderSymbolLocations
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (project: Microsoft.CodeAnalysis.Project)
    folder
    =
    async {
        let mutable wf = folder
        let mutable aggregatedLspLocations = []

        for l in symbol.Locations do
            let! symLspLocations, updatedWf = workspaceFolderResolveSymbolLocation project symbol l wf

            aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations
            wf <- updatedWf

        return aggregatedLspLocations, wf
    }

type LspWorkspaceOpenDocInfo = { Version: int; Touched: DateTime }

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      OpenDocs: Map<string, LspWorkspaceOpenDocInfo> }

    static member Empty = { Folders = []; OpenDocs = Map.empty }

    member this.SingletonFolder = this.Folders |> Seq.exactlyOne

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

let workspaceFolder (workspace: LspWorkspace) (uri: string) =
    workspace.Folders |> Seq.tryFind (fun wf -> uri.StartsWith wf.Uri)

let workspaceDocumentDetails (workspace: LspWorkspace) docType (u: string) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    let wf = workspaceFolder workspace u
    let solution = wf |> Option.bind _.Solution

    let docAndDocType =
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
                |> Option.map (fun x -> (x.Document, DecompiledDocument))

            match docType with
            | UserDocument -> matchingUserDocumentMaybe
            | DecompiledDocument -> matchingDecompiledDocumentMaybe
            | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

        | _, _ -> None

    wf, docAndDocType

let workspaceDocument workspace docType (u: string) =
    let wf, docAndType = workspaceDocumentDetails workspace docType u
    let doc = docAndType |> Option.map fst
    wf, doc

let workspaceDocumentSymbol workspace docType (uri: DocumentUri) (pos: Ionide.LanguageServerProtocol.Types.Position) = async {
    let wf, docForUri = uri |> workspaceDocument workspace AnyDocument

    match wf, docForUri with
    | Some wf, Some doc ->
        let! ct = Async.CancellationToken
        let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
        let position = Position.toRoslynPosition sourceText.Lines pos
        let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

        let symbolInfo =
            symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)

        return Some wf, symbolInfo

    | wf, _ -> return (wf, None)
}

let workspaceDocumentVersion workspace uri =
    uri |> workspace.OpenDocs.TryFind |> Option.map _.Version
