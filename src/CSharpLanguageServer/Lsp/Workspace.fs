module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging
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

let workspaceFolderUriToPath (_wf: LspWorkspaceFolder) (uri: string) : string =
    Uri.UnescapeDataString(Uri(Uri.unescape uri).LocalPath)

let workspaceFolderPathToUri (_wf: LspWorkspaceFolder) (path: string) : string =
    let metadataPrefix = "$metadata$/"

    if path.StartsWith metadataPrefix then
        "csharp:/metadata/" + path.Substring metadataPrefix.Length
    else
        Uri(path).ToString()

let workspaceFolderMetadataUriBase (wf: LspWorkspaceFolder) =
    wf.Uri
    |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
    |> _.TrimEnd('/')
    |> sprintf "csharp:/%s/$metadata$"

let workspaceFolderMetadataUri wf projectName containingAssemblyName symbolFullName =
    sprintf
        "%s/projects/%s/assemblies/%s/symbols/%s.cs"
        (workspaceFolderMetadataUriBase wf)
        projectName
        containingAssemblyName
        symbolFullName

let documentFromMetadata
    (project: Microsoft.CodeAnalysis.Project)
    (containingAssembly: Microsoft.CodeAnalysis.IAssemblySymbol)
    (fullName: string)
    =
    async {
        let! ct = Async.CancellationToken
        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

        let reference =
            compilation.GetMetadataReference containingAssembly
            |> nonNull "compilation.GetMetadataReference(containingAssembly)"

        let peReference = reference :?> PortableExecutableReference |> Option.ofObj

        let assemblyLocation =
            peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

        let decompilerSettings = DecompilerSettings()
        decompilerSettings.ThrowOnAssemblyResolveErrors <- false // this shouldn't be a showstopper for us

        let decompiler = CSharpDecompiler(assemblyLocation, decompilerSettings)

        // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
        // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
        decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

        let fullTypeName = TypeSystem.FullTypeName fullName

        let text = decompiler.DecompileTypeAsString fullTypeName

        let mdDocumentFilename =
            $"$metadata$/projects/{project.Name}/assemblies/{containingAssembly.Name}/symbols/{fullName}.cs"

        let mdDocumentEmpty = project.AddDocument(mdDocumentFilename, String.Empty)

        let mdDocument = SourceText.From text |> mdDocumentEmpty.WithText
        return mdDocument, text
    }

let workspaceFolderWithDocumentFromMetadata
    wf
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (l: Microsoft.CodeAnalysis.Location)
    =
    async {
        let containingAssembly: IAssemblySymbol =
            l.MetadataModule |> nonNull "l.MetadataModule" |> _.ContainingAssembly

        let symbolGetContainingTypeOrThis (symbol: Microsoft.CodeAnalysis.ISymbol) =
            match symbol with
            | :? INamedTypeSymbol as namedType -> namedType
            | _ -> symbol.ContainingType

        let symbolFullName =
            symbol |> symbolGetContainingTypeOrThis |> symbolGetFullReflectionName

        let metadataUri =
            workspaceFolderMetadataUri wf project.Name containingAssembly.Name symbolFullName

        match Map.tryFind metadataUri wf.DecompiledMetadata with
        | Some md -> return wf, metadataUri, md.Document
        | None ->
            let! documentFromMd, text = documentFromMetadata project containingAssembly symbolFullName

            let csharpMetadata =
                { ProjectName = project.Name
                  AssemblyName = containingAssembly.Name
                  SymbolName = symbolFullName
                  Source = text }

            let md =
                { Metadata = csharpMetadata
                  Document = documentFromMd }

            let updatedFolder =
                { wf with
                    DecompiledMetadata = Map.add metadataUri md wf.DecompiledMetadata }

            return updatedFolder, metadataUri, documentFromMd
    }

let workspaceFolderResolveSymbolLocation
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (l: Microsoft.CodeAnalysis.Location)
    (wf: LspWorkspaceFolder)
    =
    async {
        match l.IsInMetadata, l.IsInSource with
        | true, _ ->
            let! ct = Async.CancellationToken

            let! (updatedWf, metadataUri, mdDocument) = workspaceFolderWithDocumentFromMetadata wf project symbol l

            // figure out location on the document (approx implementation)
            let! syntaxTree = mdDocument.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForMatchingSymbolName(metadataUri, symbol)
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask
            collector.Visit(root)

            let fallbackLocationInMetadata =
                { Uri = metadataUri
                  Range =
                    { Start = { Line = 0u; Character = 0u }
                      End = { Line = 0u; Character = 1u } } }

            return
                match collector.GetLocations() with
                | [] -> [ fallbackLocationInMetadata ], updatedWf
                | ls -> ls, updatedWf

        | false, true ->
            return
                match (Location.fromRoslynLocation l) with
                | Some loc -> [ loc ], wf
                | None -> [], wf

        | _, _ -> return [], wf
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
    let workspaceFolderMatchesUri wf =
        uri.StartsWith wf.Uri || uri.StartsWith(workspaceFolderMetadataUriBase wf)

    workspace.Folders |> Seq.tryFind workspaceFolderMatchesUri

let workspaceWithFolder (workspace: LspWorkspace) (updatedWf: LspWorkspaceFolder) =
    let existingW = workspace.Folders |> Seq.tryFind (fun wf -> wf.Uri = updatedWf.Uri)

    let updatedFolders =
        match existingW with
        | Some existingWf ->
            let replaceByUri wf =
                if wf.Uri = existingWf.Uri then updatedWf else wf

            workspace.Folders |> List.map replaceByUri
        | None -> workspace.Folders @ [ updatedWf ]

    { workspace with
        Folders = updatedFolders }

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
