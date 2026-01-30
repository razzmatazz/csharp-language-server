module CSharpLanguageServer.Lsp.WorkspaceFolder

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Roslyn.Symbol
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Logging

let logger = Logging.getLoggerByName "WorkspaceFolder"

type LspWorkspaceDecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type LspWorkspaceFolder =
    {
        Uri: string
        Name: string
        RoslynWorkspace: Workspace option
        Solution: Solution option

        /// key is (project.Path * symbol metadata name)
        DecompiledSymbolMetadata: Map<string * string, LspWorkspaceDecompiledMetadataDocument>
    }

    static member Empty =
        { Uri = Directory.GetCurrentDirectory() |> Uri |> string
          Name = "(no name)"
          RoslynWorkspace = None
          Solution = None
          DecompiledSymbolMetadata = Map.empty }

type LspWorkspaceDocumentType =
    | UserDocument // user Document from solution, on disk
    | DecompiledDocument // Document decompiled from metadata, readonly
    | AnyDocument

// Unescape some necessary char before passing string to Uri.
// Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
// ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
/// "#/ProjDir" is Fragment instead of part of LocalPath.
let workspaceFolderUriUnescape (_wf: LspWorkspaceFolder) (uri: string) : string = uri.Replace("%3a", ":", true, null)

let workspaceFolderUriToPath (wf: LspWorkspaceFolder) (uri: string) : option<string> =
    try
        uri
        |> workspaceFolderUriUnescape wf
        |> Uri
        |> _.LocalPath
        |> Uri.UnescapeDataString
        |> Some
    with _ex ->
        None

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
    |> sprintf "csharp:/%s"

let workspaceFolderMetadataSymbolSourceViewUri
    _wf
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    =
    let projectFile =
        project.FilePath
        |> Uri
        |> string
        |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
        |> _.TrimEnd('/')

    let symbolMetadataName = symbolGetMetadataName symbol

    sprintf "csharp:/%s/decompiled/%s.cs" projectFile (Uri.EscapeDataString symbolMetadataName)

let workspaceFolderParseMetadataSymbolSourceViewUri (_wf: LspWorkspaceFolder) (uri: string) : option<string * string> =
    let uri = uri |> Uri

    match uri.Scheme with
    | "csharp" ->
        let path = uri.LocalPath

        let decompiledSourcePathPrefix =
            sprintf ".csproj%sdecompiled%s" (string Path.DirectorySeparatorChar) (string Path.DirectorySeparatorChar)

        match path.IndexOf decompiledSourcePathPrefix with
        | -1 -> None
        | idx ->
            let projectFile = path.Substring(0, idx + ".csproj".Length)

            let symbolMetadataName =
                path
                |> _.Substring(idx + decompiledSourcePathPrefix.Length)
                |> _.TrimEnd(".cs".ToCharArray())
                |> Uri.UnescapeDataString

            Some(projectFile, symbolMetadataName)
    | _ -> None

let documentFromMetadata
    (project: Microsoft.CodeAnalysis.Project)
    (containingAssembly: Microsoft.CodeAnalysis.IAssemblySymbol)
    (symbolMetadataName: string)
    =
    async {
        let! ct = Async.CancellationToken
        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

        let reference =
            compilation.GetMetadataReference containingAssembly
            |> nonNull "compilation.GetMetadataReference(containingAssembly)"

        let peReference = reference :?> PortableExecutableReference |> Option.ofObj

        let assemblyLocation =
            peReference |> Option.map _.FilePath |> Option.defaultValue "???"

        let decompilerSettings = DecompilerSettings()
        decompilerSettings.ThrowOnAssemblyResolveErrors <- false // this shouldn't be a showstopper for us

        let decompiler = CSharpDecompiler(assemblyLocation, decompilerSettings)

        // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
        // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
        decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

        let text =
            symbolMetadataName
            |> TypeSystem.FullTypeName
            |> decompiler.DecompileTypeAsString

        let mdDocumentFilename =
            $"$metadata$/assemblies/{containingAssembly.Name}/symbols/{symbolMetadataName}.cs"

        return project.AddDocument(mdDocumentFilename, text = text), text
    }

let workspaceFolderWithDocumentFromMetadata
    wf
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    =
    async {
        let symbolMetadataName = symbolGetMetadataName symbol

        match Map.tryFind (project.FilePath, symbolMetadataName) wf.DecompiledSymbolMetadata with
        | Some md -> return wf, md
        | None ->
            let! documentFromMd, text = documentFromMetadata project symbol.ContainingAssembly symbolMetadataName

            let csharpMetadata =
                { ProjectName = project.Name
                  AssemblyName = symbol.ContainingAssembly.Name
                  SymbolName = symbolMetadataName
                  Source = text }

            let symbolMetadata =
                { Metadata = csharpMetadata
                  Document = documentFromMd }

            let updatedWf =
                { wf with
                    DecompiledSymbolMetadata =
                        wf.DecompiledSymbolMetadata
                        |> Map.add (project.FilePath, symbolMetadataName) symbolMetadata }

            return updatedWf, symbolMetadata
    }

let workspaceFolderSymbolLocationsInMetadata wf project symbol = async {
    let! ct = Async.CancellationToken

    let! updatedWf, symbolMetadata = workspaceFolderWithDocumentFromMetadata wf project symbol

    // figure out location on the document (approx implementation)
    let! syntaxTree = symbolMetadata.Document.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

    let symbolMetadataUri = workspaceFolderMetadataSymbolSourceViewUri wf project symbol

    let collector =
        DocumentSymbolCollectorForMatchingSymbolName(symbolMetadataUri, symbol)

    let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask
    collector.Visit(root)

    let fallbackLocationInMetadata =
        { Uri = symbolMetadataUri
          Range =
            { Start = { Line = 0u; Character = 0u }
              End = { Line = 0u; Character = 1u } } }

    return
        match collector.GetLocations() with
        | [] -> [ fallbackLocationInMetadata ], updatedWf
        | ls -> ls, updatedWf
}

let workspaceFolderResolveSymbolLocation
    (wf: LspWorkspaceFolder)
    (settings: ServerSettings)
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (l: Microsoft.CodeAnalysis.Location)
    =
    match l.IsInMetadata, l.IsInSource with
    | true, _ ->
        match settings.UseMetadataUris with
        | true -> workspaceFolderSymbolLocationsInMetadata wf project symbol
        | false -> ([], wf) |> async.Return

    | false, true ->
        let wfPathToUri = workspaceFolderPathToUri wf

        match Location.fromRoslynLocation wfPathToUri l with
        | Some loc -> ([ loc ], wf) |> async.Return
        | None -> ([], wf) |> async.Return

    | _, _ -> ([], wf) |> async.Return

/// The process of retrieving locations may update LspWorkspaceFolder itself,
/// thus return value is a pair of symbol location list * LspWorkspaceFolder
let workspaceFolderSymbolLocations
    folder
    (settings: ServerSettings)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (project: Microsoft.CodeAnalysis.Project)
    =
    async {
        let mutable wf = folder
        let mutable aggregatedLspLocations = []

        for l in symbol.Locations do
            let! symLspLocations, updatedWf = workspaceFolderResolveSymbolLocation wf settings project symbol l

            aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations
            wf <- updatedWf

        return aggregatedLspLocations, wf
    }

let workspaceFolderDocumentDetails (wf: LspWorkspaceFolder) docType (u: string) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    match wf.Solution with
    | None -> None

    | Some solution ->
        let matchingUserDocuments =
            solution.Projects
            |> Seq.collect _.Documents
            |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri)
            |> List.ofSeq

        let matchingUserDocumentMaybe =
            match matchingUserDocuments with
            | [ d ] -> Some(d, UserDocument)
            | _ -> None

        let matchingDecompiledDocumentMaybe =
            u
            |> workspaceFolderParseMetadataSymbolSourceViewUri wf
            |> Option.bind (fun (projectPath, symbolMetadataName) ->
                Map.tryFind (projectPath, symbolMetadataName) wf.DecompiledSymbolMetadata)
            |> Option.map (fun x -> x.Document, DecompiledDocument)

        match docType with
        | UserDocument -> matchingUserDocumentMaybe
        | DecompiledDocument -> matchingDecompiledDocumentMaybe
        | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

let workspaceFolderProjectForPath wf (filePath: string) : Project option =
    let docDir = Path.GetDirectoryName filePath

    let fileIsOnProjectDir (p: Project) =
        let projectDir = Path.GetDirectoryName p.FilePath
        let projectDirWithDirSepChar = projectDir + string Path.DirectorySeparatorChar

        docDir = projectDir || docDir.StartsWith projectDirWithDirSepChar

    let findMatchingFileInSolution (sln: Solution) =
        sln.Projects |> Seq.filter fileIsOnProjectDir |> Seq.tryHead

    wf.Solution |> Option.bind findMatchingFileInSolution

let workspaceFolderAdditionalTextDocumentForPath (wf: LspWorkspaceFolder) (filePath: string) : TextDocument option =
    let project = workspaceFolderProjectForPath wf filePath

    match project with
    | None -> None
    | Some project ->
        project.AdditionalDocuments
        |> Seq.filter (fun d -> d.FilePath = filePath)
        |> Seq.tryHead

let workspaceFolderWithDocumentAdded
    wf
    (docFilePath: string)
    (text: string)
    : Async<LspWorkspaceFolder * Document option> =
    async {
        let projectOnPath = workspaceFolderProjectForPath wf docFilePath

        match projectOnPath with
        | Some proj ->
            let projectBaseDir = Path.GetDirectoryName proj.FilePath
            let docName = docFilePath.Substring(projectBaseDir.Length + 1)

            let newDoc =
                proj.AddDocument(name = docName, text = SourceText.From text, folders = null, filePath = docFilePath)

            let updatedWf =
                newDoc |> _.Project.Solution |> (fun sln -> { wf with Solution = Some sln })

            return updatedWf, Some newDoc

        | None ->
            logger.LogTrace(
                "workspaceFolderWithDocumentAdded: No parent project could be resolved to add file \"{file}\" to workspace!",
                docFilePath
            )

            return wf, None
    }

let workspaceFolderWithAdditionalTextDocumentTextUpdated
    (wf: LspWorkspaceFolder)
    (textDoc: TextDocument)
    (newSourceText: SourceText)
    : LspWorkspaceFolder =
    let updatedSolution =
        textDoc.Project
        |> _.RemoveAdditionalDocument(textDoc.Id)
        |> _.AddAdditionalDocument(textDoc.Name, newSourceText, textDoc.Folders, textDoc.FilePath)
        |> _.Project.Solution

    { wf with
        Solution = Some updatedSolution }

let workspaceFolderWithDocumentTextUpdated
    (wf: LspWorkspaceFolder)
    (doc: Document)
    (newSourceText: SourceText)
    : LspWorkspaceFolder =
    let updatedSolution = newSourceText |> doc.WithText |> _.Project.Solution

    { wf with
        Solution = Some updatedSolution }

let workspaceFolderWithAdditionalTextDocumentAdded
    (wf: LspWorkspaceFolder)
    (docFilePath: string)
    (text: string)
    : LspWorkspaceFolder * TextDocument option =
    let projectOnPath = workspaceFolderProjectForPath wf docFilePath

    match projectOnPath with
    | Some project ->
        let projectBaseDir = Path.GetDirectoryName project.FilePath
        let relativePath = Path.GetRelativePath(projectBaseDir, docFilePath)

        let folders = relativePath.Split Path.DirectorySeparatorChar
        let folders = folders |> Seq.take (folders.Length - 1)

        let newDoc =
            project.AddAdditionalDocument(
                name = Path.GetFileName docFilePath,
                text = SourceText.From text,
                folders = folders,
                filePath = docFilePath
            )

        let updatedWf =
            newDoc |> _.Project.Solution |> (fun sln -> { wf with Solution = Some sln })

        updatedWf, Some newDoc

    | None ->
        logger.LogTrace(
            "workspaceFolderWithAdditionalTextDocumentAdded: No parent project could be resolved to add file \"{file}\" to workspace!",
            docFilePath
        )

        wf, None
