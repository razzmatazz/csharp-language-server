module CSharpLanguageServer.Lsp.WorkspaceFolder

open System
open System.IO
open System.Threading

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

let logger = Logging.getLoggerByName "Lsp.WorkspaceFolder"

type LspWorkspaceDecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type LspWorkspaceOpenDocInfo = { Version: int; Touched: DateTime }

type LspWorkspaceFolderSolution =
    | Pending
    | Loading of Workspace * Solution
    | Ready of Workspace * Solution
    | Defunct

type LspWorkspaceFolder =
    {
        Uri: string
        Name: string

        /// When set, the solution loader uses this path directly instead of
        /// auto-discovering a solution under Uri. Set from the --solution CLI flag.
        SolutionPathOverride: string option

        Solution: LspWorkspaceFolderSolution

        /// key is (project.Path * symbol metadata name)
        DecompiledSymbolMetadata: Map<string * string, LspWorkspaceDecompiledMetadataDocument>

        OpenDocs: Map<string, LspWorkspaceOpenDocInfo>
    }

    static member Empty =
        { Uri = Directory.GetCurrentDirectory() |> Uri |> string
          Name = "(no name)"
          SolutionPathOverride = None
          Solution = Pending
          DecompiledSymbolMetadata = Map.empty
          OpenDocs = Map.empty }

type LspWorkspaceFolderDocumentType =
    | UserDocument // user Document from solution, on disk
    | DecompiledDocument // Document decompiled from metadata, readonly
    | AnyDocument

let workspaceFolderWithReadySolutionUpdated (update: Solution -> Solution) wf =
    match wf.Solution with
    | Ready(workspace, solution) ->
        let updatedSolution = update solution

        { wf with
            Solution = Ready(workspace, updatedSolution) }

    | Pending -> failwith "wf.Solution in Pending state!"
    | Loading _ -> failwith "wf.Solution in Loading state!"
    | Defunct -> failwith "wf.Solution in Defunct state!"

let workspaceFolderWithReadySolutionReplaced (sln: Solution) wf =
    workspaceFolderWithReadySolutionUpdated (fun _ -> sln) wf

// Unescape some necessary char before passing string to Uri.
// Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
// ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
/// "#/ProjDir" is Fragment instead of part of LocalPath.
let workspaceFolderUriUnescape (uri: string) (_wf: LspWorkspaceFolder) : string = uri.Replace("%3a", ":", true, null)

let workspaceFolderUriToPath (uri: string) (wf: LspWorkspaceFolder) : option<string> =
    try
        workspaceFolderUriUnescape uri wf
        |> Uri
        |> _.LocalPath
        |> Uri.UnescapeDataString
        |> Some
    with _ex ->
        None

let workspaceFolderPathToUri (path: string) (_wf: LspWorkspaceFolder) : string =
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
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    _wf
    =
    let projectFile =
        project.FilePath
        |> Uri
        |> string
        |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
        |> _.TrimEnd('/')

    let symbolMetadataName = symbolGetMetadataName symbol

    sprintf "csharp:/%s/decompiled/%s.cs" projectFile (Uri.EscapeDataString symbolMetadataName)

let workspaceFolderParseMetadataSymbolSourceViewUri (uri: string) (_wf: LspWorkspaceFolder) : option<string * string> =
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
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    wf
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

let workspaceFolderSymbolLocationsInMetadata project symbol wf = async {
    let! ct = Async.CancellationToken

    let! updatedWf, symbolMetadata = workspaceFolderWithDocumentFromMetadata project symbol wf

    // figure out location on the document (approx implementation)
    let! syntaxTree = symbolMetadata.Document.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

    let symbolMetadataUri = workspaceFolderMetadataSymbolSourceViewUri project symbol wf

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
    (config: CSharpConfiguration)
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (l: Microsoft.CodeAnalysis.Location)
    (wf: LspWorkspaceFolder)
    =
    match l.IsInMetadata, l.IsInSource with
    | true, _ ->
        match config.useMetadataUris |> Option.defaultValue false with
        | true -> workspaceFolderSymbolLocationsInMetadata project symbol wf
        | false -> ([], wf) |> async.Return

    | false, true ->
        let wfPathToUri path = workspaceFolderPathToUri path wf

        match Location.fromRoslynLocation wfPathToUri l with
        | Some loc -> ([ loc ], wf) |> async.Return
        | None -> ([], wf) |> async.Return

    | _, _ -> ([], wf) |> async.Return

/// The process of retrieving locations may update LspWorkspaceFolder itself,
/// thus return value is a pair of symbol location list * LspWorkspaceFolder
let workspaceFolderSymbolLocations
    folder
    (config: CSharpConfiguration)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    (project: Microsoft.CodeAnalysis.Project)
    =
    async {
        let mutable wf = folder
        let mutable aggregatedLspLocations = []

        for l in symbol.Locations do
            let! symLspLocations, updatedWf = workspaceFolderResolveSymbolLocation config project symbol l wf

            aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations
            wf <- updatedWf

        return aggregatedLspLocations, wf
    }

let workspaceFolderDocumentDetails docType (u: string) (wf: LspWorkspaceFolder) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    match wf.Solution with
    | Pending -> None
    | Loading _ -> None
    | Defunct -> None
    | Ready(_, solution) ->
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
            workspaceFolderParseMetadataSymbolSourceViewUri u wf
            |> Option.bind (fun (projectPath, symbolMetadataName) ->
                Map.tryFind (projectPath, symbolMetadataName) wf.DecompiledSymbolMetadata)
            |> Option.map (fun x -> x.Document, DecompiledDocument)

        match docType with
        | UserDocument -> matchingUserDocumentMaybe
        | DecompiledDocument -> matchingDecompiledDocumentMaybe
        | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

let workspaceFolderDocument docType u wf =
    workspaceFolderDocumentDetails docType u wf |> Option.map fst

let workspaceFolderProjectForPath (filePath: string) wf : Project option =
    let docDir = Path.GetDirectoryName filePath

    let fileIsOnProjectDir (p: Project) =
        let projectDir = Path.GetDirectoryName p.FilePath
        let projectDirWithDirSepChar = projectDir + string Path.DirectorySeparatorChar

        docDir = projectDir || docDir.StartsWith projectDirWithDirSepChar

    let findMatchingFileInSolution (sln: Solution) =
        sln.Projects |> Seq.filter fileIsOnProjectDir |> Seq.tryHead

    match wf.Solution with
    | Ready(_, solution) -> findMatchingFileInSolution solution
    | _ -> None

let workspaceFolderAdditionalTextDocumentForPath (filePath: string) (wf: LspWorkspaceFolder) : TextDocument option =
    let project = workspaceFolderProjectForPath filePath wf

    match project with
    | None -> None
    | Some project ->
        project.AdditionalDocuments
        |> Seq.filter (fun d -> d.FilePath = filePath)
        |> Seq.tryHead

let workspaceFolderWithDocumentAdded
    (docFilePath: string)
    (text: string)
    wf
    : Async<LspWorkspaceFolder * Document option> =
    async {
        let projectOnPath = workspaceFolderProjectForPath docFilePath wf

        match projectOnPath with
        | Some proj ->
            let projectBaseDir = Path.GetDirectoryName proj.FilePath
            let docName = docFilePath.Substring(projectBaseDir.Length + 1)

            let newDoc =
                proj.AddDocument(name = docName, text = SourceText.From text, folders = null, filePath = docFilePath)

            let updatedWf = workspaceFolderWithReadySolutionReplaced newDoc.Project.Solution wf

            return updatedWf, Some newDoc

        | None ->
            logger.LogTrace(
                "workspaceFolderWithDocumentAdded: No parent project could be resolved to add file \"{file}\" to workspace!",
                docFilePath
            )

            return wf, None
    }

let workspaceFolderWithAdditionalTextDocumentTextUpdated
    (textDoc: TextDocument)
    (newSourceText: SourceText)
    (wf: LspWorkspaceFolder)
    =
    let sln =
        textDoc.Project
        |> _.RemoveAdditionalDocument(textDoc.Id)
        |> _.AddAdditionalDocument(textDoc.Name, newSourceText, textDoc.Folders, textDoc.FilePath)
        |> _.Project.Solution

    workspaceFolderWithReadySolutionReplaced sln wf

let workspaceFolderWithDocumentTextUpdated (doc: Document) (newSourceText: SourceText) (wf: LspWorkspaceFolder) =
    let sln = newSourceText |> doc.WithText |> _.Project.Solution

    workspaceFolderWithReadySolutionReplaced sln wf

let workspaceFolderWithDocumentRemoved (uri: string) (wf: LspWorkspaceFolder) : LspWorkspaceFolder =
    let removeDoc (solution: Solution) =
        let filename = workspaceFolderUriToPath uri wf

        let doc =
            solution.Projects
            |> Seq.collect _.Documents
            |> Seq.tryFind (fun d -> Some d.FilePath = filename)

        match doc with
        | Some doc -> solution.RemoveDocument(doc.Id)
        | None ->
            logger.LogTrace("workspaceFolderWithDocumentRemoved: No document found for uri \"{uri}\"", uri)
            solution

    workspaceFolderWithReadySolutionUpdated removeDoc wf

let workspaceFolderWithAdditionalTextDocumentAdded
    (docFilePath: string)
    (text: string)
    (wf: LspWorkspaceFolder)
    : LspWorkspaceFolder * TextDocument option =
    let projectOnPath = workspaceFolderProjectForPath docFilePath wf

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
            workspaceFolderWithReadySolutionReplaced (newDoc.Project.Solution) wf

        updatedWf, Some newDoc

    | None ->
        logger.LogTrace(
            "workspaceFolderWithAdditionalTextDocumentAdded: No parent project could be resolved to add file \"{file}\" to workspace!",
            docFilePath
        )

        wf, None

let workspaceFolderWithAdditionalDocumentRemoved (uri: string) (wf: LspWorkspaceFolder) : LspWorkspaceFolder =
    match wf.Solution with
    | Pending
    | Loading _
    | Defunct -> wf
    | Ready(workspace, solution) ->
        let filename = workspaceFolderUriToPath uri wf

        let doc =
            solution.Projects
            |> Seq.collect _.AdditionalDocuments
            |> Seq.tryFind (fun d -> Some d.FilePath = filename)

        match doc with
        | Some doc ->
            { wf with
                Solution = Ready(workspace, solution.RemoveAdditionalDocument(doc.Id)) }
        | None ->
            logger.LogTrace(
                "workspaceFolderWithAdditionalDocumentRemoved: No additional document found for uri \"{uri}\"",
                uri
            )

            wf

let workspaceFolderDocumentSymbol
    docType
    (uri: DocumentUri)
    (pos: Ionide.LanguageServerProtocol.Types.Position)
    (wf: LspWorkspaceFolder)
    =
    async {
        match uri.EndsWith ".cshtml" with
        | true ->
            let cshtmlPath = workspaceFolderUriToPath uri wf

            match wf.Solution, cshtmlPath with
            | Ready(_, solution), Some cshtmlPath ->
                let! symbolInfo = solutionFindSymbolForRazorDocumentPath solution cshtmlPath pos
                return symbolInfo

            | _, _ -> return None

        | false ->
            let docForUri = workspaceFolderDocument docType uri wf

            match docForUri with
            | None -> return None
            | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let position = Position.toRoslynPosition sourceText.Lines pos
                let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

                let symbolInfo =
                    symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)

                return symbolInfo
    }

let workspaceFolderDocumentSemanticModel (uri: DocumentUri) (wf: LspWorkspaceFolder) = async {
    if uri.EndsWith ".cshtml" then
        let cshtmlPath = workspaceFolderUriToPath uri wf

        match wf.Solution, cshtmlPath with
        | Ready(_, solution), Some cshtmlPath ->
            match! solutionGetRazorDocumentForPath solution cshtmlPath with
            | None -> return None
            | Some(_, compilation, cshtmlTree) ->
                let semanticModel = compilation.GetSemanticModel(cshtmlTree) |> Option.ofObj
                return semanticModel

        | _, _ -> return None
    else
        let doc = workspaceFolderDocument AnyDocument uri wf

        match doc with
        | None -> return None
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask |> Async.map Option.ofObj
            return semanticModel
}

let workspaceFolderDocumentVersion (uri: string) (wf: LspWorkspaceFolder) : int option =
    wf.OpenDocs |> Map.tryFind uri |> Option.map _.Version

let workspaceFolderWithDocOpened
    (uri: string)
    (ver: int)
    (timestamp: DateTime)
    (wf: LspWorkspaceFolder)
    : LspWorkspaceFolder =
    let openDocInfo = { Version = ver; Touched = timestamp }

    { wf with
        OpenDocs = wf.OpenDocs |> Map.add uri openDocInfo }

let workspaceFolderWithDocClosed (uri: string) (wf: LspWorkspaceFolder) : LspWorkspaceFolder =
    { wf with
        OpenDocs = wf.OpenDocs |> Map.remove uri }

let workspaceFolderWithDocTouched
    (uri: string)
    (timestamp: DateTime)
    (wf: LspWorkspaceFolder)
    : LspWorkspaceFolder option =
    match wf.OpenDocs |> Map.tryFind uri with
    | None -> None
    | Some openDocInfo ->
        let updated = { openDocInfo with Touched = timestamp }

        Some
            { wf with
                OpenDocs = wf.OpenDocs |> Map.add uri updated }

/// Release all disposable resources held by a workspace folder.
/// Call this whenever a folder is replaced or removed from the workspace.
let workspaceFolderTeardown (wf: LspWorkspaceFolder) : unit =
    match wf.Solution with
    | Defunct -> ()
    | Pending -> ()
    | Loading(workspace, _) -> workspace.Dispose()
    | Ready(workspace, _) -> workspace.Dispose()

/// Load the solution for a single workspace folder, returning the updated folder.
let workspaceFolderWithSolutionLoaded
    (lspClient: ILspClient)
    (progressReporter: ProgressReporter)
    (totalFolders: int)
    (folderNum: int)
    (wf: LspWorkspaceFolder)
    =
    async {
        let wfRootDir = workspaceFolderUriToPath wf.Uri wf

        let beginMessage =
            sprintf "%s (%d/%d)..." (wfRootDir |> Option.defaultValue "???") folderNum totalFolders

        do! progressReporter.Report(message = beginMessage)

        let! newSolution =
            solutionLoadSolutionWithPathOrOnDir lspClient progressReporter wf.SolutionPathOverride wfRootDir.Value

        let updatedWf =
            match newSolution with
            | Some(workspace, solution) ->
                { wf with
                    Solution = Ready(workspace, solution) }
            | None -> { wf with Solution = Defunct }

        do! progressReporter.Report(false, sprintf "Finished loading workspace folder %s" wf.Uri)

        return updatedWf
    }
