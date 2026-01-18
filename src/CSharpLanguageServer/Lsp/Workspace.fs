module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Extensions.Logging
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Symbol
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.Solution

let logger = Logging.getLoggerByName "Lsp.Workspace"

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

            let updatedFolder =
                { wf with
                    DecompiledSymbolMetadata =
                        wf.DecompiledSymbolMetadata
                        |> Map.add (project.FilePath, symbolMetadataName) symbolMetadata }

            return updatedFolder, symbolMetadata
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

type LspWorkspaceOpenDocInfo = { Version: int; Touched: DateTime }

type LspWorkspace =
    { Folders: LspWorkspaceFolder list
      OpenDocs: Map<string, LspWorkspaceOpenDocInfo> }

    static member Empty = { Folders = []; OpenDocs = Map.empty }

let workspaceFrom (workspaceFolders: WorkspaceFolder list) =
    if workspaceFolders.Length = 0 then
        failwith "workspaceFrom: at least 1 workspace folder must be provided!"

    let folders =
        workspaceFolders
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
    let wf = workspaceFolder workspace u

    let docAndDocType =
        match wf with
        | None -> None
        | Some wf -> workspaceFolderDocumentDetails wf docType u

    wf, docAndDocType

let workspaceDocument workspace docType (u: string) =
    let wf, docAndType = workspaceDocumentDetails workspace docType u
    let doc = docAndType |> Option.map fst
    wf, doc

let workspaceDocumentSemanticModel (workspace: LspWorkspace) (uri: DocumentUri) = async {
    let wf = workspaceFolder workspace uri

    match wf with
    | None -> return None, None
    | Some wf ->
        if uri.EndsWith ".cshtml" then
            let cshtmlPath = workspaceFolderUriToPath wf uri

            match wf.Solution, cshtmlPath with
            | Some solution, Some cshtmlPath ->
                match! solutionGetRazorDocumentForPath solution cshtmlPath with
                | None -> return Some wf, None
                | Some(_, compilation, cshtmlTree) ->
                    let semanticModel = compilation.GetSemanticModel(cshtmlTree) |> Option.ofObj
                    return Some wf, semanticModel

            | _, _ -> return None, None
        else
            let wf, docAndType = workspaceDocumentDetails workspace AnyDocument uri

            match docAndType with
            | Some(doc, _) ->
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask |> Async.map Option.ofObj
                return wf, semanticModel

            | None -> return wf, None
}

let workspaceDocumentSymbol
    (workspace: LspWorkspace)
    docType
    (uri: DocumentUri)
    (pos: Ionide.LanguageServerProtocol.Types.Position)
    =
    async {
        let wf = workspaceFolder workspace uri

        match wf, uri.EndsWith ".cshtml" with
        | None, _ -> return None, None

        | Some wf, true ->
            let cshtmlPath = workspaceFolderUriToPath wf uri

            match wf.Solution, cshtmlPath with
            | Some solution, Some cshtmlPath ->
                let! symbolInfo = solutionFindSymbolForRazorDocumentPath solution cshtmlPath pos
                return Some wf, symbolInfo

            | _, _ -> return Some wf, None

        | Some wf, false ->
            let docForUri = uri |> workspaceFolderDocumentDetails wf docType

            match docForUri with
            | None -> return Some wf, None
            | Some(doc, _) ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let position = Position.toRoslynPosition sourceText.Lines pos
                let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

                let symbolInfo =
                    symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc.Project, Some doc)

                return Some wf, symbolInfo
    }

let workspaceDocumentVersion workspace uri =
    uri |> workspace.OpenDocs.TryFind |> Option.map _.Version

let workspaceWithSolutionsLoaded (settings: ServerSettings) (lspClient: ILspClient) workspace = async {
    let progressReporter = ProgressReporter lspClient

    let beginMessage =
        sprintf "Loading workspace (%d workspace folders)" workspace.Folders.Length

    do! progressReporter.Begin(beginMessage)

    let mutable updatedWorkspace = workspace

    for wf in workspace.Folders do
        let beginMessage = sprintf "Loading workspace folder %s..." wf.Uri

        let wfRootDir = wf.Uri |> workspaceFolderUriToPath wf

        let! newSolution =
            solutionLoadSolutionWithPathOrOnDir lspClient progressReporter settings.SolutionPath wfRootDir.Value

        let updatedWf = { wf with Solution = newSolution }
        updatedWorkspace <- updatedWf |> workspaceWithFolder updatedWorkspace

        do! progressReporter.Report(false, sprintf "Finished loading workspace folder %s" wf.Uri)

    let endMessage = sprintf "Finished loading workspace"
    do! progressReporter.End(endMessage)

    return updatedWorkspace
}
