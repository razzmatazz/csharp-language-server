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
    |> sprintf "csharp:///%s"

let workspaceFolderMetadataSymbolSourceViewUri
    _wf
    (project: Microsoft.CodeAnalysis.Project)
    (symbol: Microsoft.CodeAnalysis.ISymbol)
    =
    let projectFileUri =
        project.FilePath
        |> Uri
        |> string
        |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
        |> _.TrimEnd('/')
        |> sprintf "csharp:///%s"

    let symbolMetadataName = symbolGetMetadataName symbol

    sprintf "%s?symbol=%s&view=source" projectFileUri (Uri.EscapeDataString(symbolMetadataName))

let workspaceFolderParseMetadataSymbolSourceViewUri (_wf: LspWorkspaceFolder) (uri: string) : option<string * string> =
    let uri = uri |> Uri

    match uri.Scheme with
    | "csharp" ->
        let queryParams =
            uri.Query
            |> _.TrimStart('?').Split([| '&' |], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map _.Split('=')
            |> Seq.filter (fun parts -> parts.Length = 2)
            |> Seq.map (fun parts -> parts[0], parts[1])
            |> Map.ofSeq

        let getParam name : option<string> =
            queryParams |> Map.tryFind name |> Option.map Uri.UnescapeDataString

        match getParam "view", getParam "symbol" with
        | (Some "source", Some symbolMetadataName) -> Some(uri.LocalPath, symbolMetadataName)
        | _ -> None

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

        | false, true ->
            let wfPathToUri = workspaceFolderPathToUri wf

            return
                match Location.fromRoslynLocation wfPathToUri l with
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
                u
                |> workspaceFolderParseMetadataSymbolSourceViewUri wf
                |> Option.bind (fun (projectPath, symbolMetadataName) ->
                    Map.tryFind (projectPath, symbolMetadataName) wf.DecompiledSymbolMetadata)
                |> Option.map (fun x -> x.Document, DecompiledDocument)

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
