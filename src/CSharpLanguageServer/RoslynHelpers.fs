module CSharpLanguageServer.RoslynHelpers

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Text.RegularExpressions

open Microsoft.Build.Locator
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Build.Exceptions
open Microsoft.Build.Construction
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text
open Microsoft.Extensions.Logging

open CSharpLanguageServer
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Roslyn.WorkspaceServices
open CSharpLanguageServer.Util


type DocumentSymbolCollectorForMatchingSymbolName(documentUri, sym: ISymbol) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations = []
    let mutable suggestedLocations = []

    let collectIdentifier (identifier: SyntaxToken) exactMatch =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span |> Range.fromLinePositionSpan }

        if exactMatch then
            collectedLocations <- location :: collectedLocations
        else
            suggestedLocations <- location :: suggestedLocations

    member __.GetLocations() =
        if not (Seq.isEmpty collectedLocations) then
            collectedLocations |> Seq.rev |> List.ofSeq
        else
            suggestedLocations |> Seq.rev |> List.ofSeq

    override __.Visit(node: SyntaxNode | null) =
        let node = node |> nonNull "node"

        match sym.Kind, node with
        | SymbolKind.Method, (:? MethodDeclarationSyntax as m) when m.Identifier.ValueText = sym.Name ->
            let symMethod = sym :?> IMethodSymbol

            let methodArityMatches =
                symMethod.Parameters.Length = m.ParameterList.Parameters.Count

            collectIdentifier m.Identifier methodArityMatches

        | _, (:? TypeDeclarationSyntax as t) when t.Identifier.ValueText = sym.Name ->
            collectIdentifier t.Identifier false

        | _, (:? PropertyDeclarationSyntax as p) when p.Identifier.ValueText = sym.Name ->
            collectIdentifier p.Identifier false

        | _, (:? EventDeclarationSyntax as e) when e.Identifier.ValueText = sym.Name ->
            collectIdentifier e.Identifier false
        // TODO: collect other type of syntax nodes too

        | _ -> ()


        base.Visit node

let loadProjectFilenamesFromSolution (solutionPath: string) =
    assert Path.IsPathRooted solutionPath
    let projectFilenames = new List<string>()

    let solutionFile = SolutionFile.Parse solutionPath

    for project in solutionFile.ProjectsInOrder do
        if project.ProjectType = SolutionProjectType.KnownToBeMSBuildFormat then
            projectFilenames.Add project.AbsolutePath

    projectFilenames |> Set.ofSeq


type TfmCategory =
    | NetFramework of Version
    | NetStandard of Version
    | NetCoreApp of Version
    | Net of Version
    | Unknown


let selectLatestTfm (tfms: string seq) : string option =
    let parseTfm (tfm: string) : TfmCategory =
        let patterns =
            [ @"^net(?<major>\d)(?<minor>\d)?(?<build>\d)?$", NetFramework
              @"^netstandard(?<major>\d+)\.(?<minor>\d+)$", NetStandard
              @"^netcoreapp(?<major>\d+)\.(?<minor>\d+)$", NetCoreApp
              @"^net(?<major>\d+)\.(?<minor>\d+)$", Net ]

        let matchingTfmCategory (pat, categoryCtor) =
            let m = Regex.Match(tfm.ToLowerInvariant(), pat)

            if m.Success then
                let readVersionNum (groupName: string) =
                    let group = m.Groups.[groupName]
                    if group.Success then int group.Value else 0

                Version(readVersionNum "major", readVersionNum "minor", readVersionNum "build")
                |> categoryCtor
                |> Some
            else
                None

        patterns |> List.tryPick matchingTfmCategory |> Option.defaultValue Unknown

    let rankTfm =
        function
        | Net v -> 3000 + v.Major * 10 + v.Minor
        | NetCoreApp v -> 2000 + v.Major * 10 + v.Minor
        | NetStandard v -> 1000 + v.Major * 10 + v.Minor
        | NetFramework v -> 0 + v.Major * 10 + v.Minor
        | Unknown -> -1

    tfms |> Seq.sortByDescending (parseTfm >> rankTfm) |> Seq.tryHead


let loadProjectTfms (logger: ILogger) (projs: string seq) : Map<string, list<string>> =
    let mutable projectTfms = Map.empty

    for projectFilename in projs do
        let projectCollection = new Microsoft.Build.Evaluation.ProjectCollection()
        let props = new Dictionary<string, string>()

        try
            let buildProject =
                projectCollection.LoadProject(projectFilename, props, toolsVersion = null)

            let noneIfEmpty s =
                s
                |> Option.ofObj
                |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)

            let targetFramework =
                match buildProject.GetPropertyValue "TargetFramework" |> noneIfEmpty with
                | Some tfm -> [ tfm.Trim() ]
                | None -> []

            let targetFrameworks =
                match buildProject.GetPropertyValue "TargetFrameworks" |> noneIfEmpty with
                | Some tfms -> tfms.Split ";" |> Array.map (fun s -> s.Trim()) |> List.ofArray
                | None -> []

            projectTfms <- projectTfms |> Map.add projectFilename (targetFramework @ targetFrameworks)

            projectCollection.UnloadProject buildProject
        with :? InvalidProjectFileException as ipfe ->
            logger.LogDebug(
                "loadProjectTfms: failed to load {projectFilename}: {ex}",
                projectFilename,
                ipfe.GetType() |> string
            )

    projectTfms

let applyWorkspaceTargetFrameworkProp (tfmsPerProject: Map<string, list<string>>) props : Map<string, string> =
    let selectedTfm =
        match tfmsPerProject.Count with
        | 0 -> None
        | _ ->
            tfmsPerProject.Values
            |> Seq.map Set.ofSeq
            |> Set.intersectMany
            |> selectLatestTfm

    match selectedTfm with
    | Some tfm -> props |> Map.add "TargetFramework" tfm
    | None -> props

let resolveDefaultWorkspaceProps (logger: ILogger) projs : Map<string, string> =
    let tfmsPerProject = loadProjectTfms logger projs

    Map.empty |> applyWorkspaceTargetFrameworkProp tfmsPerProject


let tryLoadSolutionOnPath (lspClient: ILspClient) (logger: ILogger) (solutionPath: string) =
    assert Path.IsPathRooted solutionPath
    let progress = ProgressReporter lspClient

    let logMessage m =
        lspClient.WindowLogMessage
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m }

    let showMessage m =
        lspClient.WindowShowMessage
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m }

    async {
        try
            let beginMessage = sprintf "Loading solution \"%s\"..." solutionPath
            do! progress.Begin beginMessage
            do! logMessage beginMessage

            let projs = loadProjectFilenamesFromSolution solutionPath
            let workspaceProps = resolveDefaultWorkspaceProps logger projs

            if workspaceProps.Count > 0 then
                logger.LogInformation("Will use these MSBuild props: {workspaceProps}", string workspaceProps)

            let msbuildWorkspace =
                MSBuildWorkspace.Create(workspaceProps, CSharpLspHostServices())

            msbuildWorkspace.LoadMetadataForReferencedProjects <- true

            let! solution = msbuildWorkspace.OpenSolutionAsync solutionPath |> Async.AwaitTask

            for diag in msbuildWorkspace.Diagnostics do
                logger.LogInformation("msbuildWorkspace.Diagnostics: {message}", diag.ToString())

                do! logMessage (sprintf "msbuildWorkspace.Diagnostics: %s" (diag.ToString()))

            let endMessage = sprintf "Finished loading solution \"%s\"" solutionPath
            do! progress.End endMessage
            do! logMessage endMessage

            return Some solution
        with ex ->
            let errorMessage =
                sprintf "Solution \"%s\" could not be loaded: %s" solutionPath (ex.ToString())

            do! progress.End errorMessage
            do! showMessage errorMessage
            return None
    }

let tryLoadSolutionFromProjectFiles
    (lspClient: ILspClient)
    (logger: ILogger)
    (logMessage: string -> Async<unit>)
    (projs: string list)
    =
    let progress = ProgressReporter lspClient

    async {
        do! progress.Begin($"Loading {projs.Length} project(s)...", false, $"0/{projs.Length}", 0u)
        let loadedProj = ref 0

        let workspaceProps = resolveDefaultWorkspaceProps logger projs

        if workspaceProps.Count > 0 then
            logger.LogDebug("Will use these MSBuild props: {workspaceProps}", string workspaceProps)

        let msbuildWorkspace =
            MSBuildWorkspace.Create(workspaceProps, CSharpLspHostServices())

        msbuildWorkspace.LoadMetadataForReferencedProjects <- true

        for file in projs do
            if projs.Length < 10 then
                do! logMessage (sprintf "loading project \"%s\".." file)

            try
                do! msbuildWorkspace.OpenProjectAsync file |> Async.AwaitTask |> Async.Ignore
            with ex ->
                logger.LogError("could not OpenProjectAsync('{file}'): {exception}", file, string ex)

            let projectFile = new FileInfo(file)
            let projName = projectFile.Name
            let loaded = Interlocked.Increment loadedProj
            let percent = 100 * loaded / projs.Length |> uint
            do! progress.Report(false, $"{projName} {loaded}/{projs.Length}", percent)

        for diag in msbuildWorkspace.Diagnostics do
            logger.LogTrace("msbuildWorkspace.Diagnostics: {message}", diag.ToString())

        do! progress.End(sprintf "OK, %d project file(s) loaded" projs.Length)

        //workspace <- Some(msbuildWorkspace :> Workspace)
        return Some msbuildWorkspace.CurrentSolution
    }

let selectPreferredSolution (slnFiles: string list) : option<string> =
    let getProjectCount (slnPath: string) =
        try
            let sln = SolutionFile.Parse slnPath
            Some(sln.ProjectsInOrder.Count, slnPath)
        with _ ->
            None

    match slnFiles with
    | [] -> None
    | slnFiles ->
        slnFiles
        |> Seq.choose getProjectCount
        |> Seq.sortByDescending fst
        |> Seq.map snd
        |> Seq.tryHead

let findAndLoadSolutionOnDir (lspClient: ILspClient) (logger: ILogger) dir = async {
    let fileNotOnNodeModules (filename: string) =
        filename.Split Path.DirectorySeparatorChar |> Seq.contains "node_modules" |> not

    let solutionFiles =
        [ "*.sln"; "*.slnx" ]
        |> List.collect (fun p -> Directory.GetFiles(dir, p, SearchOption.AllDirectories) |> List.ofArray)
        |> Seq.filter fileNotOnNodeModules
        |> Seq.toList

    let logMessage m =
        lspClient.WindowLogMessage
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m }

    do! logMessage (sprintf "%d solution(s) found: [%s]" solutionFiles.Length (String.Join(", ", solutionFiles)))

    let preferredSlnFile = solutionFiles |> selectPreferredSolution

    match preferredSlnFile with
    | None ->
        do!
            logMessage (
                "no single preferred .sln/.slnx file found on "
                + dir
                + "; fill load project files manually"
            )

        do! logMessage ("looking for .csproj/fsproj files on " + dir + "..")

        let projFiles =
            let csprojFiles = Directory.GetFiles(dir, "*.csproj", SearchOption.AllDirectories)
            let fsprojFiles = Directory.GetFiles(dir, "*.fsproj", SearchOption.AllDirectories)

            [ csprojFiles; fsprojFiles ]
            |> Seq.concat
            |> Seq.filter fileNotOnNodeModules
            |> Seq.toList

        if projFiles.Length = 0 then
            let message = "no or .csproj/.fsproj or sln files found on " + dir
            do! logMessage message
            Exception message |> raise

        return! tryLoadSolutionFromProjectFiles lspClient logger logMessage projFiles

    | Some solutionPath -> return! tryLoadSolutionOnPath lspClient logger solutionPath
}

let loadSolutionOnSolutionPathOrDir
    (lspClient: ILspClient)
    (logger: ILogger)
    (solutionPathMaybe: string option)
    (rootPath: string)
    =
    match solutionPathMaybe with
    | Some solutionPath -> async {
        let rootedSolutionPath =
            match Path.IsPathRooted solutionPath with
            | true -> solutionPath
            | false -> Path.Combine(rootPath, solutionPath)

        return! tryLoadSolutionOnPath lspClient logger rootedSolutionPath
      }

    | None -> async {
        let logMessage: LogMessageParams =
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: attempting to find and load solution based on root path (\"%s\").." rootPath }

        do! lspClient.WindowLogMessage logMessage
        return! findAndLoadSolutionOnDir lspClient logger rootPath
      }

let getContainingTypeOrThis (symbol: ISymbol) : INamedTypeSymbol =
    if symbol :? INamedTypeSymbol then
        symbol :?> INamedTypeSymbol
    else
        symbol.ContainingType

let getFullReflectionName (containingType: INamedTypeSymbol) =
    let stack = Stack<string>()
    stack.Push containingType.MetadataName
    let mutable ns = containingType.ContainingNamespace

    let mutable doContinue = true

    while doContinue do
        stack.Push ns.Name
        ns <- ns.ContainingNamespace

        doContinue <- ns <> null && not ns.IsGlobalNamespace

    String.Join(".", stack)

let getProjectForPathOnSolution (solution: Solution) (filePath: string) : Project option =
    let docDir = Path.GetDirectoryName filePath

    let fileOnProjectDir (p: Project) =
        let projectDir = Path.GetDirectoryName p.FilePath
        let projectDirWithDirSepChar = projectDir + string Path.DirectorySeparatorChar

        docDir = projectDir || docDir.StartsWith projectDirWithDirSepChar

    solution.Projects |> Seq.filter fileOnProjectDir |> Seq.tryHead

let tryAddDocument
    (logger: ILogger)
    (docFilePath: string)
    (text: string)
    (solution: Solution)
    : Async<Document option> =
    async {
        let projectOnPath = getProjectForPathOnSolution solution docFilePath

        let! newDocumentMaybe =
            match projectOnPath with
            | Some proj ->
                let projectBaseDir = Path.GetDirectoryName proj.FilePath
                let docName = docFilePath.Substring(projectBaseDir.Length + 1)

                let newDoc =
                    proj.AddDocument(
                        name = docName,
                        text = SourceText.From text,
                        folders = null,
                        filePath = docFilePath
                    )

                Some newDoc |> async.Return

            | None -> async {
                logger.LogTrace("No parent project could be resolved to add file \"{file}\" to workspace", docFilePath)
                return None
              }

        return newDocumentMaybe
    }

let makeDocumentFromMetadata
    (compilation: Microsoft.CodeAnalysis.Compilation)
    (project: Microsoft.CodeAnalysis.Project)
    (l: Microsoft.CodeAnalysis.Location)
    (fullName: string)
    =
    let mdLocation = l

    let containingAssembly =
        mdLocation.MetadataModule
        |> nonNull "mdLocation.MetadataModule"
        |> _.ContainingAssembly

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
    mdDocument, text


let initializeMSBuild (logger: ILogger) : unit =
    let vsInstanceQueryOpt = VisualStudioInstanceQueryOptions.Default
    let vsInstanceList = MSBuildLocator.QueryVisualStudioInstances(vsInstanceQueryOpt)

    if Seq.isEmpty vsInstanceList then
        raise (
            InvalidOperationException(
                "No instances of MSBuild could be detected."
                + Environment.NewLine
                + "Try calling RegisterInstance or RegisterMSBuildPath to manually register one."
            )
        )

    logger.LogTrace("MSBuildLocator instances found:")

    for vsInstance in vsInstanceList do
        logger.LogTrace(
            sprintf
                "- SDK=\"%s\", Version=%s, MSBuildPath=\"%s\", DiscoveryType=%s"
                vsInstance.Name
                (string vsInstance.Version)
                vsInstance.MSBuildPath
                (string vsInstance.DiscoveryType)
        )

    let vsInstance = vsInstanceList |> Seq.head

    logger.LogInformation(
        "MSBuildLocator: will register \"{vsInstanceName}\", Version={vsInstanceVersion} as default instance",
        vsInstance.Name,
        (string vsInstance.Version)
    )

    MSBuildLocator.RegisterInstance(vsInstance)
