module CSharpLanguageServer.Roslyn.Solution

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Build.Construction
open Microsoft.Build.Exceptions
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text
open Microsoft.Extensions.Logging
open NuGet.Frameworks

open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.WorkspaceServices

let private logger = Logging.getLoggerByName "Roslyn.Solution"

let initializeMSBuild () : unit =
    let vsInstanceQueryOpt = VisualStudioInstanceQueryOptions.Default
    let vsInstanceList = MSBuildLocator.QueryVisualStudioInstances(vsInstanceQueryOpt)

    if Seq.isEmpty vsInstanceList then
        raise (
            InvalidOperationException(
                "No instances of MSBuild could be detected."
                + Environment.NewLine
                + "Try calling RegisterInstance or RegisterMSBuildPath to manually register one."
                + Environment.NewLine
                + "Or try installing the dotnet sdk version: "
                + sprintf "%i.%i." Environment.Version.Major Environment.Version.Minor
            )
        )

    let sdkInstanceInfo =
        "SDK instances found, as reported by MSBuild:\n"
        + String.Join(
            "\n",
            vsInstanceList
            |> Seq.map (fun vsInstance ->
                sprintf
                    "- SDK=\"%s\", Version=%s, MSBuildPath=\"%s\", DiscoveryType=%s"
                    vsInstance.Name
                    (string vsInstance.Version)
                    vsInstance.MSBuildPath
                    (string vsInstance.DiscoveryType))
        )

    logger.LogInformation(sdkInstanceInfo)

    let vsInstance = vsInstanceList |> Seq.head

    logger.LogInformation(
        "MSBuildLocator: will register \"{vsInstanceName}\", Version={vsInstanceVersion} as default instance",
        vsInstance.Name,
        (string vsInstance.Version)
    )

    MSBuildLocator.RegisterInstance(vsInstance)

let solutionLoadProjectFilenames (solutionPath: string) =
    assert Path.IsPathRooted solutionPath
    let projectFilenames = new List<string>()

    let solutionFile = SolutionFile.Parse solutionPath

    for project in solutionFile.ProjectsInOrder do
        if project.ProjectType = SolutionProjectType.KnownToBeMSBuildFormat then
            projectFilenames.Add project.AbsolutePath

    projectFilenames |> Set.ofSeq


let loadProjectTfms (projs: string seq) : Map<string, list<string>> =
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

let frameworkIsCompatible a b =
    DefaultCompatibilityProvider.Instance.IsCompatible(a, b)

let compatibleTfmsOfTwoSets afxs bfxs = seq {
    for a in afxs |> Seq.map NuGetFramework.Parse do
        for b in bfxs |> Seq.map NuGetFramework.Parse do
            if frameworkIsCompatible a b then
                yield a.GetShortFolderName()
            else if frameworkIsCompatible b a then
                yield b.GetShortFolderName()
}

let compatibleTfmSet (tfmSets: list<Set<string>>) : Set<string> =
    match tfmSets.Length with
    | 0 -> Set.empty
    | 1 -> tfmSets |> List.head
    | _ ->
        let firstSet = tfmSets |> List.head

        tfmSets |> List.skip 1 |> Seq.fold compatibleTfmsOfTwoSets firstSet |> Set.ofSeq

let bestTfm (tfms: string seq) : string option =
    let frameworks = tfms |> Seq.map NuGetFramework.Parse |> List.ofSeq

    match frameworks with
    | [] -> None
    | fxes ->
        let compatibleWithAllOtherFxes candidate =
            fxes |> Seq.forall (frameworkIsCompatible candidate)

        fxes
        |> Seq.maxBy (fun fx -> compatibleWithAllOtherFxes fx, fx.HasPlatform, fx.Version)
        |> _.GetShortFolderName()
        |> Some

let workspaceTargetFramework (tfmsPerProject: Map<string, string list>) : option<string> =
    match tfmsPerProject.Count with
    | 0 -> None
    | _ ->
        tfmsPerProject.Values
        |> Seq.map Set.ofSeq
        |> List.ofSeq
        |> compatibleTfmSet
        |> bestTfm

let resolveDefaultWorkspaceProps (targetFramework: string option) : Map<string, string> =
    let applyTargetFrameworkProp props =
        match targetFramework with
        | Some tfm -> props |> Map.add "TargetFramework" tfm
        | None -> props

    Map.empty |> applyTargetFrameworkProp

let solutionGetProjectForPath (solution: Solution) (filePath: string) : Project option =
    let docDir = Path.GetDirectoryName filePath

    let fileOnProjectDir (p: Project) =
        let projectDir = Path.GetDirectoryName p.FilePath
        let projectDirWithDirSepChar = projectDir + string Path.DirectorySeparatorChar

        docDir = projectDir || docDir.StartsWith projectDirWithDirSepChar

    solution.Projects |> Seq.filter fileOnProjectDir |> Seq.tryHead

let solutionTryAddDocument (docFilePath: string) (text: string) (solution: Solution) : Async<Document option> = async {
    let projectOnPath = solutionGetProjectForPath solution docFilePath

    let! newDocumentMaybe =
        match projectOnPath with
        | Some proj ->
            let projectBaseDir = Path.GetDirectoryName proj.FilePath
            let docName = docFilePath.Substring(projectBaseDir.Length + 1)

            let newDoc =
                proj.AddDocument(name = docName, text = SourceText.From text, folders = null, filePath = docFilePath)

            Some newDoc |> async.Return

        | None -> async {
            logger.LogTrace("No parent project could be resolved to add file \"{file}\" to workspace", docFilePath)
            return None
          }

    return newDocumentMaybe
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

let solutionTryLoadOnPath (lspClient: ILspClient) (solutionPath: string) =
    assert Path.IsPathRooted solutionPath

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
            do! logMessage (sprintf "Loading solution \"%s\"..." solutionPath)

            let projs = solutionLoadProjectFilenames solutionPath

            let tfmsPerProject = loadProjectTfms projs
            let tfmToUse = workspaceTargetFramework tfmsPerProject
            let workspaceProps = resolveDefaultWorkspaceProps tfmToUse
            logger.LogInformation("Will use MSBuild props: {workspaceProps}", string workspaceProps)

            let msbuildWorkspace =
                MSBuildWorkspace.Create(workspaceProps, CSharpLspHostServices())

            msbuildWorkspace.LoadMetadataForReferencedProjects <- true

            let! solution = msbuildWorkspace.OpenSolutionAsync solutionPath |> Async.AwaitTask

            for diag in msbuildWorkspace.Diagnostics do
                logger.LogInformation("msbuildWorkspace.Diagnostics: {message}", diag.ToString())

                do! logMessage (sprintf "msbuildWorkspace.Diagnostics: %s" (diag.ToString()))

            do! logMessage (sprintf "Finished loading solution \"%s\"" solutionPath)

            return Some solution
        with ex ->
            let errorMessage =
                sprintf "Solution \"%s\" could not be loaded: %s" solutionPath (ex.ToString())

            do! showMessage errorMessage
            return None
    }

let solutionTryLoadFromProjectFiles
    (lspClient: ILspClient)
    (logMessage: string -> Async<unit>)
    (progressReport: string * uint -> Async<unit>)
    (projs: string list)
    =
    async {
        do! progressReport ($"Loading {projs.Length} project(s)...", 0u)
        let loadedProj = ref 0

        let tfmsPerProject = loadProjectTfms projs
        let tmfToUse = workspaceTargetFramework tfmsPerProject
        let workspaceProps = resolveDefaultWorkspaceProps tmfToUse
        logger.LogInformation("Will use MSBuild props: {workspaceProps}", string workspaceProps)

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
            do! progressReport ($"{projName} {loaded}/{projs.Length}", percent)

        for diag in msbuildWorkspace.Diagnostics do
            logger.LogTrace("msbuildWorkspace.Diagnostics: {message}", diag.ToString())

        //workspace <- Some(msbuildWorkspace :> Workspace)
        return Some msbuildWorkspace.CurrentSolution
    }

let solutionFindAndLoadOnDir (progressReporter: ProgressReporter) (lspClient: ILspClient) dir = async {
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

        let progressReport (message, percent) =
            progressReporter.Report(false, message, percent)

        return! solutionTryLoadFromProjectFiles lspClient logMessage progressReport projFiles

    | Some solutionPath -> return! solutionTryLoadOnPath lspClient solutionPath
}

let solutionLoadSolutionWithPathOrOnDir
    (lspClient: ILspClient)
    (progressReporter: ProgressReporter)
    (solutionPathMaybe: string option)
    (dir: string)
    =
    match solutionPathMaybe with
    | Some solutionPath -> async {
        let rootedSolutionPath =
            match Path.IsPathRooted solutionPath with
            | true -> solutionPath
            | false -> Path.Combine(dir, solutionPath)

        return! solutionTryLoadOnPath lspClient rootedSolutionPath
      }

    | None -> async {
        let logMessage: LogMessageParams =
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: attempting to find and load solution on path \"%s\".." dir }

        do! lspClient.WindowLogMessage logMessage

        return! solutionFindAndLoadOnDir progressReporter lspClient dir
      }

let solutionGetRazorDocumentForPath
    (solution: Solution)
    (cshtmlPath: string)
    : Async<(Project * Compilation * SyntaxTree) option> =
    async {
        let normalizedTargetDir = cshtmlPath |> Path.GetDirectoryName |> Path.GetFullPath

        let projectForPath =
            solution.Projects
            |> Seq.tryFind (fun project ->
                let projectDirectory = Path.GetDirectoryName(project.FilePath)
                let normalizedProjectDir = Path.GetFullPath(projectDirectory)

                normalizedTargetDir.StartsWith(
                    normalizedProjectDir + Path.DirectorySeparatorChar.ToString(),
                    StringComparison.OrdinalIgnoreCase
                ))

        match projectForPath with
        | None -> return None
        | Some project ->
            let projectBaseDir = Path.GetDirectoryName project.FilePath

            let! compilation = project.GetCompilationAsync() |> Async.AwaitTask

            let mutable cshtmlTree: SyntaxTree option = None

            let cshtmlPathTranslated =
                Path.GetRelativePath(projectBaseDir, cshtmlPath)
                |> _.Replace(".", "_")
                |> _.Replace(Path.DirectorySeparatorChar, '_')
                |> (fun s -> s + ".g.cs")

            for tree in compilation.SyntaxTrees do
                let path = tree.FilePath

                if path.StartsWith projectBaseDir then
                    let relativePath = Path.GetRelativePath(projectBaseDir, path)

                    if relativePath.EndsWith cshtmlPathTranslated then
                        cshtmlTree <- Some tree

            return cshtmlTree |> Option.map (fun cst -> (project, compilation, cst))
    }

let solutionFindSymbolForRazorDocumentPath solution cshtmlPath pos = async {
    match! solutionGetRazorDocumentForPath solution cshtmlPath with
    | None -> return None

    | Some(project, compilation, cshtmlTree) ->
        let model = compilation.GetSemanticModel cshtmlTree

        let root = cshtmlTree.GetRoot()

        let token =
            root.DescendantTokens()
            |> Seq.tryFind (fun t ->
                let span = cshtmlTree.GetMappedLineSpan(t.Span)

                span.Path = cshtmlPath
                && span.StartLinePosition.Line <= (int pos.Line)
                && span.EndLinePosition.Line >= (int pos.Line)
                && span.StartLinePosition.Character <= (int pos.Character)
                && span.EndLinePosition.Character > (int pos.Character))

        let symbol =
            token
            |> Option.bind (fun x -> x.Parent |> Option.ofObj)
            |> Option.map (fun parentToken -> model.GetSymbolInfo(parentToken))
            |> Option.bind (fun x -> x.Symbol |> Option.ofObj)

        return symbol |> Option.map (fun sym -> (sym, project, None))
}

let solutionSemanticModelMappedPositionAndToken (cshtmlTree: SyntaxTree) pos =
    let root = cshtmlTree.GetRoot()

    let mutable positionAndToken: (int * SyntaxToken) option = None

    for t in root.DescendantTokens() do
        let cshtmlSpan = cshtmlTree.GetMappedLineSpan t.Span

        if
            cshtmlSpan.StartLinePosition.Line = (int pos.Line)
            && cshtmlSpan.EndLinePosition.Line = (int pos.Line)
            && cshtmlSpan.StartLinePosition.Character <= (int pos.Character)
        then
            let tokenStartCharacterOffset =
                (int pos.Character) - cshtmlSpan.StartLinePosition.Character

            positionAndToken <- Some(t.Span.Start + tokenStartCharacterOffset, t)

    positionAndToken
