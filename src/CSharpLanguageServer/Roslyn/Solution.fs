module CSharpLanguageServer.Roslyn.Solution

open System
open System.IO
open System.Threading
open System.Collections.Generic
open System.Xml.Linq

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


/// Try to read TargetFramework / TargetFrameworks directly from the project XML without
/// invoking any MSBuild machinery.  Returns Some(tfms) when every value found is a plain
/// string literal (contains no "$(" property reference), or None when a value needs full
/// MSBuild evaluation or neither property appears in the file.
let private tryGetTfmsFromXml (projectFilename: string) : string list option =
    try
        let xdoc = XDocument.Load projectFilename
        let root = xdoc.Root |> Option.ofObj

        let tryText localName =
            root
            |> Option.map (fun r ->
                let ns = r.Name.Namespace

                r.Descendants(ns + localName)
                |> Seq.tryPick (fun el ->
                    let v = el.Value.Trim()
                    if String.IsNullOrEmpty v then None else Some v))
            |> Option.flatten

        let isTrivial (s: string) = not (s.Contains("$("))

        match tryText "TargetFramework", tryText "TargetFrameworks" with
        | Some tfm, _ when not (isTrivial tfm) -> None
        | _, Some tfms when not (isTrivial tfms) -> None
        | tfm, tfms ->
            let single = tfm |> Option.map (fun s -> [ s.Trim() ]) |> Option.defaultValue []

            let multi =
                tfms
                |> Option.map (fun s -> s.Split(';') |> Array.map (fun t -> t.Trim()) |> List.ofArray)
                |> Option.defaultValue []

            match single @ multi with
            | [] -> None // neither property found — need MSBuild fallback
            | tfmList -> Some tfmList
    with _ ->
        None // any parse error — let ProjectCollection handle it

/// Slow path: evaluate a project file with MSBuild's ProjectCollection to extract
/// TargetFramework / TargetFrameworks.  The BuildManager singleton serialises concurrent
/// callers internally, so this is safe to call from parallel async workflows.
let private tryGetTfmsFromProjectCollection (projectFilename: string) : string list =
    let projectCollection = new Microsoft.Build.Evaluation.ProjectCollection()
    let props = new Dictionary<string, string>()

    try
        let buildProject =
            projectCollection.LoadProject(projectFilename, props, toolsVersion = null)

        let targetFramework =
            buildProject.GetPropertyValue "TargetFramework"
            |> noneIfEmpty
            |> Option.map (fun tfm -> [ tfm.Trim() ])
            |> Option.defaultValue []

        let targetFrameworks =
            buildProject.GetPropertyValue "TargetFrameworks"
            |> noneIfEmpty
            |> Option.map (fun tfms -> tfms.Split ";" |> Array.map _.Trim() |> List.ofArray)
            |> Option.defaultValue []

        projectCollection.UnloadProject buildProject
        targetFramework @ targetFrameworks
    with :? InvalidProjectFileException as ipfe ->
        logger.LogDebug(
            "loadProjectTfms: failed to load {projectFilename}: {ex}",
            projectFilename,
            ipfe.GetType() |> string
        )

        []

let loadProjectTfms (projs: string seq) : Async<Map<string, list<string>>> =
    let loadOne (projectFilename: string) : Async<string * string list> = async {
        let tfms =
            match tryGetTfmsFromXml projectFilename with
            | Some tfms -> tfms
            | None -> tryGetTfmsFromProjectCollection projectFilename

        return (projectFilename, tfms)
    }

    async {
        let! results = projs |> Seq.map loadOne |> Async.Parallel
        return results |> Array.fold (fun m (k, v) -> m |> Map.add k v) Map.empty
    }

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

let solutionTryLoadOnPath (lspClient: ILspClient) (solutionPath: string) : Async<option<Workspace * Solution>> =
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

            let! tfmsPerProject = loadProjectTfms projs
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

            return Some(msbuildWorkspace, solution)
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
    : Async<option<Workspace * Solution>> =
    async {
        do! progressReport ($"Loading {projs.Length} project(s)...", 0u)
        let loadedProj = ref 0

        let! tfmsPerProject = loadProjectTfms projs
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
        return Some(msbuildWorkspace, msbuildWorkspace.CurrentSolution)
    }

let solutionFindAndLoadOnDir
    (progressReporter: ProgressReporter)
    (lspClient: ILspClient)
    dir
    : Async<option<Workspace * Solution>> =
    async {
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
    : Async<option<Workspace * Solution>> =
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

            // SDK 10.0.300+: Razor is an IIncrementalGenerator; generated files live in
            // SourceGeneratedDocuments, not in compilation.SyntaxTrees.
            // MSBuildWorkspace populates project.AnalyzerConfigDocuments from the
            // EditorConfigFiles MSBuild items produced by GenerateMSBuildEditorConfigFile
            // during its own design-time build, so the generator already has all the
            // build_property.* options it needs — no manual injection required.
            //
            // The Razor generator derives HintNames from the base64-decoded TargetPath in
            // the editorconfig. The exact separator convention has varied across SDK/Roslyn
            // versions:
            //   - SDK 10.0.1xx (Roslyn ~5.0-5.3): directory separators AND the extension dot
            //     are both replaced with '_', e.g.
            //     Views/Test/ViewFileWithErrors.cshtml → Views_Test_ViewFileWithErrors_cshtml.g.cs
            //   - SDK 10.0.300+ (Roslyn ~5.5+): directory separators are preserved as '/'
            //     (Roslyn supports hierarchical hint names) and only the extension dot is
            //     replaced, e.g.
            //     Views/Test/ViewFileWithErrors.cshtml → Views/Test/ViewFileWithErrors_cshtml.g.cs
            // Match against both formats for forward/backward compatibility.
            let relativeCshtmlPath = Path.GetRelativePath(projectBaseDir, cshtmlPath)

            let cshtmlHintNameFlat =
                relativeCshtmlPath
                |> _.Replace(".", "_")
                |> _.Replace(Path.DirectorySeparatorChar, '_')
                |> (fun s -> s + ".g.cs")

            let cshtmlHintNameHierarchical =
                relativeCshtmlPath
                |> _.Replace(Path.DirectorySeparatorChar, '/')
                |> _.Replace(".", "_")
                |> (fun s -> s + ".g.cs")

            let! ct = Async.CancellationToken

            let! generatedDocs = project.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask

            let razorGenDoc =
                generatedDocs
                |> Seq.tryFind (fun d ->
                    d.HintName.EndsWith cshtmlHintNameFlat || d.HintName.EndsWith cshtmlHintNameHierarchical)

            match razorGenDoc with
            | None -> return None
            | Some doc ->
                let! tree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

                match tree |> Option.ofObj with
                | None -> return None
                | Some tree ->
                    let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
                    return compilation |> Option.ofObj |> Option.map (fun c -> (project, c, tree))
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
