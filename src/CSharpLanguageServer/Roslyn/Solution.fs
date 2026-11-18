module CSharpLanguageServer.Roslyn.Solution

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Collections.Generic
open System.Text.Json
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

/// Tells whether a project of this solution survives its `.slnf` filter, answering
/// true for every project when the solution is not a filter.
///
/// MSBuild applies the filter internally but exposes no public way to query it, so
/// this reaches its own `ProjectShouldBuild` rather than reimplementing the match:
/// that normalizes path separators and compares case-sensitively on Linux only, and
/// a hand-written comparison is what would make us disagree with the build about
/// which projects a filter selects.
///
/// Unlike the Roslyn internals reflected elsewhere in this server, Microsoft.Build
/// is whichever assembly MSBuildLocator found in the installed SDK, so a member that
/// stops existing must not fail the whole solution load: it degrades to loading
/// every project the solution declares.
let private projectShouldBuildPredicate (solutionFile: SolutionFile) : string -> bool =
    match typeof<SolutionFile>.GetMethod("ProjectShouldBuild", BindingFlags.Instance ||| BindingFlags.NonPublic) with
    | null ->
        logger.LogWarning(
            "MSBuild SolutionFile has no ProjectShouldBuild method; .slnf solution filters will be ignored"
        )

        fun _ -> true

    | projectShouldBuild ->
        fun relativePath ->
            projectShouldBuild.Invoke(solutionFile, [| relativePath |])
            |> nonNull "SolutionFile.ProjectShouldBuild()"
            |> unbox<bool>

let solutionLoadProjectFilenames (solutionPath: string) =
    assert Path.IsPathRooted solutionPath
    let projectFilenames = new List<string>()

    let solutionFile = SolutionFile.Parse solutionPath

    // For a .slnf, ProjectsInOrder lists every project of the PARENT solution, not
    // just the filtered set. Without applying the filter a filtered solution
    // over-counts the load-progress denominator (reports like "(74/358)" stuck at
    // 20% on dotnet/roslyn's Compilers.slnf) and widens the workspace-global TFM
    // intersection to projects the filter excludes.
    let projectShouldBuild = projectShouldBuildPredicate solutionFile

    for project in solutionFile.ProjectsInOrder do
        if
            project.ProjectType = SolutionProjectType.KnownToBeMSBuildFormat
            && projectShouldBuild project.RelativePath
        then
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

/// Normalise a declared TFM to its canonical short folder name so that equivalent
/// spellings intersect correctly.  Returns None for values NuGet cannot make sense of
/// (GetShortFolderName throws on unsupported frameworks).
let tryNormalizeTfm (tfm: string) : string option =
    try
        let fx = NuGetFramework.Parse tfm

        if fx.IsUnsupported then
            None
        else
            Some(fx.GetShortFolderName())
    with _ ->
        None

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

/// The TFM to hand to MSBuild as a workspace-global property, or None when no single TFM
/// is safe for every project.  Only a TFM declared by *every* project qualifies: MSBuild
/// global properties override each project's own <TargetFramework>, and a project that was
/// never restored for that TFM has no matching target in its project.assets.json, so its
/// design-time build fails with NETSDK1005 and loses every package and framework reference.
let workspaceTargetFramework (tfmsPerProject: Map<string, string list>) : option<string> =
    match
        tfmsPerProject.Values
        |> Seq.map (Seq.choose tryNormalizeTfm >> Set.ofSeq)
        |> List.ofSeq
    with
    | [] -> None
    | first :: rest -> rest |> List.fold Set.intersect first |> bestTfm

let resolveDefaultWorkspaceProps (targetFramework: string option) : Map<string, string> =
    let applyTargetFrameworkProp props =
        match targetFramework with
        | Some tfm -> props |> Map.add "TargetFramework" tfm
        | None -> props

    Map.empty |> applyTargetFrameworkProp

/// Turns MSBuildWorkspace's per-operation load progress into one progress
/// report per project. A project goes through its Evaluate/Build/Resolve
/// operations once per target framework, so completion is counted on the
/// first Resolve seen for each project file.
type private SolutionLoadProgressObserver(totalProjectCount: int, progressReport: string * uint -> Async<unit>) =
    let completedProjects = HashSet<string>()
    let syncRoot = obj ()

    // Reports are forwarded through a mailbox so they reach the client in
    // the order they were produced without blocking the loader's callback.
    let reportQueue =
        MailboxProcessor<string * uint>.Start(fun inbox -> async {
            while true do
                let! report = inbox.Receive()
                do! progressReport report
        })

    interface IProgress<ProjectLoadProgress> with
        member _.Report(loadProgress: ProjectLoadProgress) =
            if loadProgress.Operation = ProjectLoadOperation.Resolve then
                let reportMaybe =
                    lock syncRoot (fun () ->
                        match completedProjects.Add loadProgress.FilePath with
                        | false -> None
                        | true ->
                            // The workspace may load more projects than the
                            // solution file declares — LoadMetadataForReferencedProjects
                            // follows references to non-member projects — so the
                            // denominator widens as extras appear, keeping the
                            // percentage monotonic and at most 100.
                            let total = max totalProjectCount completedProjects.Count
                            let percent = uint (100 * completedProjects.Count / max 1 total)
                            let projectName = Path.GetFileNameWithoutExtension loadProgress.FilePath

                            Some(sprintf "%s (%d/%d)" projectName completedProjects.Count total, percent))

                reportMaybe |> Option.iter reportQueue.Post

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

/// Short, actionable summary of a solution load failure for window/showMessage; the
/// full exception goes to the log.  Roslyn's build host wraps the real error in an
/// AggregateException/RemoteInvocationException, so we search the whole text for it.
let describeSolutionLoadFailure (solutionPath: string) (ex: exn) : string =
    let text = ex.ToString()

    let tryCapture (pattern: string) =
        let m = Text.RegularExpressions.Regex.Match(text, pattern)
        if m.Success then Some(m.Groups[1].Value.Trim()) else None

    if text.Contains "A compatible .NET SDK was not found" then
        let requested = tryCapture @"Requested SDK version:\s*(\S+)"
        let globalJson = tryCapture @"global\.json file:\s*(.+)"

        match requested, globalJson with
        | Some version, Some globalJsonPath ->
            sprintf
                "Solution \"%s\" could not be loaded: .NET SDK %s, requested by \"%s\", is not installed. Install that SDK (`dotnet --list-sdks` shows what is installed) or update global.json, then reload."
                solutionPath
                version
                globalJsonPath
        | _ ->
            sprintf
                "Solution \"%s\" could not be loaded: no compatible .NET SDK was found. Check that a .NET SDK is installed and matches any global.json, then reload."
                solutionPath
    else
        let rec innermost (e: exn) =
            match e with
            | :? AggregateException as ae when ae.InnerExceptions.Count = 1 -> innermost ae.InnerExceptions[0]
            | e when not (isNull e.InnerException) -> innermost e.InnerException
            | e -> e

        let firstLine =
            (innermost ex).Message.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryHead
            |> Option.map _.Trim()
            |> Option.defaultValue (ex.GetType().Name)

        sprintf
            "Solution \"%s\" could not be loaded: %s (see the csharp-ls output log for details)"
            solutionPath
            firstLine

let solutionTryLoadOnPath
    (lspClient: ILspClient)
    (progressReport: string * uint -> Async<unit>)
    (solutionPath: string)
    : Async<option<Workspace * Solution>> =
    assert Path.IsPathRooted solutionPath

    let logMessage m =
        lspClient.WindowLogMessage
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m }

    let showError m =
        lspClient.WindowShowMessage
            { Type = MessageType.Error
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

            let loadProgressObserver =
                SolutionLoadProgressObserver(Set.count projs, progressReport)

            let! solution =
                msbuildWorkspace.OpenSolutionAsync(solutionPath, progress = loadProgressObserver)
                |> Async.AwaitTask

            for diag in msbuildWorkspace.Diagnostics do
                logger.LogInformation("msbuildWorkspace.Diagnostics: {message}", diag.ToString())

                do! logMessage (sprintf "msbuildWorkspace.Diagnostics: %s" (diag.ToString()))

            do! logMessage (sprintf "Finished loading solution \"%s\"" solutionPath)

            return Some(msbuildWorkspace, solution)
        with ex ->
            logger.LogError(ex, "Solution {solutionPath} could not be loaded", solutionPath)
            do! logMessage (sprintf "Solution \"%s\" could not be loaded: %s" solutionPath (ex.ToString()))
            do! showError (describeSolutionLoadFailure solutionPath ex)
            return None
    }

let solutionTryLoadFromProjectFiles
    (lspClient: ILspClient)
    (logMessage: string -> Async<unit>)
    (progressReport: string * uint -> Async<unit>)
    (projs: string list)
    : Async<option<Workspace * Solution>> =
    async {
        do! progressReport ($"{projs.Length} project(s)", 0u)
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
            let projName = Path.GetFileNameWithoutExtension projectFile.Name
            let loaded = Interlocked.Increment loadedProj
            let percent = 100 * loaded / projs.Length |> uint
            do! progressReport ($"{projName} ({loaded}/{projs.Length})", percent)

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

        | Some solutionPath ->
            let progressReport (message, percent) =
                progressReporter.Report(false, message, percent)

            return! solutionTryLoadOnPath lspClient progressReport solutionPath
    }

/// Finds the NuGet restore output of a project: `obj/project.assets.json` unless the project
/// moves its intermediate output (e.g. `UseArtifactsOutput`), which only an MSBuild evaluation
/// can tell, so one is done only when the default location has no file. Some(None) means the
/// project has no assets file to look for (non-SDK-style, packages.config), None that it is missing.
let private tryFindProjectAssetsFile (projectFilename: string) : string option option =
    let projectDir =
        Path.GetDirectoryName projectFilename
        |> nonNull "Path.GetDirectoryName(projectFilename)"

    let defaultAssetsFile = Path.Combine(projectDir, "obj", "project.assets.json")

    if File.Exists defaultAssetsFile then
        Some(Some defaultAssetsFile)
    else
        use projectCollection = new Microsoft.Build.Evaluation.ProjectCollection()

        try
            let buildProject =
                projectCollection.LoadProject(projectFilename, Dictionary<string, string>(), toolsVersion = null)

            match buildProject.GetPropertyValue "ProjectAssetsFile" |> noneIfEmpty with
            | Some assetsFile when File.Exists assetsFile -> Some(Some assetsFile)
            | Some _ -> None
            | None -> Some None
        with :? InvalidProjectFileException ->
            // A restore can't fix a project MSBuild can't evaluate.
            Some None

/// Names of the packages a project file references with a literal, unconditional
/// `<PackageReference Include="...">`. Only the project file itself is read, as that is what
/// gets edited by hand; whether a conditional reference applies would need an evaluation.
let private tryGetLiteralPackageReferences (projectFilename: string) : string list =
    try
        let hasCondition (el: XElement) =
            el.AncestorsAndSelf()
            |> Seq.exists (fun e -> e.Attribute(XName.Get "Condition") <> null)

        XDocument.Load(projectFilename).Descendants()
        |> Seq.filter (fun el -> el.Name.LocalName = "PackageReference" && not (hasCondition el))
        |> Seq.choose (fun el -> el.Attribute(XName.Get "Include") |> Option.ofObj |> Option.map _.Value)
        |> Seq.collect _.Split(';')
        |> Seq.map _.Trim()
        |> Seq.filter (fun name -> name <> "" && not (name.Contains "$("))
        |> List.ofSeq
    with _ ->
        []

/// Names of the packages a project.assets.json was restored with as direct dependencies,
/// over all target frameworks; None if the file can't be read.
let private tryGetAssetsFileDirectDependencies (assetsFile: string) : Set<string> option =
    try
        use assets = JsonDocument.Parse(File.ReadAllText assetsFile)

        assets.RootElement.GetProperty("project").GetProperty("frameworks").EnumerateObject()
        |> Seq.collect (fun framework ->
            match framework.Value.TryGetProperty "dependencies" with
            | true, dependencies -> dependencies.EnumerateObject() |> Seq.map _.Name |> List.ofSeq
            | false, _ -> [])
        |> Seq.map _.ToLowerInvariant()
        |> Set.ofSeq
        |> Some
    with _ ->
        None

/// Tells whether a project's NuGet restore output is missing or out of date, as Roslyn's own
/// language server checks: no assets file, or a package reference the assets file lacks (one
/// added by hand without restoring).
let private projectAssetsNeedRestore (projectFilename: string) : bool =
    match tryFindProjectAssetsFile projectFilename with
    | None -> true
    | Some None -> false
    | Some(Some assetsFile) ->
        match tryGetLiteralPackageReferences projectFilename with
        | [] -> false
        | packageReferences ->
            match tryGetAssetsFileDirectDependencies assetsFile with
            | None -> true
            | Some restoredPackages ->
                packageReferences
                |> List.exists (fun name -> not (restoredPackages.Contains(name.ToLowerInvariant())))

let private coreLibraryFileNames =
    HashSet<string>(
        [ "System.Runtime.dll"
          "System.Private.CoreLib.dll"
          "netstandard.dll"
          "mscorlib.dll" ],
        StringComparer.OrdinalIgnoreCase
    )

/// A design-time build that can't find the project's targeting pack doesn't fail: the SDK
/// turns that error off for design-time builds, so the project just loads with no framework
/// references. This happens when the project was restored by an SDK other than the one now
/// evaluating it, e.g. a net10.0 project restored with SDK 10 (whose targeting pack ships in
/// dotnet/packs) and then evaluated by SDK 11 (which downloads a newer 10.0.x one on restore).
let private projectHasCoreLibraryReference (project: Project) : bool =
    project.MetadataReferences
    |> Seq.exists (fun reference ->
        match reference with
        | :? PortableExecutableReference as peReference ->
            peReference.FilePath
            |> Option.ofObj
            |> Option.exists (Path.GetFileName >> coreLibraryFileNames.Contains)
        | _ -> false)

/// Project files of the C# projects in the solution that look like they need a NuGet restore:
/// ones with missing or out of date restore output (see `projectAssetsNeedRestore`), and ones
/// restored without the targeting pack they now need (see `projectHasCoreLibraryReference`).
let solutionProjectsNeedingRestore (solution: Solution) : string list =
    solution.Projects
    |> Seq.filter (fun project -> project.Language = LanguageNames.CSharp)
    |> Seq.choose (fun project -> project.FilePath |> Option.ofObj |> Option.map (fun path -> path, project))
    // A multi-targeted project is loaded as one Roslyn project per target framework.
    |> Seq.groupBy fst
    |> Seq.filter (fun (projectFilename, projects) ->
        not (projects |> Seq.forall (snd >> projectHasCoreLibraryReference))
        || projectAssetsNeedRestore projectFilename)
    |> Seq.map fst
    |> List.ofSeq

/// Runs `dotnet restore` on a solution or project file, in its own directory so that the SDK
/// is picked the same way as for the design-time build (global.json is honored).
let private runDotnetRestore (restoreTarget: string) : Async<bool> = async {
    let psi = ProcessStartInfo("dotnet")
    psi.ArgumentList.Add "restore"
    psi.ArgumentList.Add restoreTarget
    // Worker nodes outlive the restore and would inherit the redirected pipes, so reading
    // them to the end would wait for the nodes rather than the restore.
    psi.ArgumentList.Add "-nodeReuse:false"
    psi.WorkingDirectory <- Path.GetDirectoryName restoreTarget
    // Our own stdin/stdout carry the LSP connection: don't let the child inherit them.
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    // MSBuildLocator points these at the SDK it registered in this process, which needn't
    // be the one `dotnet` picks for the restore target; left in place, the CLI runs that
    // other SDK's MSBuild and fails without any output.
    for variable in [ "MSBUILD_EXE_PATH"; "MSBuildSDKsPath"; "MSBuildExtensionsPath" ] do
        psi.Environment.Remove variable |> ignore

    use proc = Process.Start psi |> nonNull "Process.Start(dotnet restore)"
    proc.StandardInput.Close()

    let stdoutTask = proc.StandardOutput.ReadToEndAsync()
    let stderrTask = proc.StandardError.ReadToEndAsync()
    do! proc.WaitForExitAsync() |> Async.AwaitTask
    let! stdout = stdoutTask |> Async.AwaitTask
    let! stderr = stderrTask |> Async.AwaitTask

    if proc.ExitCode = 0 then
        logger.LogInformation("dotnet restore \"{target}\" succeeded", restoreTarget)
    else
        logger.LogWarning(
            "dotnet restore \"{target}\" failed with exit code {exitCode}:\n{stdout}\n{stderr}",
            restoreTarget,
            proc.ExitCode,
            stdout,
            stderr
        )

    return proc.ExitCode = 0
}

let private showProjectsStillNeedingRestore (lspClient: ILspClient) (projectFilenames: string list) =
    lspClient.WindowShowMessage
        { Type = MessageType.Warning
          Message =
            sprintf
                "csharp-ls: %d project(s) still look unrestored after `dotnet restore`, so some of their references are missing: %s"
                projectFilenames.Length
                (String.Join(", ", projectFilenames)) }

let private solutionLoadSolutionWithPathOrOnDirOnce
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

        let progressReport (message, percent) =
            progressReporter.Report(false, message, percent)

        return! solutionTryLoadOnPath lspClient progressReport rootedSolutionPath
      }

    | None -> async {
        let logMessage: LogMessageParams =
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: attempting to find and load solution on path \"%s\".." dir }

        do! lspClient.WindowLogMessage logMessage

        return! solutionFindAndLoadOnDir progressReporter lspClient dir
      }

/// Loads the solution and, like Roslyn's own language server, restores the projects that
/// turn out to need it (`solutionProjectsNeedingRestore`) and loads the solution again.
let solutionLoadSolutionWithPathOrOnDir
    (lspClient: ILspClient)
    (progressReporter: ProgressReporter)
    (solutionPathMaybe: string option)
    (dir: string)
    : Async<option<Workspace * Solution>> =
    let load () =
        solutionLoadSolutionWithPathOrOnDirOnce lspClient progressReporter solutionPathMaybe dir

    async {
        match! load () with
        | Some(workspace, solution) as loaded ->
            match solutionProjectsNeedingRestore solution with
            | [] -> return loaded

            | projectFilenames ->
                // One restore of the whole solution rather than one per project: restoring
                // projects that don't need it is a quick no-op.
                let restoreTargets =
                    match solution.FilePath |> Option.ofObj with
                    | Some solutionPath -> [ solutionPath ]
                    | None -> projectFilenames

                workspace.Dispose()

                for restoreTarget in restoreTargets do
                    let restoreMessage =
                        match restoreTargets with
                        | [ _ ] -> "restoring packages"
                        | _ -> sprintf "restoring %s" (Path.GetFileNameWithoutExtension restoreTarget)

                    do! progressReporter.Report(false, restoreMessage)
                    let! _ = runDotnetRestore restoreTarget
                    ()

                match! load () with
                | Some(_, reloadedSolution) as reloaded ->
                    match solutionProjectsNeedingRestore reloadedSolution with
                    | [] -> ()
                    | stillNeedingRestore -> do! showProjectsStillNeedingRestore lspClient stillNeedingRestore

                    return reloaded
                | None -> return None

        | None -> return None
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
                    d.HintName.EndsWith cshtmlHintNameFlat
                    || d.HintName.EndsWith cshtmlHintNameHierarchical)

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

        // Roslyn's newer "range + character-offset" #line directive format (used by the
        // Razor generator since SDK 10.0.400 / Roslyn 5.9) maps tokens that precede the
        // offset threshold within a directive block (e.g. the "Write(" call wrapping a
        // Razor expression) to the *entire* enclosing mapped span rather than a precise
        // sub-range — only tokens at/after the offset (e.g. the expression itself) get an
        // accurate per-character mapping. A plain "first token whose mapped span contains
        // the position" search can therefore match an overly-broad outer token (like
        // "Write") ahead of the precise inner token. Prefer the candidate with the
        // narrowest mapped span instead, so the most specific match wins.
        let token =
            root.DescendantTokens()
            |> Seq.choose (fun t ->
                let span = cshtmlTree.GetMappedLineSpan(t.Span)

                if
                    span.Path = cshtmlPath
                    && span.StartLinePosition.Line <= (int pos.Line)
                    && span.EndLinePosition.Line >= (int pos.Line)
                    && span.StartLinePosition.Character <= (int pos.Character)
                    && span.EndLinePosition.Character > (int pos.Character)
                then
                    Some(t, span)
                else
                    None)
            |> Seq.sortBy (fun (_, span) ->
                (span.EndLinePosition.Line - span.StartLinePosition.Line,
                 span.EndLinePosition.Character - span.StartLinePosition.Character))
            |> Seq.tryHead
            |> Option.map fst

        let symbol =
            token
            |> Option.bind (fun x -> x.Parent |> Option.ofObj)
            |> Option.map (fun parentToken -> model.GetSymbolInfo(parentToken))
            |> Option.bind (fun x -> x.Symbol |> Option.ofObj)

        return symbol |> Option.map (fun sym -> (sym, project, None))
}
