namespace CSharpLanguageServer.Workspace

open System.Collections.Concurrent
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FSharpPlus

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Workspace.Util

type private FileInfo =
    { Version: int
      Debouncer: Debounce option }

type DecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type WorkspaceManager(lspClient: ICSharpLspClient) =

    let workspaces: ConcurrentDictionary<DocumentUri, Workspace> = ConcurrentDictionary()
    let workspaceUpdaters: ConcurrentDictionary<DocumentUri, Debounce> = ConcurrentDictionary()
    let files: ConcurrentDictionary<DocumentUri, FileInfo> = ConcurrentDictionary()
    // TODO: Remove item from the dictionary if it isn't used for some time?
    let decompiledMetadata: ConcurrentDictionary<DocumentUri, DecompiledMetadataDocument> = ConcurrentDictionary()
    let initialized: TaskCompletionSource<bool> = TaskCompletionSource<bool>()

    let logger = LogProvider.getLoggerByName "WorkspaceManager"

    member private this.loadWorkspace (workspace: MSBuildWorkspace) (uri: DocumentUri) : Async<unit> = async {
        workspace.LoadMetadataForReferencedProjects <- true

        let root = Uri.toPath uri

        match findInteresting root with
        | None ->
            // TODO: Create a sln/csproj file so that it can be used for single cs file?
            logger.info (
                Log.setMessage "Can't find .sln/.csproj files under {path}"
                >> Log.addContext "path" root
            )
        | Some(Solution sln) ->
            logger.info (Log.setMessage "Start to load {slnPath}..." >> Log.addContext "slnPath" sln)
            try
                let progress = ProgressReporter(lspClient)
                do! progress.Begin("Loading solution...")
                do! workspace.OpenSolutionAsync(sln) |> Async.AwaitTask |> Async.Ignore
                for diag in workspace.Diagnostics do
                    logger.warn (
                        Log.setMessage "Diagnostic during load {slnPath}: {diag}"
                        >> Log.addContext "slnPath" sln
                        >> Log.addContext "diag" (diag.ToString())
                    )
                do! progress.End()
            with ex ->
                logger.error (
                    Log.setMessage "Failed to load {slnPath}:"
                    >> Log.addContext "slnPath" sln
                    >> Log.addException ex
                )
            logger.info (Log.setMessage "Finish to load {slnPath}" >> Log.addContext "slnPath" sln)
        | Some(Projects projs) ->
            logger.info (
                Log.setMessage "Start to load {projNum} projects..."
                >> Log.addContext "projNum" projs.Length
            )

            let progress = ProgressReporter(lspClient)
            do! progress.Begin($"Loading {projs.Length} projects...", false, $"0/{projs.Length}", 0u)
            let loadedProj = ref 0
            do!
                projs
                |> map (fun proj -> async {
                    logger.info (Log.setMessage "Start to load {proj}..." >> Log.addContext "proj" proj)

                    try
                        do! workspace.OpenProjectAsync(proj) |> Async.AwaitTask |> Async.Ignore
                    with ex ->
                        logger.info (
                            Log.setMessage "Failed to load {proj}:"
                            >> Log.addContext "proj" proj
                            >> Log.addException ex
                        )

                    logger.info (Log.setMessage "Finish to load {proj}" >> Log.addContext "proj" proj)
                    let loaded = Interlocked.Increment(loadedProj)
                    let percent = 100 * loaded / projs.Length |> uint
                    do! progress.Report(false, $"{loaded}/{projs.Length}", percent)
                })
                |> Async.Parallel
                |> Async.Ignore
            do! progress.End()

            logger.info (
                Log.setMessage "Finish to load {projNum} projects"
                >> Log.addContext "projNum" projs.Length
            )
            for diag in workspace.Diagnostics do
                logger.warn (
                    Log.setMessage "Diagnostic during load projects: {diag}"
                    >> Log.addContext "diag" (diag.ToString())
                )

    }

    member this.ChangeWorkspaceFolders (added: WorkspaceFolder[]) (removed: WorkspaceFolder[]) : Async<unit> = async {
        // TODO: Add a lock to provide atomic? A group of atomic operations is not atomic.
        removed
        |> map (fun workspaceFolder -> workspaceFolder.Uri)
        |> map workspaces.TryRemove
        |> ignore

        do!
            added
            |> map (fun add -> async {
                let workspace = Workspace.create()
                workspaces.[add.Uri] <- workspace
                do! this.loadWorkspace workspace add.Uri
            })
            |> Async.Parallel
            |> Async.Ignore
    }

    member private this.GetWorkspaceWithDocumentId(uri: DocumentUri) : (Workspace * DocumentId) option =
        let path = Uri.toPath uri

        workspaces.Values
        |> Seq.filter (fun workspace -> not (isNull workspace.CurrentSolution))
        |> Seq.map (fun workspace ->
            (workspace, workspace.CurrentSolution.GetDocumentIdsWithFilePath(path) |> Seq.tryHead))
        |> Seq.filter (snd >> Option.isSome)
        |> Seq.map (second Option.get)
        |> Seq.tryHead

    member this.GetDocument(uri: DocumentUri) : Document option =
        this.GetWorkspaceWithDocumentId uri
        |> Option.map (fun (workspace, docId) -> workspace.CurrentSolution.GetDocument(docId))
        |> Option.bind Option.ofObj
        |> Option.orElse (decompiledMetadata.TryFind (Uri.unescape uri) |> Option.map (fun metadata -> metadata.Document))

    member this.FindSymbol' (uri: DocumentUri) (pos: Position): Async<(ISymbol * Document) option> = async {
        match this.GetDocument uri with
        | None -> return None
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position = Position.toRoslynPosition sourceText.Lines pos
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
            return symbol |> Option.ofObj |> Option.map (fun sym -> sym, doc)
    }

    member private this.FindDerivedClasses' (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> =
        workspaces.Values
        |> Seq.map (fun workspace -> SymbolFinder.FindDerivedClassesAsync(symbol, workspace.CurrentSolution, transitive) |> Async.AwaitTask)
        |> Async.Parallel
        |> map (Seq.collect id)

    member private this.FindDerivedInterfaces' (symbol: INamedTypeSymbol) (transitive: bool):  Async<INamedTypeSymbol seq> =
        workspaces.Values
        |> Seq.map (fun workspace -> SymbolFinder.FindDerivedInterfacesAsync(symbol, workspace.CurrentSolution, transitive) |> Async.AwaitTask)
        |> Async.Parallel
        |> map (Seq.collect id)

    member private this.GetDiagnostics (uri: DocumentUri): Async<Diagnostic array> = async {
        match this.GetDocument uri with
        | None -> return Array.empty
        | Some doc ->
            let! semanticModelMaybe = doc.GetSemanticModelAsync() |> Async.AwaitTask
            match Option.ofObj semanticModelMaybe with
            | None -> return Array.empty
            | Some semanticModel ->
                return semanticModel.GetDiagnostics() |> Seq.map Diagnostic.fromRoslynDiagnostic |> Seq.toArray
    }

    member private this.PublishDiagnostics (uri: DocumentUri): Async<unit> = async {
        let! diagnostics = this.GetDiagnostics uri
        // TODO: Add version info
        do! lspClient.TextDocumentPublishDiagnostics { Uri = uri; Diagnostics = diagnostics; Version = None }
    }

    member private this.UpdateFile (uri: DocumentUri) (version: int) =
        let uri = Uri.unescape uri
        // TODO: If client support pull diagnostics, Debouncer should be None
        let adder = fun uri ->  {
            Version = version
            // TODO: Make timeout configurable?
            Debouncer = Some (Debounce(250, fun () -> this.PublishDiagnostics uri |> Async.Start)) }
        let updater = konst (fun (info: FileInfo) -> { info with Version = max info.Version version })
        let info = files.AddOrUpdate(uri, adder, updater)
        info.Debouncer |> Option.iter (fun debouncer -> debouncer.Bounce())

    member private this.SaveDocument (uri: DocumentUri) (text: string option) = async {
        let tryLoadText uri =
            try
                uri |> Uri.toPath |> File.ReadAllText |> Some
            with _ ->
                None
        let text = text |> Option.orElse (tryLoadText uri)
        match text, this.GetWorkspaceWithDocumentId uri with
        | None, _ -> return ()
        | Some text, Some (workspace, docId) ->
            let solution = workspace.CurrentSolution.WithDocumentText(docId, SourceText.From(text))
            workspace.TryApplyChanges(solution) |> ignore
        | Some text, None ->
            let docPath = Uri.toPath uri
            let docDir = Path.GetDirectoryName(docPath) + string Path.DirectorySeparatorChar
            let fileOnProjectDir (p: Project) =
                let projectDir = Path.GetDirectoryName(p.FilePath) + string Path.DirectorySeparatorChar
                docDir.StartsWith(projectDir)
            match
                workspaces.Values
                |> Seq.collect (fun workspace -> workspace.CurrentSolution.Projects)
                |> Seq.filter fileOnProjectDir
                |> Seq.tryMaxBy (fun p -> Path.GetDirectoryName(p.FilePath).Length)
            with
            | None -> return ()
            | Some proj ->
                let projectDir = Path.GetDirectoryName(proj.FilePath)
                let docName = docPath.Substring(projectDir.Length+1)
                let doc = proj.AddDocument(docName, text, folders=null, filePath=docPath)
                proj.Solution.Workspace.TryApplyChanges(doc.Project.Solution) |> ignore
    }

    member private this.ResolveSymbolLocationsFromMetadata (project: Project) (symbol: ISymbol) (location: Microsoft.CodeAnalysis.Location) = async {
        let fullName = symbol |> getContainingTypeOrThis |> getFullReflectionName
        let uri = $"csharp:/metadata/projects/{project.Name}/assemblies/{location.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
        let! doc =
            match decompiledMetadata.TryFind uri with
            | Some metadata -> async { return metadata.Document }
            | None -> async {
                let! doc, text = makeDocumentFromMetadata project location.MetadataModule fullName
                let metadata =
                    { Metadata =
                        { ProjectName = project.Name
                          AssemblyName = location.MetadataModule.ContainingAssembly.Name
                          SymbolName = fullName
                          Source = text }
                      Document = doc }
                return decompiledMetadata.AddOrUpdate(uri, metadata, (fun _ _ -> metadata)).Document
            }

        let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
        let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, symbol)
        collector.Visit(syntaxTree.GetRoot())
        match collector.GetLocations() with
        | [] -> return [ { Uri = uri
                           Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 0 } } } ]
        | ls -> return ls
    }

    member this.OnSolutionOrProjectUpdate (uri: DocumentUri) changeType =
        let workspaceFolders =
            workspaces.Keys
            |> Seq.filter (fun root -> uri.StartsWith(root + string Path.DirectorySeparatorChar))
        let reload (uri: DocumentUri) =
            workspaces.TryFind uri
            |> Option.iter (fun workspace -> this.loadWorkspace (workspace :?> MSBuildWorkspace) uri |> Async.Start)
            workspaceUpdaters.TryRemove uri |> ignore
        // TODO: make 1s configurable?
        let adder = fun uri -> Debounce(1000, konst (reload uri))

        workspaceFolders
        |> Seq.iter (fun folder -> workspaceUpdaters.AddOrUpdate(folder, adder, flip konst).Bounce())

    interface IWorkspaceManager with
        override this.ChangeWorkspaceFolders added removed =
            this.ChangeWorkspaceFolders added removed

        override this.Initialize(workspaceFolders: WorkspaceFolder list) = async {
            let instance = MSBuildLocator.RegisterDefaults()
            logger.info (
                Log.setMessage "MSBuild environment:\nName: {name}\nVersion: {version}\nMSBuildPath: {msbuildPath}\nVisualStudioRootPath: {vsPath}"
                >> Log.addContext "name" instance.Name
                >> Log.addContext "version" instance.Version
                >> Log.addContext "msbuildPath" instance.MSBuildPath
                >> Log.addContext "vsPath" instance.VisualStudioRootPath
            )
            do! this.ChangeWorkspaceFolders (List.toArray workspaceFolders) Array.empty
            initialized.SetResult(true)
            let registrationParams = { Registrations = getRegistrations lspClient.Capabilities |> List.toArray }
            // TODO: Retry on error?
            do! lspClient.ClientRegisterCapability registrationParams |> Async.Ignore
        }

        override this.WaitInitialized() = async {
            do! initialized.Task |> Async.AwaitTask |> Async.Ignore
        }

        override this.GetDocument(uri: DocumentUri) : Document option = this.GetDocument uri

        override this.GetDiagnostics (uri: DocumentUri): Async<Diagnostic array> = this.GetDiagnostics uri

        override this.FindMetadata (uri: DocumentUri): CSharpMetadataInformation option =
            Uri.unescape uri |> decompiledMetadata.TryFind |> Option.map (fun metadata -> metadata.Metadata)

        override this.FindSymbol (uri: DocumentUri) (pos: Position): Async<ISymbol option> =
            this.FindSymbol' uri pos |> map (Option.map fst)

        override this.FindSymbol' (uri: DocumentUri) (pos: Position): Async<(ISymbol * Document) option> =
            this.FindSymbol' uri pos

        override this.FindSymbols (pattern: string option): Async<ISymbol seq> =
            let findTask =
                match pattern with
                | Some pat ->
                    fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsWithPatternAsync(sln, pat, SymbolFilter.TypeAndMember)
                | None ->
                    fun (sln: Solution) -> SymbolFinder.FindSourceDeclarationsAsync(sln, konst true, SymbolFilter.TypeAndMember)
            workspaces.Values
            |> Seq.map (fun workspace -> findTask workspace.CurrentSolution |> Async.AwaitTask)
            |> Async.Parallel
            |> map (Seq.collect id)

        override this.FindReferences (symbol: ISymbol): Async<ReferencedSymbol seq> = async {
            let! symbols =
                workspaces.Values
                |> Seq.map (fun workspace -> SymbolFinder.FindReferencesAsync(symbol, workspace.CurrentSolution) |> Async.AwaitTask)
                |> Async.Parallel
            return symbols |> Seq.collect id
        }

        override this.FindImplementations (symbol: ISymbol): Async<ISymbol seq> =
            workspaces.Values
            |> Seq.map (fun workspace -> SymbolFinder.FindImplementationsAsync(symbol, workspace.CurrentSolution) |> Async.AwaitTask)
            |> Async.Parallel
            |> map (Seq.collect id)

        override this.FindImplementations' (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> =
            workspaces.Values
            |> Seq.map (fun workspace -> SymbolFinder.FindImplementationsAsync(symbol, workspace.CurrentSolution, transitive) |> Async.AwaitTask)
            |> Async.Parallel
            |> map (Seq.collect id)

        override this.FindDerivedClasses (symbol: INamedTypeSymbol): Async<INamedTypeSymbol seq> = this.FindDerivedClasses' symbol true
        override this.FindDerivedClasses' (symbol: INamedTypeSymbol) (transitive: bool): Async<INamedTypeSymbol seq> = this.FindDerivedClasses' symbol transitive

        override this.FindDerivedInterfaces (symbol: INamedTypeSymbol):  Async<INamedTypeSymbol seq> = this.FindDerivedInterfaces' symbol true
        override this.FindDerivedInterfaces' (symbol: INamedTypeSymbol) (transitive: bool):  Async<INamedTypeSymbol seq> = this.FindDerivedInterfaces' symbol transitive

        override this.FindCallers (symbol: ISymbol): Async<SymbolCallerInfo seq> = async {
            let! symbols =
                workspaces.Values
                |> Seq.map (fun workspace -> SymbolFinder.FindCallersAsync(symbol, workspace.CurrentSolution) |> Async.AwaitTask)
                |> Async.Parallel
            return symbols |> Seq.collect id
        }

        override this.ResolveSymbolLocations (symbol: ISymbol) (project: Project option): Async<Location list> = async {
            let sourceLocations =
                symbol.Locations
                |> Seq.filter (fun l -> l.IsInSource)
                |> Seq.map Location.fromRoslynLocation
            let! metadateLocations =
                match project with
                | None -> async { return Seq.empty }
                | Some proj ->
                    symbol.Locations
                    |> Seq.filter (fun l -> l.IsInMetadata)
                    |> Seq.map (this.ResolveSymbolLocationsFromMetadata proj symbol)
                    |> Async.Parallel
                    |> map (Seq.collect id)

            return
                Seq.append sourceLocations metadateLocations
                |> Seq.toList
        }

        override this.GetDocumentVersion (uri: DocumentUri): int option =
            Uri.unescape uri |> files.TryFind |> Option.map (fun info -> info.Version)

        override this.OpenDocument (uri: DocumentUri) (version: int) (text: string) = async {
            do! this.SaveDocument uri (Some text)
            this.UpdateFile uri version
        }

        override this.CloseDocument (uri: DocumentUri) = async {
            Uri.unescape uri |> files.TryRemove |> ignore
        }

        override this.SaveDocument (uri: DocumentUri) (text: string option) = async {
            do! this.SaveDocument uri text
            this.UpdateFile uri 0
        }

        override this.ChangeDocument (uri: DocumentUri) (version: int) (changes: TextDocumentContentChangeEvent[]) : Async<unit> = async {
            match this.GetWorkspaceWithDocumentId uri with
            | None -> return ()
            | Some(workspace, docId) ->
                let solution = workspace.CurrentSolution
                let doc = solution.GetDocument(docId)
                let! initialSourceText = doc.GetTextAsync() |> Async.AwaitTask

                let applyChange (sourceText: SourceText) (change: Types.TextDocumentContentChangeEvent) =
                    match change.Range with
                    | Some changeRange ->
                        let changeTextSpan = Range.toTextSpan sourceText.Lines changeRange

                        TextChange(changeTextSpan, change.Text) |> sourceText.WithChanges

                    | None -> SourceText.From(change.Text)

                let newSourceText = Seq.fold applyChange initialSourceText changes

                logger.trace (
                    Log.setMessage "new source text: \n{newSourceText}"
                    >> Log.addContext "newSourceText" newSourceText
                )
                workspace.TryApplyChanges(solution.WithDocumentText(docId, newSourceText)) |> ignore

                this.UpdateFile uri version
        }

        override this.RemoveDocument (uri: DocumentUri) : Async<unit> = async {
            Uri.unescape uri |> files.TryRemove |> ignore
            match this.GetWorkspaceWithDocumentId uri with
            | None -> ()
            | Some (workspace, docId) ->
                let doc = workspace.CurrentSolution.GetDocument(docId)
                let project = doc.Project.RemoveDocument(docId)
                workspace.TryApplyChanges(project.Solution) |> ignore
        }

        override this.OnSolutionUpdate uri changeType = this.OnSolutionOrProjectUpdate uri changeType
        override this.OnProjectUpdate uri changeType = this.OnSolutionOrProjectUpdate uri changeType