namespace CSharpLanguageServer.Workspace

open System.Collections.Concurrent
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

type WorkspaceManager(lspClient: ICSharpLspClient) =

    let workspaces: ConcurrentDictionary<DocumentUri, Workspace> =
        ConcurrentDictionary()
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
            // TODO: Report progress to client?
            do! workspace.OpenSolutionAsync(sln) |> Async.AwaitTask |> Async.Ignore
            logger.info (Log.setMessage "Finish to load {slnPath}" >> Log.addContext "slnPath" sln)
        | Some(Projects projs) ->
            logger.info (
                Log.setMessage "Start to load {projNum} projects..."
                >> Log.addContext "projNum" projs.Length
            )

            do!
                projs
                |> map (fun proj -> async {
                    logger.info (Log.setMessage "Start to load {proj}..." >> Log.addContext "proj" proj)

                    try
                        do! workspace.OpenProjectAsync(proj) |> Async.AwaitTask |> Async.Ignore
                    with ex ->
                        logger.info (
                            Log.setMessage "Exception during loading {proj}:"
                            >> Log.addContext "proj" proj
                            >> Log.addException ex
                        )

                    logger.info (Log.setMessage "Finish to load {proj}" >> Log.addContext "proj" proj)
                })
                |> Async.Parallel
                |> Async.Ignore

            logger.info (
                Log.setMessage "Finish to load {projNum} projects"
                >> Log.addContext "projNum" projs.Length
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
                let workspace = MSBuildWorkspace.Create()
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

    interface IWorkspaceManager with
        override this.ChangeWorkspaceFolders added removed =
            this.ChangeWorkspaceFolders added removed

        override this.Initialize(workspaceFolders: WorkspaceFolder list) =
            let doInitialize (workspaceFolders: WorkspaceFolder list) = async {
                MSBuildLocator.RegisterDefaults() |> ignore
                do! this.ChangeWorkspaceFolders (List.toArray workspaceFolders) Array.empty
                initialized.SetResult(true)
            }
            doInitialize workspaceFolders |> Async.Start

        override this.WaitInitialized() = async {
            do! initialized.Task |> Async.AwaitTask |> Async.Ignore
        }

        override this.GetDocument(uri: DocumentUri) : Document option = this.GetDocument uri

        override this.FindSymbol (uri: DocumentUri) (pos: Position): Async<ISymbol option> = async {
            match this.GetDocument uri with
            | None -> return None
            | Some doc ->
                let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
                let position = Position.toRoslynPosition sourceText.Lines pos
                let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
                return symbol |> Option.ofObj
        }

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

        override this.ResolveSymbolLocations (symbol: ISymbol): Async<Location list> = async {
            // TODO: Support symbols in decompiled document
            return
                symbol.Locations
                |> Seq.map Location.fromRoslynLocation
                |> Seq.toList
        }

        override this.ChangeDocument (uri: DocumentUri) (changes: TextDocumentContentChangeEvent[]) : Async<unit> = async {
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
        }
