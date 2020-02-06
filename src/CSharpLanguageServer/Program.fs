module CSharpLanguageServer.Program

open System
open System.IO
open System.Linq
open LSP
open LSP.Types
open LSP.Log

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text;
open Microsoft.CodeAnalysis.Completion
open FSharp.Control.Tasks.V2
open Microsoft.Build.Locator

type Server(client: ILanguageClient) =
    let mutable workspace: Workspace option = None

    let logMessage message = client.ShowMessage { ``type`` = MessageType.Log ;
                                                   message = "cs-lsp-server: " + message } |> ignore

    let mutable deferredInitialize = async {
        let solutionPath = "/Users/bob/src/omnisharp/test/test.sln"
        logMessage ("in deferredInitialize, loading solution: " + solutionPath)

        let msbuildWorkspace = MSBuildWorkspace.Create()
        msbuildWorkspace.LoadMetadataForReferencedProjects <- true
        let! testSolution = msbuildWorkspace.OpenSolutionAsync(solutionPath) |> Async.AwaitTask

        logMessage "in deferredInitialize, ok solution loaded"

        for diag in msbuildWorkspace.Diagnostics do
            logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

        workspace <- Some(msbuildWorkspace :> Workspace)
        ()
    }

    let currentSolution () = match workspace with
                             | Some ws -> Some ws.CurrentSolution
                             | _ -> None

    let getPathUri path = Uri("file://" + path)

    let getDocumentForUri (uri: Uri) =
        match currentSolution () with
        | Some solution -> let documents = solution.Projects |> Seq.collect (fun p -> p.Documents)
                           let matchingDocuments = documents |> Seq.filter (fun d -> uri = getPathUri d.FilePath) |> List.ofSeq
                           match matchingDocuments with
                           | [d] -> //logMessage ("resolved to document " + d.FilePath + " while looking for " + uri.ToString())
                                    Some d
                           | _ -> None
        | None -> None

    let getSymbolAtPosition documentUri pos = async {
        match getDocumentForUri documentUri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(pos.line, pos.character))
            let! symbolRef = SymbolFinder.FindSymbolAtPositionAsync(semanticModel, position, workspace.Value) |> Async.AwaitTask
            return symbolRef |> Option.ofObj
        | None ->
            return None
    }

    let todo() = raise (Exception "TODO")

    let makeRangeForLinePos (pos: FileLinePositionSpan) =
        { start = { line = pos.StartLinePosition.Line ;
                    character = pos.StartLinePosition.Character  }
          ``end`` = { line = pos.EndLinePosition.Line ;
                      character = pos.EndLinePosition.Character  } }

    let mapDiagSeverity s =
        match s with
        | DiagnosticSeverity.Info -> Some LSP.Types.DiagnosticSeverity.Information
        | DiagnosticSeverity.Warning -> Some LSP.Types.DiagnosticSeverity.Warning
        | DiagnosticSeverity.Error -> Some LSP.Types.DiagnosticSeverity.Error
        | DiagnosticSeverity.Hidden -> None
        | _ -> None

    let makeLspDiag (d: Diagnostic) =
        { range = makeRangeForLinePos(d.Location.GetLineSpan()) ;
            severity = mapDiagSeverity d.Severity ;
            code = None ;
            source = None ;
            message = d.GetMessage() }

    let locationToLspLocation (loc: Location) =
        { uri = loc.SourceTree.FilePath |> getPathUri ;
          range = loc.GetLineSpan() |> makeRangeForLinePos }

    interface ILanguageServer with
        member _.Initialize(_: InitializeParams) =
            async {
                return {
                   capabilities =
                       { defaultServerCapabilities with
                             hoverProvider = true
                             completionProvider = Some defaultCompletionOptions
                             signatureHelpProvider = None
                             documentSymbolProvider = false
                             codeLensProvider = None
                             workspaceSymbolProvider = false
                             definitionProvider = true
                             referencesProvider = true
                             renameProvider = false
                             textDocumentSync = {
                                defaultTextDocumentSyncOptions with
                                      openClose = false
                                      save = Some({ includeText = true })
                                      change = TextDocumentSyncKind.Full
                             }
                         }
                   }
                }

        member __.Initialized(): Async<unit> =
            deferredInitialize

        member __.Shutdown(): Async<unit> = todo()

        member __.DidChangeConfiguration(_: DidChangeConfigurationParams): Async<unit> = todo()

        member __.DidOpenTextDocument(openParams: DidOpenTextDocumentParams): Async<unit> = async {
            let document = getDocumentForUri openParams.textDocument.uri

            match document with
            | Some doc ->
                let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

                let diagnostics = semanticModel.GetDiagnostics()
                                    |> Seq.map makeLspDiag
                                    |> List.ofSeq

                client.PublishDiagnostics { uri = openParams.textDocument.uri ;
                                            diagnostics = diagnostics }
                return ()
            | None -> ()
        }

        member __.DidChangeTextDocument(change: DidChangeTextDocumentParams): Async<unit> = async {
            match getDocumentForUri change.textDocument.uri with
            | Some doc ->
                let fullText = SourceText.From(change.contentChanges.[0].text)

                let updatedDoc = doc.WithText(fullText)

                //let updatedSolution = updatedDoc.Project.Solution;
                //let applySucceeded = workspace.Value.TryApplyChanges(updatedSolution)

                //if not applySucceeded then
                //  logMessage "workspace.TryApplyChanges has failed!"

                let! semanticModel = updatedDoc.GetSemanticModelAsync() |> Async.AwaitTask

                let diagnostics = semanticModel.GetDiagnostics()
                                    |> Seq.map makeLspDiag
                                    |> List.ofSeq

                client.PublishDiagnostics { uri = change.textDocument.uri ;
                                            diagnostics = diagnostics }
                ()

            | _ -> ()

            return ()
        }

        member __.WillSaveTextDocument(_: WillSaveTextDocumentParams): Async<unit> = todo()

        member __.WillSaveWaitUntilTextDocument(_: WillSaveTextDocumentParams): Async<TextEdit list> = todo()

        member __.DidSaveTextDocument(saveParams: DidSaveTextDocumentParams): Async<unit> = async {
            match getDocumentForUri saveParams.textDocument.uri with
            | Some doc ->
                let newDoc = match saveParams.text with
                             | Some text -> let fullText = SourceText.From(text)
                                            let updatedDoc = doc.WithText(fullText)
                                            let updatedSolution = updatedDoc.Project.Solution;
                                            workspace.Value.TryApplyChanges(updatedSolution) |> ignore
                                            updatedDoc
                             | None -> doc

                let! semanticModel = newDoc.GetSemanticModelAsync() |> Async.AwaitTask

                let diagnostics = semanticModel.GetDiagnostics()
                                |> Seq.map makeLspDiag
                                |> List.ofSeq

                client.PublishDiagnostics { uri = saveParams.textDocument.uri ;
                                            diagnostics = diagnostics }
                ()

            | _ -> ()

            return ()
        }

        member __.DidCloseTextDocument(_: DidCloseTextDocumentParams): Async<unit> =
            async {
                return ()
            }

        member __.DidChangeWatchedFiles(_: DidChangeWatchedFilesParams): Async<unit> = todo()

        member __.Completion(posParams: TextDocumentPositionParams): Async<LSP.Types.CompletionList option> = async {

            logMessage ("Completion at posParams: " + posParams.ToString())

            match getDocumentForUri posParams.textDocument.uri with
            | Some doc ->
                let completionService = CompletionService.GetService(doc)
                if isNull completionService then
                    logMessage "doc has no completionService available"
                    return ()

                let! docText = doc.GetTextAsync() |> Async.AwaitTask
                let posInText = docText.Lines.GetPosition(LinePosition(posParams.position.line, posParams.position.character))

                let! maybeCompletionResults = completionService.GetCompletionsAsync(doc, posInText) |> Async.AwaitTask

                match Option.ofObj maybeCompletionResults with
                | Some completionResults ->
                    let makeLspCompletionItem (item: Microsoft.CodeAnalysis.Completion.CompletionItem) =
                        { defaultCompletionItem with
                            label = item.DisplayText ;
                            kind = Some LSP.Types.CompletionItemKind.Field ;
                        }

                    return Some { isIncomplete = true ;
                                items = completionResults.Items
                                        |> Seq.map makeLspCompletionItem
                                        |> List.ofSeq }
                | None -> return None
            | None -> return None
        }

        member __.Hover(hoverPos: TextDocumentPositionParams): Async<Hover option> = async {
            let! maybeSymbol = getSymbolAtPosition hoverPos.textDocument.uri hoverPos.position
            let maybeHoverText = Option.map (fun (sym: ISymbol) -> sym.ToString() + "\n" +  sym.GetDocumentationCommentXml()) maybeSymbol

            let contents = [ HighlightedString(match maybeHoverText with
                                               | None -> "", "fsharp"
                                               | Some text -> text, "fsharp") ;
                                PlainString("hey") ]

            return Some({ contents=contents; range=None })
        }


        member __.ResolveCompletionItem(_: LSP.Types.CompletionItem): Async<LSP.Types.CompletionItem> = todo()

        member __.SignatureHelp(_: TextDocumentPositionParams): Async<SignatureHelp option> = todo()

        member __.GotoDefinition(def: TextDocumentPositionParams): Async<LSP.Types.Location list> = async {
            let resolveDefinition (doc: Document) = task {
                let! sourceText = doc.GetTextAsync()
                let position = sourceText.Lines.GetPosition(LinePosition(def.position.line, def.position.character))

                let! semanticModel = doc.GetSemanticModelAsync()
                let! symbol = SymbolFinder.FindSymbolAtPositionAsync(semanticModel, position, workspace.Value)

                return match symbol with
                       | null -> //logMessage "no symbol at this point"
                         []
                       | sym -> //logMessage ("have symbol " + sym.ToString() + " at this point!")
                         // TODO:
                         //  - handle symbols in metadata

                         let locationsInSource = sym.Locations |> Seq.filter (fun l -> l.IsInSource) |> List.ofSeq

                         match locationsInSource with
                         | [] -> []
                         | locations -> locations |> Seq.map locationToLspLocation |> List.ofSeq
            }

            match getDocumentForUri def.textDocument.uri with
            | Some doc -> let! locations = resolveDefinition doc |> Async.AwaitTask
                          return locations
            | None -> return []
        }

        member __.FindReferences(refParams: ReferenceParams): Async<LSP.Types.Location list> = async {
            match currentSolution () with
            | Some solution ->
                let! maybeSymbol = getSymbolAtPosition refParams.textDocument.uri refParams.position

                match maybeSymbol with
                | Some symbol ->
                    let! refs = SymbolFinder.FindReferencesAsync(symbol, solution) |> Async.AwaitTask
                    return refs |> Seq.map (fun r -> r.Locations)
                                |> Seq.concat
                                |> Seq.map (fun rl -> rl.Location)
                                |> Seq.map locationToLspLocation
                                |> List.ofSeq
                | None -> return []

            | None -> return []
        }

        member __.DocumentHighlight(_: TextDocumentPositionParams): Async<DocumentHighlight list> = todo()
        member __.DocumentSymbols(_: DocumentSymbolParams): Async<SymbolInformation list> = todo()
        member __.WorkspaceSymbols(_: WorkspaceSymbolParams): Async<SymbolInformation list> = todo()
        member __.CodeActions(_: CodeActionParams): Async<Command list> = todo()
        member __.CodeLens(_: CodeLensParams): Async<List<CodeLens>> = todo()
        member __.ResolveCodeLens(_: CodeLens): Async<CodeLens> = todo()
        member __.DocumentLink(_: DocumentLinkParams): Async<DocumentLink list> = todo()
        member __.ResolveDocumentLink(_: DocumentLink): Async<DocumentLink> = todo()
        member __.DocumentFormatting(_: DocumentFormattingParams): Async<TextEdit list> = todo()
        member __.DocumentRangeFormatting(_: DocumentRangeFormattingParams): Async<TextEdit list> = todo()
        member __.DocumentOnTypeFormatting(_: DocumentOnTypeFormattingParams): Async<TextEdit list> = todo()
        member __.Rename(_: RenameParams): Async<WorkspaceEdit> = todo()
        member __.ExecuteCommand(_: ExecuteCommandParams): Async<unit> = todo()
        member __.DidChangeWorkspaceFolders(_: DidChangeWorkspaceFoldersParams): Async<unit> = todo()

[<EntryPoint>]
let main(_: string array): int =
    MSBuildLocator.RegisterDefaults() |> ignore

    let read = new BinaryReader(Console.OpenStandardInput())
    let write = new BinaryWriter(Console.OpenStandardOutput())
    let serverFactory(client) = Server(client) :> ILanguageServer
    dprintfn "Listening on stdin"
    try
        LanguageServer.connect(serverFactory, read, write)
        0 // return an integer exit code
    with e ->
        dprintfn "Exception in language server %O" e
        1
