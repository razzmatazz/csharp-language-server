module CSharpLanguageServer.Server

open System
open System.IO
open System.Collections.Generic
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Rename
open FSharp.Control.Tasks.V2

open LanguageServerProtocol.Server
open LanguageServerProtocol
open LanguageServerProtocol.Types
open LanguageServerProtocol.LspResult

open RoslynHelpers
open Microsoft.CodeAnalysis.CodeRefactorings
open System.Threading
open Microsoft.CodeAnalysis.CodeFixes
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp.Transforms
open Newtonsoft.Json
open Newtonsoft.Json.Converters

type Options = {
    SolutionPath: string option;
    LogLevel: Types.MessageType;
}

type CSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerNotification "window/showMessage" (box p) |> Async.Ignore

    override __.WindowShowMessageRequest(p) =
        sendServerRequest.Send "window/showMessageRequest" (box p)

    override __.WindowLogMessage(p) =
        sendServerNotification "window/logMessage" (box p) |> Async.Ignore

    override __.TelemetryEvent(p) =
        sendServerNotification "telemetry/event" (box p) |> Async.Ignore

    override __.ClientRegisterCapability(p) =
        sendServerRequest.Send "client/registerCapability" (box p)

    override __.ClientUnregisterCapability(p) =
        sendServerRequest.Send "client/unregisterCapability" (box p)

    override __.WorkspaceWorkspaceFolders () =
        sendServerRequest.Send "workspace/workspaceFolders" ()

    override __.WorkspaceConfiguration (p) =
        sendServerRequest.Send "workspace/configuration" (box p)

    override __.WorkspaceApplyEdit (p) =
        sendServerRequest.Send "workspace/applyEdit" (box p)

    override __.WorkspaceSemanticTokensRefresh () =
      sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore


type CSharpMetadataParams = {
    TextDocument: TextDocumentIdentifier
}


type CSharpMetadataResponse = {
    ProjectName: string;
    AssemblyName: string;
    SymbolName: string;
    Source: string;
}


type CSharpLspServer(lspClient: CSharpLspClient, options: Options) =
    inherit LspServer()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable currentSolution: Solution option = None
    let mutable openDocVersions = Map.empty<string, int>
    let mutable decompiledMetadataUris = Map.empty<string, CSharpMetadataResponse>
    let mutable decompiledMetadataDocs = Map.empty<string, Document>

    let logMessageWithLevel l message =
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do lspClient.WindowShowMessage messageParams |> ignore

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info
    let warningMessage = logMessageWithLevel MessageType.Warning
    let errorMessage = logMessageWithLevel MessageType.Error

    let getDocumentForUri u =
        let getPathUri path = Uri("file://" + path)

        let uri = Uri u
        match currentSolution with
        | Some solution -> let documents = solution.Projects |> Seq.collect (fun p -> p.Documents)
                           let matchingDocuments = documents |> Seq.filter (fun d -> uri = getPathUri d.FilePath) |> List.ofSeq
                           match matchingDocuments with
                           | [d] -> //logMessage ("resolved to document " + d.FilePath + " while looking for " + uri.ToString())
                                    Some d
                           | _ -> Map.tryFind u decompiledMetadataDocs
        | None -> None

    let withDocumentOnUriOrNone fn u = async {
        match getDocumentForUri u with
        | Some doc ->
            let! fnResult = fn doc
            return fnResult
        | None ->
            return None
    }

    let isUriADecompiledDoc u = Map.containsKey u decompiledMetadataDocs

    let withRegularDocumentOnUriOrUnit fn u = async {
        match isUriADecompiledDoc u with
        | true ->  ()
        | false ->
            match getDocumentForUri u with
            | Some doc ->
                let! fnResult = fn doc
                return fnResult
            | None ->
                return ()
    }

    let getSymbolAtPosition uri pos =
        let resolveSymbolOrNull (doc: Document) = async {
           let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
           let position = sourceText.Lines.GetPosition(LinePosition(pos.Line, pos.Character))
           let! symbolRef = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
           return if isNull symbolRef then None else Some (symbolRef, doc)
        }

        withDocumentOnUriOrNone resolveSymbolOrNull uri

    let getContainingTypeOrThis (symbol: ISymbol): INamedTypeSymbol =
        if (symbol :? INamedTypeSymbol) then
            symbol :?> INamedTypeSymbol
        else
            symbol.ContainingType

    let getFullReflectionName (containingType: INamedTypeSymbol) =
        let stack = Stack<string>();
        stack.Push(containingType.MetadataName);
        let mutable ns = containingType.ContainingNamespace;

        let mutable doContinue = true
        while doContinue do
            stack.Push(ns.Name);
            ns <- ns.ContainingNamespace

            doContinue <- ns <> null && not ns.IsGlobalNamespace

        String.Join(".", stack)

    let publishDiagnosticsOnDocument docUri (doc: Document) = async {
        let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

        let diagnostics = semanticModel.GetDiagnostics()
                            |> Seq.map RoslynHelpers.roslynToLspDiagnostic
                            |> Array.ofSeq

        do! lspClient.TextDocumentPublishDiagnostics {
            Uri = docUri
            Diagnostics = diagnostics }

        return ()
    }

    override _.Initialize(p: InitializeParams) = async {
        clientCapabilities <- p.Capabilities

        infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                             (typeof<CSharpLspServer>.Assembly.GetName().Version |> string)
                             (JsonConvert.SerializeObject(options, StringEnumConverter())))
        infoMessage "project url is https://github.com/razzmatazz/csharp-language-server"
        infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp."

        match options.SolutionPath with
        | Some solutionPath ->
            infoMessage (sprintf "Initialize: loading specified solution file: %s.." solutionPath)
            let! solutionLoaded = tryLoadSolutionOnPath logMessage solutionPath
            currentSolution <- solutionLoaded

        | None ->
            let cwd = Directory.GetCurrentDirectory()
            infoMessage (sprintf "attempting to find and load solution based on cwd: \"%s\".." cwd)

            let! solutionLoaded = findAndLoadSolutionOnDir logMessage cwd
            currentSolution <- solutionLoaded

        infoMessage "ok, solution loaded -- initialization finished"

        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = Some true
                        DefinitionProvider = Some true
                        TypeDefinitionProvider = Some false
                        ImplementationProvider = Some false
                        ReferencesProvider = Some true
                        DocumentHighlightProvider = Some true
                        DocumentSymbolProvider = Some true
                        WorkspaceSymbolProvider = Some true
                        DocumentFormattingProvider = Some false
                        DocumentRangeFormattingProvider = Some false
                        SignatureHelpProvider = None
                        CompletionProvider =
                            Some { ResolveProvider = None
                                   TriggerCharacters = Some ([| '.'; '''; |])
                                   AllCommitCharacters = None
                                 }
                        CodeLensProvider = None
                        CodeActionProvider = Some true
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = Some false
                                     Save = Some { IncludeText = Some true }
                                     Change = Some TextDocumentSyncKind.Full
                                 }
                        FoldingRangeProvider = Some false
                        SelectionRangeProvider = Some false
                        SemanticTokensProvider = None
                    }
            }
        |> success
    }

    override __.Initialized(_: InitializedParams) = async { return () }

    override __.Exit(): Async<unit> = async { return () }

    override __.Shutdown(): Async<unit> = async { return () }

    override __.TextDocumentDidOpen(openParams: Types.DidOpenTextDocumentParams) =
        openDocVersions <- openDocVersions.Add(openParams.TextDocument.Uri, openParams.TextDocument.Version)

        withRegularDocumentOnUriOrUnit
            (publishDiagnosticsOnDocument openParams.TextDocument.Uri)
            openParams.TextDocument.Uri

    override __.TextDocumentDidChange(changeParams: Types.DidChangeTextDocumentParams): Async<unit> =
        let publishDiagnosticsOnUpdatedDocument (doc: Document) = async {
            let fullText = SourceText.From(changeParams.ContentChanges.[0].Text)

            let updatedDoc = doc.WithText(fullText)
            let updatedSolution = updatedDoc.Project.Solution;
            currentSolution <- Some updatedSolution

            do! publishDiagnosticsOnDocument changeParams.TextDocument.Uri updatedDoc
            return ()
        }

        openDocVersions <- openDocVersions.Add(
            changeParams.TextDocument.Uri,
            changeParams.TextDocument.Version |> Option.defaultValue 0)

        withRegularDocumentOnUriOrUnit
            publishDiagnosticsOnUpdatedDocument
            changeParams.TextDocument.Uri

    override __.TextDocumentDidSave(_: Types.DidSaveTextDocumentParams): Async<unit> = async {
        return ()
    }

    override __.TextDocumentDidClose(closeParams: Types.DidCloseTextDocumentParams): Async<unit> = async {
        openDocVersions <- openDocVersions.Remove(closeParams.TextDocument.Uri)
        return ()
    }

    override __.TextDocumentCodeAction(actionParams: Types.CodeActionParams): AsyncLspResult<Types.TextDocumentCodeActionResult option> = async {

        match getDocumentForUri actionParams.TextDocument.Uri with
        | None ->
            return None |> success

        | Some doc ->
            let! docText = doc.GetTextAsync() |> Async.AwaitTask

            let textSpan = actionParams.Range |> roslynLinePositionSpanForLspRange
                                              |> docText.Lines.GetTextSpan

            // register code actions
            let roslynCodeActions = List<CodeActions.CodeAction>()
            let addCodeAction = Action<CodeActions.CodeAction>(roslynCodeActions.Add)
            let codeActionContext = CodeRefactoringContext(doc, textSpan, addCodeAction, CancellationToken.None)

            for refactoringProvider in refactoringProviderInstances do
                do! refactoringProvider.ComputeRefactoringsAsync(codeActionContext) |> Async.AwaitTask

            // register code fixes
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

            let isDiagnosticsOnTextSpan (diag: Microsoft.CodeAnalysis.Diagnostic) =
                diag.Location.SourceSpan.IntersectsWith(textSpan)

            let relatedDiagnostics = semanticModel.GetDiagnostics() |> Seq.filter isDiagnosticsOnTextSpan |> List.ofSeq
            //logMessage (sprintf "relatedDiagnostics.Count=%d" (relatedDiagnostics.Count()))

            if relatedDiagnostics.Length > 0 then
                let addCodeFix =
                    Action<CodeActions.CodeAction, ImmutableArray<Microsoft.CodeAnalysis.Diagnostic>>(
                        fun ca _ -> roslynCodeActions.Add(ca))

                for refactoringProvider in codeFixProviderInstances do
                    //logMessage (sprintf "%s: .FixableDiagnosticIds=%s" (refactoringProvider |> string)
                    //                                                   (JsonConvert.SerializeObject(refactoringProvider.FixableDiagnosticIds)))

                    for diag in relatedDiagnostics do
                        let codeFixContext = CodeFixContext(doc, diag, addCodeFix, CancellationToken.None)

                        //logMessage (sprintf "%s: Id=%s" (diag |> string) diag.Id)

                        let refactoringProviderOK =
                            let translatedDiagId diagId =
                                match diagId with
                                | "CS8019" -> "RemoveUnnecessaryImportsFixable"
                                | _ -> ""

                            refactoringProvider.FixableDiagnosticIds.Contains(diag.Id)
                            || refactoringProvider.FixableDiagnosticIds.Contains(translatedDiagId diag.Id)

                        try
                            if refactoringProviderOK then
                                do! refactoringProvider.RegisterCodeFixesAsync(codeFixContext) |> Async.AwaitTask
                        with _ex ->
                            //sprintf "error in RegisterCodeFixesAsync(): %s" (ex.ToString()) |> logMessage
                            ()

            let! lspCodeActions = roslynCodeActions
                                  |> Seq.map (roslynCodeActionToLspCodeAction currentSolution.Value
                                                                              openDocVersions.TryFind
                                                                              logMessage)
                                  |> Async.Sequential

            return lspCodeActions |> Seq.collect (fun maybeAction -> match maybeAction with
                                                                     | Some action -> [action]
                                                                     | None -> [])
                                  |> Array.ofSeq
                                  |> TextDocumentCodeActionResult.CodeActions
                                  |> Some
                                  |> success
    }

    override __.TextDocumentCompletion(posParams: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = async {
        match getDocumentForUri posParams.TextDocument.Uri with
        | Some doc ->
            let! docText = doc.GetTextAsync() |> Async.AwaitTask
            let posInText = docText.Lines.GetPosition(LinePosition(posParams.Position.Line, posParams.Position.Character))

            let completionService = CompletionService.GetService(doc)
            if isNull completionService then
                return ()

            let! maybeCompletionResults = completionService.GetCompletionsAsync(doc, posInText)
                                          |> Async.AwaitTask

            match Option.ofObj maybeCompletionResults with
            | Some completionResults ->
                let makeLspCompletionItem (item: Microsoft.CodeAnalysis.Completion.CompletionItem) =
                    { CompletionItem.Create(item.DisplayText) with
                        Kind = item.Tags |> Seq.head |> roslynTagToLspCompletion |> Some ;
                        InsertTextFormat = Some Types.InsertTextFormat.PlainText }

                let completionList = {
                    IsIncomplete = false ;
                    Items = completionResults.Items
                                              |> Seq.map makeLspCompletionItem
                                              |> Array.ofSeq }

                return success (Some completionList)
            | None -> return success None
        | None -> return success None
    }

    override __.TextDocumentDefinition(def: Types.TextDocumentPositionParams): AsyncLspResult<Types.GotoResult option> = async {
        let resolveDefinition (doc: Document) = task {
            let! sourceText = doc.GetTextAsync()
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position)
            let! compilation = doc.Project.GetCompilationAsync()

            let! locations = task {
                match symbol with
                | null -> //logMessage "no symbol at this point"
                    return []

                | sym ->
                    //logMessage ("have symbol " + sym.ToString() + " at this point!")
                    // TODO:
                    //  - handle symbols in metadata

                    let locationsInSource = sym.Locations |> Seq.filter (fun l -> l.IsInSource)

                    let locationsInMetadata = sym.Locations |> Seq.filter (fun l -> l.IsInMetadata)

                    let haveLocationsInMetadata = locationsInMetadata |> Seq.isEmpty |> not

                    if haveLocationsInMetadata then
                        let mdLocation = Seq.head locationsInMetadata

                        let reference = compilation.GetMetadataReference(mdLocation.MetadataModule.ContainingAssembly)
                        let peReference = reference :?> PortableExecutableReference |> Option.ofObj
                        let assemblyLocation = peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

                        let decompiler = CSharpDecompiler(assemblyLocation, DecompilerSettings())

                        // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
                        // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
                        decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

                        let fullName = sym |> getContainingTypeOrThis |> getFullReflectionName
                        let fullTypeName = ICSharpCode.Decompiler.TypeSystem.FullTypeName(fullName)

                        let text = decompiler.DecompileTypeAsString(fullTypeName)

                        let uri = $"csharp:/metadata/projects/{doc.Project.Name}/assemblies/{mdLocation.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"

                        let csharpMetadata = { ProjectName = doc.Project.Name;
                                               AssemblyName = mdLocation.MetadataModule.ContainingAssembly.Name;
                                               SymbolName = fullName;
                                               Source = text }

                        decompiledMetadataUris <- decompiledMetadataUris |> Map.add uri csharpMetadata

                        let mdDocumentFilename = $"$metadata$/projects/{doc.Project.Name}/assemblies/{mdLocation.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
                        let mdProject = doc.Project
                        let mdDocumentEmpty = mdProject.AddDocument(mdDocumentFilename, String.Empty)
                        let mdDocument = SourceText.From(text) |> mdDocumentEmpty.WithText

                        decompiledMetadataDocs <- decompiledMetadataDocs |> Map.add uri mdDocument

                        // figure out location on the document (approx implementation)
                        let! syntaxTree = mdDocument.GetSyntaxTreeAsync()
                        let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym.Name)
                        collector.Visit(syntaxTree.GetRoot())

                        let fallbackLocationInMetadata = {
                            Uri = uri
                            Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 1 } } }

                        let locationInMetadata =
                            (collector.GetLocations() @ [ fallbackLocationInMetadata ])
                            |> Seq.head

                        return [locationInMetadata]
                    else
                        match List.ofSeq locationsInSource with
                        | [] -> return []
                        | locations ->
                            return locations
                                   |> Seq.map lspLocationForRoslynLocation
                                   |> List.ofSeq
                }

            return locations
        }

        match getDocumentForUri def.TextDocument.Uri with
        | Some doc -> let! locations = resolveDefinition doc |> Async.AwaitTask
                      return locations |> Array.ofSeq
                                       |> GotoResult.Multiple
                                       |> Some
                                       |> success
        | None ->
            logMessage (sprintf "getDocumentForUri: no document for uri %s" (def.TextDocument.Uri |> string))
            return None |> success
    }

    override __.TextDocumentDocumentHighlight(docParams: Types.TextDocumentPositionParams): AsyncLspResult<Types.DocumentHighlight [] option> = async {
        match currentSolution with
        | Some solution ->
            let! maybeSymbol = getSymbolAtPosition docParams.TextDocument.Uri docParams.Position

            match maybeSymbol with
            | Some (symbol, doc) ->
                let docSet = ImmutableHashSet<Document>.Empty.Add(doc)
                let! refs = SymbolFinder.FindReferencesAsync(symbol, solution, docSet) |> Async.AwaitTask
                let locationsFromRefs = refs |> Seq.collect (fun r -> r.Locations) |> Seq.map (fun rl -> rl.Location)

                let! defRef = SymbolFinder.FindSourceDefinitionAsync(symbol, solution) |> Async.AwaitTask
                let locationsFromDef = match Option.ofObj defRef with
                                       // TODO: we might need to skip locations that are on a different document than this one
                                       | Some sym -> sym.Locations |> List.ofSeq
                                       | None -> []

                return (Seq.append locationsFromRefs locationsFromDef)
                       |> Seq.map (fun l -> { Range = (lspLocationForRoslynLocation l).Range ;
                                              Kind = Some DocumentHighlightKind.Read })
                       |> Array.ofSeq
                       |> Some
                       |> success

            | None -> return None |> success

        | None -> return None |> success
    }

    override __.TextDocumentDocumentSymbol(p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        match getDocumentForUri p.TextDocument.Uri with
        | Some doc ->
            let collector = DocumentSymbolCollector(p.TextDocument.Uri)

            let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetSymbols() |> Some |> success

        | None ->
            return None |> success
    }

    override __.TextDocumentHover(hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = async {
        let! maybeSymbol = getSymbolAtPosition hoverPos.TextDocument.Uri hoverPos.Position

        let getSymbolDocumentation (sym: ISymbol) =
            let csharpMarkdownDoc =
                Documentation.formatDocXml
                    (sym.GetDocumentationCommentXml())
                    (sym.ToString())
                    sym.ContainingAssembly.Name

            //logMessage (sprintf "debug: xml=%s; markdown=%s" (sym.GetDocumentationCommentXml()) csharpMarkdownDoc)

            [MarkedString.WithLanguage { Language = "markdown"; Value = csharpMarkdownDoc }]

        let suggestUnderlyingSymbolType (sym: ISymbol) =
            match sym with
            | :? ILocalSymbol as ps -> ps.Type :> ISymbol
            | _ -> sym

        (* TODO debugging code -- remove
        match maybeSymbol with
        | Some (sym, _) ->
           logMessage (sprintf "have symbol on hover: %s; xml=%s"
                               (sym |> string)
                               (sym.GetDocumentationCommentXml()))
        | _ -> ()
        *)

        let contents =
            match maybeSymbol with
            | Some (sym, _) -> sym |> suggestUnderlyingSymbolType |> getSymbolDocumentation
            | _ -> []

        return Some { Contents = contents |> Array.ofSeq |> MarkedStrings;
                      Range = None } |> success
    }

    override __.TextDocumentReferences(refParams: Types.ReferenceParams): AsyncLspResult<Types.Location [] option> = async {
        match currentSolution with
        | Some solution ->
            let! maybeSymbol = getSymbolAtPosition refParams.TextDocument.Uri refParams.Position

            match maybeSymbol with
            | Some (symbol, _) ->
                let! refs = SymbolFinder.FindReferencesAsync(symbol, solution) |> Async.AwaitTask
                return refs |> Seq.collect (fun r -> r.Locations)
                            |> Seq.map (fun rl -> lspLocationForRoslynLocation rl.Location)
                            |> Array.ofSeq
                            |> Some
                            |> success
            | None -> return None |> success

        | None -> return None |> success
    }

    override __.TextDocumentRename(rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = async {
        let renameSymbolInDoc symbol (doc: Document) = async {
            let originalSolution = doc.Project.Solution
            let! updatedSolution = Renamer.RenameSymbolAsync(doc.Project.Solution,
                                                             symbol,
                                                             rename.NewName,
                                                             doc.Project.Solution.Workspace.Options)
                                   |> Async.AwaitTask

            let! docTextEdit = lspDocChangesFromSolutionDiff originalSolution
                                                             updatedSolution
                                                             openDocVersions.TryFind
            return docTextEdit
        }

        let! maybeSymbol = getSymbolAtPosition rename.TextDocument.Uri rename.Position

        let! docChanges = match maybeSymbol with
                            | Some (symbol, doc) -> renameSymbolInDoc symbol doc
                            | None -> async { return [] }

        return WorkspaceEdit.Create (docChanges |> Array.ofList, clientCapabilities.Value) |> Some |> success
    }

    override __.WorkspaceSymbol(symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let! symbols = findSymbols currentSolution.Value symbolParams.Query (Some 20)
        return symbols |> Array.ofSeq |> Some |> success
    }

    member __.CSharpMetadata (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = async {
        let uri = metadataParams.TextDocument.Uri
        return decompiledMetadataUris |> Map.tryFind uri |> success
    }

    override __.Dispose(): unit = ()


let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestHandlings =
        defaultRequestHandlings<CSharpLspServer> ()
        |> Map.add "csharp/metadata" (requestHandling (fun s p -> s.CSharpMetadata (p)))

    LanguageServerProtocol.Server.start requestHandlings
                                        input
                                        output
                                        CSharpLspClient
                                        (fun lspClient -> new CSharpLspServer(lspClient, options))

let start options =
    try
        let result = startCore options
        int result
    with
    | _ex ->
        // logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
