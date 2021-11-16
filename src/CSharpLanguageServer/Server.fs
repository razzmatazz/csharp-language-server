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

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.LspResult

open RoslynHelpers
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

type CSharpCodeActionResolutionData = {
    TextDocumentUri: string
    Range: Range
}

type DecompiledMetadata = { Metadata: CSharpMetadataResponse; Document: Document }

type ServerState = {
    ClientCapabilities: ClientCapabilities option
    Solution: Solution option
    OpenDocVersions: Map<string, int>
    DecompiledMetadata: Map<string, DecompiledMetadata>
}

let emptyServerState = { ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty }

let getDocumentForUri state u =
    let getPathUri path = Uri("file://" + path)

    let uri = Uri u
    match state.Solution with
    | Some solution ->
        let matchingDocuments = solution.Projects
                                |> Seq.collect (fun p -> p.Documents)
                                |> Seq.filter (fun d -> uri = getPathUri d.FilePath) |> List.ofSeq

        match matchingDocuments with
        | [d] -> //logMessage ("resolved to document " + d.FilePath + " while looking for " + uri.ToString())
                Some d
        | _ -> //logMessage (sprintf "getDocumentForUri: no document on solution matching uri %s" (uri |> string))
                Map.tryFind u state.DecompiledMetadata
                |> Option.map (fun x -> x.Document)

    | None -> None

let withDocumentOnUriOrNone state fn u = async {
    match getDocumentForUri state u with
    | Some doc ->
        let! fnResult = fn doc
        return fnResult
    | None ->
        return None
}

let getSymbolAtPosition state uri pos =
    let resolveSymbolOrNull (doc: Document) = async {
        let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
        let position = sourceText.Lines.GetPosition(LinePosition(pos.Line, pos.Character))
        let! symbolRef = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
        return if isNull symbolRef then None else Some (symbolRef, doc)
    }

    withDocumentOnUriOrNone state resolveSymbolOrNull uri

let withRegularDocumentOnUriOrUnit state fn u = async {
    let isUriADecompiledDoc state u = Map.containsKey u state.DecompiledMetadata

    match isUriADecompiledDoc state u with
    | true ->  ()
    | false ->
        match getDocumentForUri state u with
        | Some doc ->
            let! fnResult = fn doc
            return fnResult
        | None ->
            return ()
}

type CSharpLspServer(lspClient: CSharpLspClient, options: Options) =
    inherit LspServer()

    let mutable state: ServerState = emptyServerState

    let logMessageWithLevel l message =
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do lspClient.WindowShowMessage messageParams |> ignore

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info
    let warningMessage = logMessageWithLevel MessageType.Warning
    let errorMessage = logMessageWithLevel MessageType.Error

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

    member _.State
        with get() = state
        and set(newState) = state <- newState

    member __.LogMessage(s: string) = s |> logMessage

    override __.Dispose(): unit = ()

    override _.Initialize(p: InitializeParams) = async {
        state <- { state with ClientCapabilities = p.Capabilities }

        infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                             (typeof<CSharpLspServer>.Assembly.GetName().Version |> string)
                             (JsonConvert.SerializeObject(options, StringEnumConverter())))
        infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

        match options.SolutionPath with
        | Some solutionPath ->
            infoMessage (sprintf "Initialize: loading specified solution file: %s.." solutionPath)
            let! solutionLoaded = tryLoadSolutionOnPath logMessage solutionPath
            state <- { state with Solution = solutionLoaded }

        | None ->
            let cwd = Directory.GetCurrentDirectory()
            infoMessage (sprintf "attempting to find and load solution based on cwd: \"%s\".." cwd)

            let! solutionLoaded = findAndLoadSolutionOnDir logMessage cwd
            state <- { state with Solution = solutionLoaded }

        infoMessage "ok, solution loaded -- initialization finished"

        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = Some true
                        DefinitionProvider = Some true
                        TypeDefinitionProvider = None
                        ImplementationProvider = None
                        ReferencesProvider = Some true
                        DocumentHighlightProvider = Some true
                        DocumentSymbolProvider = Some true
                        WorkspaceSymbolProvider = Some true
                        DocumentFormattingProvider = None
                        DocumentRangeFormattingProvider = None
                        SignatureHelpProvider = None
                        CompletionProvider =
                            Some { ResolveProvider = None
                                   TriggerCharacters = Some ([| '.'; '''; |])
                                   AllCommitCharacters = None
                                 }
                        CodeLensProvider = None
                        CodeActionProvider =
                            Some { CodeActionKinds = None
                                   ResolveProvider = Some true
                                 }
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = None
                                     Save = Some { IncludeText = Some true }
                                     Change = Some TextDocumentSyncKind.Full
                                 }
                        FoldingRangeProvider = None
                        SelectionRangeProvider = None
                        SemanticTokensProvider = None
                    }
            }
        |> success
    }

    override __.TextDocumentDidOpen(openParams: Types.DidOpenTextDocumentParams) =
        match getDocumentForUri state openParams.TextDocument.Uri with
        | Some doc -> async {
            state <- { state with OpenDocVersions = state.OpenDocVersions.Add(openParams.TextDocument.Uri, openParams.TextDocument.Version) }

            do! publishDiagnosticsOnDocument openParams.TextDocument.Uri doc

            return ()
          }

        | None -> async {
            let docFilePath = openParams.TextDocument.Uri.Substring("file://".Length)

            let newDocMaybe = tryAddDocument logMessage
                                             docFilePath
                                             openParams.TextDocument.Text
                                             state.Solution.Value
            match newDocMaybe with
            | Some newDoc ->
                state <- { state with Solution = Some newDoc.Project.Solution }

                do! publishDiagnosticsOnDocument openParams.TextDocument.Uri newDoc
                return ()

            | None ->
                return ()
        }

    override __.TextDocumentDidChange(changeParams: Types.DidChangeTextDocumentParams): Async<unit> =
        let publishDiagnosticsOnUpdatedDocument (doc: Document) = async {
            let fullText = SourceText.From(changeParams.ContentChanges.[0].Text)

            let updatedDoc = doc.WithText(fullText)
            let updatedSolution = updatedDoc.Project.Solution;
            state <- { state with Solution = Some updatedSolution }

            do! publishDiagnosticsOnDocument changeParams.TextDocument.Uri updatedDoc
            return ()
        }

        state <- { state with OpenDocVersions = state.OpenDocVersions.Add(changeParams.TextDocument.Uri,
                                                                          changeParams.TextDocument.Version |> Option.defaultValue 0) }
        withRegularDocumentOnUriOrUnit
            state
            publishDiagnosticsOnUpdatedDocument
            changeParams.TextDocument.Uri

    override __.TextDocumentDidSave(saveParams: Types.DidSaveTextDocumentParams): Async<unit> =
        // we need to add this file to solution if not already
        match getDocumentForUri state saveParams.TextDocument.Uri with
        | None ->
            let docFilePath = saveParams.TextDocument.Uri.Substring("file://".Length)
            let newDocMaybe = tryAddDocument logMessage
                                             docFilePath
                                             saveParams.Text.Value
                                             state.Solution.Value
            match newDocMaybe with
            | Some newDoc -> async {
                state <- { state with Solution = Some newDoc.Project.Solution }

                do! publishDiagnosticsOnDocument saveParams.TextDocument.Uri newDoc

                return ()
              }

            | None ->
                async { return () }

        | _ -> async { return () }

let handleTextDocumentDidClose state logMessage (closeParams: Types.DidCloseTextDocumentParams): Async<(unit * ServerState)> = async {
    let newOpenDocVersions = state.OpenDocVersions.Remove(closeParams.TextDocument.Uri)
    return (), { state with OpenDocVersions = newOpenDocVersions }
}

let handleTextDocumentCodeAction state logMessage (actionParams: Types.CodeActionParams): AsyncLspResult<Types.TextDocumentCodeActionResult option> = async {
    match getDocumentForUri state actionParams.TextDocument.Uri with
    | None ->
        return None |> success

    | Some doc ->
        let! docText = doc.GetTextAsync() |> Async.AwaitTask

        let textSpan = actionParams.Range |> roslynLinePositionSpanForLspRange
                                          |> docText.Lines.GetTextSpan

        let! roslynCodeActions = getRoslynCodeActions doc textSpan

        let clientCapabilities = state.ClientCapabilities

        let clientSupportsCodeActionEditResolveWithEditAndData =
            clientCapabilities.IsSome
                && (clientCapabilities.Value.TextDocument.IsSome)
                && (clientCapabilities.Value.TextDocument.Value.CodeAction.IsSome)
                && (clientCapabilities.Value.TextDocument.Value.CodeAction.Value.DataSupport = Some true)
                && (clientCapabilities.Value.TextDocument.Value.CodeAction.Value.ResolveSupport.IsSome)
                && (clientCapabilities.Value.TextDocument.Value.CodeAction.Value.ResolveSupport.Value.Properties |> Array.contains "edit")

        let! lspCodeActions =
            match clientSupportsCodeActionEditResolveWithEditAndData with
            | true -> async {
                let toUnresolvedLspCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
                    let resolutionData: CSharpCodeActionResolutionData =
                        { TextDocumentUri = actionParams.TextDocument.Uri
                          Range = actionParams.Range }

                    let caData = JsonConvert.SerializeObject(resolutionData)

                    let lspCa = roslynCodeActionToUnresolvedLspCodeAction ca
                    { lspCa with Data = Some caData }

                return roslynCodeActions |> Seq.map toUnresolvedLspCodeAction |> Array.ofSeq
              }

            | false -> async {
                let results = List<CodeAction>()

                for ca in roslynCodeActions do
                    let! maybeLspCa =
                        roslynCodeActionToResolvedLspCodeAction
                            state.Solution.Value
                            state.OpenDocVersions.TryFind
                            logMessage
                            doc
                            ca

                    if maybeLspCa.IsSome then
                        results.Add(maybeLspCa.Value)

                return results |> Array.ofSeq
              }

        return lspCodeActions
               |> TextDocumentCodeActionResult.CodeActions
               |> Some
               |> success
}

let handleCodeActionResolve state logMessage (codeAction: CodeAction): AsyncLspResult<CodeAction option> = async {
    let resolutionData =
        codeAction.Data
        |> Option.map (fun x -> x :?> string)
        |> Option.map JsonConvert.DeserializeObject<CSharpCodeActionResolutionData>

    match getDocumentForUri state resolutionData.Value.TextDocumentUri with
    | None ->
        return None |> success

    | Some doc ->
        let! docText = doc.GetTextAsync() |> Async.AwaitTask

        let textSpan = resolutionData.Value.Range
                       |> roslynLinePositionSpanForLspRange
                       |> docText.Lines.GetTextSpan

        let! roslynCodeActions = getRoslynCodeActions doc textSpan

        let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = codeAction.Title)

        let toResolvedLspCodeAction =
            roslynCodeActionToResolvedLspCodeAction state.Solution.Value
                                                    state.OpenDocVersions.TryFind
                                                    logMessage
                                                    doc

        let! maybeLspCodeAction =
            match selectedCodeAction with
            | Some ca -> async { return! toResolvedLspCodeAction ca }
            | None -> async { return None }

        return maybeLspCodeAction |> success
}

let handleTextDocumentDefinition state logMessage (def: Types.TextDocumentPositionParams): Async<(LspResult<Types.GotoResult option> * ServerState)> = async {
    let resolveDefinition (doc: Document) = task {
        let! sourceText = doc.GetTextAsync()
        let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
        let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position)
        let! compilation = doc.Project.GetCompilationAsync()

        let! (locations, newState) = async {
            match Option.ofObj symbolMaybe with
            | None ->
                return ([], state)

            | Some sym ->
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

                    let mdDocumentFilename = $"$metadata$/projects/{doc.Project.Name}/assemblies/{mdLocation.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
                    let mdDocumentEmpty = doc.Project.AddDocument(mdDocumentFilename, String.Empty)
                    let mdDocument = SourceText.From(text) |> mdDocumentEmpty.WithText

                    let newDecompiledMetadataMap = state.DecompiledMetadata |> Map.add uri { Metadata = csharpMetadata; Document = mdDocument }
                    let newState = { state with DecompiledMetadata = newDecompiledMetadataMap }

                    // figure out location on the document (approx implementation)
                    let! syntaxTree = mdDocument.GetSyntaxTreeAsync() |> Async.AwaitTask
                    let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym.Name)
                    collector.Visit(syntaxTree.GetRoot())

                    let fallbackLocationInMetadata = {
                        Uri = uri
                        Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 1 } } }

                    let locationInMetadata =
                        (collector.GetLocations() @ [ fallbackLocationInMetadata ])
                        |> Seq.head

                    return ([locationInMetadata], newState)
                else
                    let lspLocations = match List.ofSeq locationsInSource with
                                       | [] -> []
                                       | locations -> locations
                                                      |> Seq.map lspLocationForRoslynLocation
                                                      |> List.ofSeq
                    return (lspLocations, state)
            }

        return (locations, newState)
    }

    match getDocumentForUri state def.TextDocument.Uri with
    | Some doc -> let! (locations, newState) = resolveDefinition doc |> Async.AwaitTask
                  let result = locations |> Array.ofSeq |> GotoResult.Multiple
                  return (Some result |> success, newState)
    | None ->
        logMessage (sprintf "getDocumentForUri: no document for uri %s" (def.TextDocument.Uri |> string))
        return (None |> success, state)
}

let handleTextDocumentCompletion state _logMessage (posParams: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = async {
    match getDocumentForUri state posParams.TextDocument.Uri with
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

let handleTextDocumentDocumentHighlight state _logMessage (docParams: Types.TextDocumentPositionParams): AsyncLspResult<Types.DocumentHighlight [] option> = async {

    let getSymbolLocations symbol doc solution = async {
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
    }

    let shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    match state.Solution with
    | Some solution ->
        let! maybeSymbol = getSymbolAtPosition state docParams.TextDocument.Uri docParams.Position

        match maybeSymbol with
        | Some (symbol, doc) ->
            if shouldHighlight symbol then
                let! locations = getSymbolLocations symbol doc solution
                return locations |> Array.ofSeq |> Some |> success
            else
                return None |> success

        | None -> return None |> success

    | None -> return None |> success
}

let handleTextDocumentDocumentSymbol state _logMessage (p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
    match getDocumentForUri state p.TextDocument.Uri with
    | Some doc ->
        let collector = DocumentSymbolCollector(p.TextDocument.Uri)

        let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
        collector.Visit(syntaxTree.GetRoot())

        return collector.GetSymbols() |> Some |> success

    | None ->
        return None |> success
}

let handleTextDocumentHover state _logMessage (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = async {
    let! maybeSymbol = getSymbolAtPosition state hoverPos.TextDocument.Uri hoverPos.Position

    let getSymbolDocumentation (sym: ISymbol) =
        let csharpMarkdownDoc =
            let containingAssemblyName =
                sym.ContainingAssembly |> Option.ofObj |> Option.map (fun a -> a.Name) |> Option.defaultValue ""

            Documentation.formatDocXmlWithTypeInfo
                (sym.GetDocumentationCommentXml())
                (sym.ToString())
                containingAssemblyName

        //logMessage (sprintf "debug: xml=%s; markdown=%s" (sym.GetDocumentationCommentXml()) csharpMarkdownDoc)

        [MarkedString.WithLanguage { Language = "markdown"; Value = csharpMarkdownDoc }]

    let suggestUnderlyingSymbolType (sym: ISymbol) =
        match sym with
        | :? ILocalSymbol as ps -> ps.Type :> ISymbol
        | _ -> sym

    let contents =
        match maybeSymbol with
        | Some (sym, _) -> sym |> suggestUnderlyingSymbolType |> getSymbolDocumentation
        | _ -> []

    return Some { Contents = contents |> Array.ofSeq |> MarkedStrings
                  Range = None } |> success
}

let handleTextDocumentReferences state _logMessage (refParams: Types.ReferenceParams): AsyncLspResult<Types.Location [] option> = async {
    match state.Solution with
    | Some solution ->
        let! maybeSymbol = getSymbolAtPosition state refParams.TextDocument.Uri refParams.Position

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

let handleTextDocumentRename state logMessage (rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = async {
    let renameSymbolInDoc symbol (doc: Document) = async {
        let originalSolution = doc.Project.Solution

        let! updatedSolution =
            Renamer.RenameSymbolAsync(doc.Project.Solution,
                                      symbol,
                                      rename.NewName,
                                      doc.Project.Solution.Workspace.Options)
            |> Async.AwaitTask

        let! docTextEdit =
            lspDocChangesFromSolutionDiff originalSolution
                                          updatedSolution
                                          state.OpenDocVersions.TryFind
                                          logMessage
                                          doc
        return docTextEdit
    }

    let! maybeSymbol = getSymbolAtPosition state rename.TextDocument.Uri rename.Position

    let! docChanges = match maybeSymbol with
                      | Some (symbol, doc) -> renameSymbolInDoc symbol doc
                      | None -> async { return [] }

    return WorkspaceEdit.Create (docChanges |> Array.ofList, state.ClientCapabilities.Value) |> Some |> success
}

let handleWorkspaceSymbol state _logMessage (symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
    let! symbols = findSymbols state.Solution.Value symbolParams.Query (Some 20)
    return symbols |> Array.ofSeq |> Some |> success
}

let handleCSharpMetadata state _logMessage (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = async {
    let uri = metadataParams.TextDocument.Uri

    return state.DecompiledMetadata
           |> Map.tryFind uri
           |> Option.map (fun x -> x.Metadata)
           |> success
}

let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let notificationHandlingWithStateChange fn =
        let wrappedHandler (s: CSharpLspServer) p = async {
            let! (response, newState) = fn s.State s.LogMessage p
            s.State <- newState
            return Result.Ok ()
        }

        requestHandling wrappedHandler

    let requestHandlingWithStateChange fn =
        let wrappedHandler (s: CSharpLspServer) p = async {
            let! (response, newState) = fn s.State s.LogMessage p
            s.State <- newState
            return response
        }

        requestHandling wrappedHandler

    let requestHandlingWithState fn =
        requestHandling (fun (s: CSharpLspServer) p -> fn s.State s.LogMessage p)

    let requestHandlings =
        defaultRequestHandlings<CSharpLspServer> ()
        |> Map.add "textDocument/didClose"          (handleTextDocumentDidClose          |> notificationHandlingWithStateChange)
        |> Map.add "textDocument/codeAction"        (handleTextDocumentCodeAction        |> requestHandlingWithState)
        |> Map.add "codeAction/resolve"             (handleCodeActionResolve             |> requestHandlingWithState)
        |> Map.add "textDocument/definition"        (handleTextDocumentDefinition        |> requestHandlingWithStateChange)
        |> Map.add "textDocument/completion"        (handleTextDocumentCompletion        |> requestHandlingWithState)
        |> Map.add "textDocument/documentHighlight" (handleTextDocumentDocumentHighlight |> requestHandlingWithState)
        |> Map.add "textDocument/documentSymbol"    (handleTextDocumentDocumentSymbol    |> requestHandlingWithState)
        |> Map.add "textDocument/hover"             (handleTextDocumentHover             |> requestHandlingWithState)
        |> Map.add "textDocument/references"        (handleTextDocumentReferences        |> requestHandlingWithState)
        |> Map.add "textDocument/rename"            (handleTextDocumentRename            |> requestHandlingWithState)
        |> Map.add "workspace/symbol"               (handleWorkspaceSymbol               |> requestHandlingWithState)
        |> Map.add "csharp/metadata"                (handleCSharpMetadata                |> requestHandlingWithState)

    Ionide.LanguageServerProtocol.Server.start requestHandlings
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
