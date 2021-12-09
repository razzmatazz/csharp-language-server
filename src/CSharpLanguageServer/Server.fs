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
open Newtonsoft.Json.Linq

type Options = {
    SolutionPath: string option;
    LogLevel: Types.MessageType;
}

let emptyOptions = { SolutionPath = None; LogLevel = Types.MessageType.Log }

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
    Options: Options
}

let emptyServerState = { ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty
                         Options = emptyOptions }

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

type HandlerResult<'Result> = {
    State: ServerState
    DiagnosticsToPublishOnDocument: (string * Microsoft.CodeAnalysis.Document) list
    MessagesToLog: (MessageType * string) list
    LspResult: LspResult<'Result>;
}

let emptyNotificationHandlerResult = {
        State = emptyServerState
        DiagnosticsToPublishOnDocument = []
        MessagesToLog = []
        LspResult = Result.Ok () }

let emptyNotificationHandlerResultWithState state = { emptyNotificationHandlerResult with State = state }

let handlerResultOf result state = {
        State = state;
        DiagnosticsToPublishOnDocument = [];
        MessagesToLog = [];
        LspResult = success result }

type CSharpLspServer(lspClient: CSharpLspClient, options: Options) =
    inherit LspServer()

    let mutable state: ServerState = { emptyServerState with Options = options }

    member _.State
        with get() = state
        and set(newState) = state <- newState

    member _.LspClient
        with get() = lspClient

    override __.Dispose(): unit = ()


let handleInitialize state (p: InitializeParams): Async<HandlerResult<InitializeResult>> = async {
    let options = state.Options

    let logMessages = List<(MessageType * string)>()
    let logMessage m = logMessages.Add((MessageType.Log, m))
    let infoMessage m = logMessages.Add((MessageType.Log, m))

    infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                            (typeof<CSharpLspServer>.Assembly.GetName().Version |> string)
                            (JsonConvert.SerializeObject(options, StringEnumConverter())))
    infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

    let! solution =
        match options.SolutionPath with
        | Some solutionPath -> async {
            infoMessage (sprintf "Initialize: loading specified solution file: %s.." solutionPath)
            return! tryLoadSolutionOnPath logMessage solutionPath
          }

        | None -> async {
            let cwd = Directory.GetCurrentDirectory()
            infoMessage (sprintf "attempting to find and load solution based on cwd: \"%s\".." cwd)
            return! findAndLoadSolutionOnDir logMessage cwd
          }

    let newState = { state with Solution = solution
                                ClientCapabilities = p.Capabilities }

    infoMessage "ok, solution loaded -- initialization finished"

    let result =
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = Some true
                        DefinitionProvider = Some true
                        TypeDefinitionProvider = None
                        ImplementationProvider = Some true
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
                        CodeLensProvider =
                             Some { ResolveProvider = Some true }
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

    return { handlerResultOf result newState with
                 MessagesToLog = logMessages |> List.ofSeq }
}

let handleTextDocumentDidOpen state (openParams: Types.DidOpenTextDocumentParams): Async<HandlerResult<unit>> =
    match getDocumentForUri state openParams.TextDocument.Uri with
    | Some doc -> async {
        let newState = { state with OpenDocVersions = state.OpenDocVersions.Add(openParams.TextDocument.Uri, openParams.TextDocument.Version) }

        return { emptyNotificationHandlerResult with
                     State = newState;
                     DiagnosticsToPublishOnDocument = [(openParams.TextDocument.Uri, doc)] }
      }

    | None -> async {
        let docFilePath = openParams.TextDocument.Uri.Substring("file://".Length)

        let logMessages = List<string>()
        let logMessage m = logMessages.Add(m)

        let newDocMaybe = tryAddDocument logMessage
                                             docFilePath
                                             openParams.TextDocument.Text
                                             state.Solution.Value
        match newDocMaybe with
        | Some newDoc ->
            return { emptyNotificationHandlerResult with
                         State = { state with Solution = Some newDoc.Project.Solution }
                         DiagnosticsToPublishOnDocument = [(openParams.TextDocument.Uri, newDoc)] }

        | None ->
            return emptyNotificationHandlerResultWithState state
      }

let handleTextDocumentDidChange state (changeParams: Types.DidChangeTextDocumentParams): Async<HandlerResult<unit>> =
    let handlerResult =
        emptyNotificationHandlerResultWithState
            { state with OpenDocVersions = state.OpenDocVersions.Add(changeParams.TextDocument.Uri,
                                                                     changeParams.TextDocument.Version |> Option.defaultValue 0) }


    let isUriADecompiledDoc state u = Map.containsKey u state.DecompiledMetadata

    match isUriADecompiledDoc state changeParams.TextDocument.Uri with
    | true -> async { return handlerResult }
    | false ->
        match getDocumentForUri state changeParams.TextDocument.Uri with
        | Some doc ->
            let fullText = SourceText.From(changeParams.ContentChanges.[0].Text)

            let updatedDoc = doc.WithText(fullText)
            let updatedSolution = updatedDoc.Project.Solution;

            async { return { handlerResult with
                                State = { handlerResult.State with Solution = Some updatedSolution }
                                DiagnosticsToPublishOnDocument = [(changeParams.TextDocument.Uri, updatedDoc)]
                        } }
        | None ->
            async { return handlerResult }

let handleTextDocumentDidSave state (saveParams: Types.DidSaveTextDocumentParams): Async<HandlerResult<unit>> =
    // we need to add this file to solution if not already
    match getDocumentForUri state saveParams.TextDocument.Uri with
    | None ->
        let logMessages = List<(MessageType * string)>()
        let logMessage m = logMessages.Add((MessageType.Log, m))

        let docFilePath = saveParams.TextDocument.Uri.Substring("file://".Length)
        let newDocMaybe = tryAddDocument logMessage
                                             docFilePath
                                             saveParams.Text.Value
                                             state.Solution.Value
        match newDocMaybe with
        | Some newDoc -> async {
            let newState = { state with Solution = Some newDoc.Project.Solution }

            return { emptyNotificationHandlerResult with
                         State = newState
                         DiagnosticsToPublishOnDocument = [(saveParams.TextDocument.Uri, newDoc)]
                         MessagesToLog = logMessages |> List.ofSeq }
          }

        | None ->
            async { return emptyNotificationHandlerResultWithState state }

    | _ -> async { return emptyNotificationHandlerResultWithState state }

let handleTextDocumentDidClose state (closeParams: Types.DidCloseTextDocumentParams): Async<HandlerResult<unit>> =
    let newOpenDocVersions = state.OpenDocVersions.Remove(closeParams.TextDocument.Uri)
    let newState = { state with OpenDocVersions = newOpenDocVersions }
    async { return emptyNotificationHandlerResultWithState newState }

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
                    { lspCa with Data = Some (caData :> obj) }

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
               |> Seq.sortByDescending (fun ca -> ca.IsPreferred)
               |> Array.ofSeq
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

let resolveSymbolLocation (compilation: Microsoft.CodeAnalysis.Compilation)
                          state
                          (doc: Microsoft.CodeAnalysis.Document)
                          sym
                          (l: Microsoft.CodeAnalysis.Location) = async {
    if l.IsInMetadata then
        let mdLocation = l

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

        // figure out location on the document (approx implementation)
        let! syntaxTree = mdDocument.GetSyntaxTreeAsync() |> Async.AwaitTask
        let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym.Name)
        collector.Visit(syntaxTree.GetRoot())

        let fallbackLocationInMetadata = {
            Uri = uri
            Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 1 } } }

        let resolved =
            match collector.GetLocations() with
            | [] -> [fallbackLocationInMetadata]
            | ls -> ls

        let newDecompiledMetadataMap = state.DecompiledMetadata |> Map.add uri { Metadata = csharpMetadata; Document = mdDocument }

        let newState = { state with DecompiledMetadata = newDecompiledMetadataMap }

        return (resolved, newState)

    else if l.IsInSource then
        let resolved = [lspLocationForRoslynLocation l]

        return (resolved, state)
    else
        return ([], state)
}

let resolveSymbolLocations state
                           (doc: Document)
                           (symbols: Microsoft.CodeAnalysis.ISymbol list) = async {
    let! compilation = doc.Project.GetCompilationAsync() |> Async.AwaitTask

    let mutable lspLocations = []
    let mutable aggregatedState = state

    for sym in symbols do
        for l in sym.Locations do
            let! (symLspLocations, newState) =
                resolveSymbolLocation compilation aggregatedState doc sym l

            lspLocations <- lspLocations @ symLspLocations
            aggregatedState <- newState

    return (lspLocations, aggregatedState)
}

type CodeLensData = { DocumentUri: string; Position: Position  }
let emptyCodeLensData = { DocumentUri=""; Position={ Line=0; Character=0 } }

let handleTextDocumentCodeLens state _logMessage (lensParams: CodeLensParams)
        : AsyncLspResult<CodeLens[] option> =
    match getDocumentForUri state lensParams.TextDocument.Uri with
    | Some doc ->
        async {
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForCodeLens(semanticModel)
            collector.Visit(syntaxTree.GetRoot())

            let makeCodeLens (symbol: ISymbol, location: Microsoft.CodeAnalysis.Location): CodeLens =
                let start = location.GetLineSpan().Span.Start

                let lensData: CodeLensData = {
                    DocumentUri = lensParams.TextDocument.Uri
                    Position = start |> lspPositionForRoslynLinePosition
                }

                { Range = (location.GetLineSpan().Span) |> lspRangeForRoslynLinePosSpan
                  Command = None
                  Data = lensData |> JToken.FromObject |> Some
                }

            let codeLens = collector.GetSymbols() |> Seq.map makeCodeLens

            return codeLens |> Array.ofSeq |> Some |> success
        }

    | None ->
        async { return None |> success }

let handleCodeLensResolve state _logMessage (codeLens: CodeLens)
        : AsyncLspResult<CodeLens> =
    let lensData =
        codeLens.Data
        |> Option.map (fun t -> t.ToObject<CodeLensData>())
        |> Option.defaultValue emptyCodeLensData

    async {
        let doc = lensData.DocumentUri |> getDocumentForUri state |> fun o -> o.Value
        let! sourceText = doc.GetTextAsync() |> Async.AwaitTask

        let position =
            LinePosition(lensData.Position.Line, lensData.Position.Character)
            |> sourceText.Lines.GetPosition

        let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask

        let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution) |> Async.AwaitTask

        let locations = refs |> Seq.collect (fun r -> r.Locations)

        let formattedRefCount =
            locations |> Seq.length |> sprintf "%d Reference(s)"

        let command =
            { Title = formattedRefCount
              Command = "csharp.showReferences"
              Arguments = None // TODO: we really want to pass some more info to the client
            }

        return { codeLens with Command=Some command } |> success
    }

let handleTextDocumentDefinition state (def: Types.TextDocumentPositionParams)
        : Async<HandlerResult<Types.GotoResult option>> =

    match getDocumentForUri state def.TextDocument.Uri with
    | Some doc ->
        async {
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask

            let symbols =
                match Option.ofObj symbolMaybe with
                | Some sym -> [sym]
                | None -> []

            let! (locations, newState) = resolveSymbolLocations state doc symbols

            let result = locations |> Array.ofSeq |> GotoResult.Multiple |> Some

            return handlerResultOf result newState
        }

    | None ->
        async { return handlerResultOf None state }

let handleTextDocumentImplementation state (def: Types.TextDocumentPositionParams): Async<HandlerResult<Types.GotoResult option>> =
    match getDocumentForUri state def.TextDocument.Uri with
    | Some doc ->
        async {
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask

            let! symbols = async {
                match Option.ofObj symbolMaybe with
                | Some sym ->
                    let! implSymbols = SymbolFinder.FindImplementationsAsync(sym, state.Solution.Value) |> Async.AwaitTask
                    return implSymbols |> List.ofSeq
                | None -> return []
            }

            let! (locations, newState) = resolveSymbolLocations state doc symbols

            let result = locations |> Array.ofSeq |> GotoResult.Multiple |> Some

            return handlerResultOf result newState
        }

    | None ->
        async { return handlerResultOf None state }

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

let handleTextDocumentDocumentSymbol state _logMessage (p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> =
    match getDocumentForUri state p.TextDocument.Uri with
    | Some doc -> async {
        let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
        let showAttributes = false
        let collector = DocumentSymbolCollector(p.TextDocument.Uri, semanticModel, showAttributes)

        let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
        collector.Visit(syntaxTree.GetRoot())

        return collector.GetSymbols() |> Some |> success
      }

    | None ->
        async { return None |> success }

let handleTextDocumentHover state _logMessage (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> =
    let csharpMarkdownDocForSymbol (sym: ISymbol) (docAssembly: IAssemblySymbol) =
        let symbolInfo = symbolToLspSymbolInformation true sym

        let symAssemblyName =
            sym.ContainingAssembly
            |> Option.ofObj
            |> Option.map (fun a -> a.Name)
            |> Option.defaultValue ""

        let symbolInfoLines =
            match symbolInfo.Name, symAssemblyName with
            | "", "" -> []
            | typeName, "" -> [sprintf "`%s`" typeName]
            | _, _ ->
                if symAssemblyName = docAssembly.Name then
                    [sprintf "`%s`" symbolInfo.Name]
                else
                    [sprintf "`%s` from assembly `%s`" symbolInfo.Name symAssemblyName]

        let comment = Documentation.parseComment (sym.GetDocumentationCommentXml())
        let formattedDocLines = Documentation.formatComment comment

        let formattedDoc =
            formattedDocLines
            |> Seq.append (if symbolInfoLines.Length > 0 && formattedDocLines.Length > 0 then [""] else [])
            |> Seq.append symbolInfoLines
            |> (fun ss -> String.Join("\n", ss))

        [MarkedString.WithLanguage { Language = "markdown"; Value = formattedDoc }]

    async {
        let! maybeSymbol = getSymbolAtPosition state hoverPos.TextDocument.Uri hoverPos.Position

        let! contents =
            match maybeSymbol with
            | Some (sym, doc) -> async {
                let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
                return csharpMarkdownDocForSymbol sym semanticModel.Compilation.Assembly
              }
            | _ -> async { return [] }

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
    let! symbols = findSymbolsInSolution state.Solution.Value symbolParams.Query (Some 20)
    return symbols |> Array.ofSeq |> Some |> success
}

let handleCSharpMetadata state _logMessage (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = async {
    let uri = metadataParams.TextDocument.Uri

    return state.DecompiledMetadata
           |> Map.tryFind uri
           |> Option.map (fun x -> x.Metadata)
           |> success
}

let publishDiagnosticsOnDocument (lspClient: LspClient) docUri (doc: Document) = async {
    let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

    let diagnostics =
        semanticModel.GetDiagnostics()
        |> Seq.map RoslynHelpers.roslynToLspDiagnostic
        |> Array.ofSeq

    do! lspClient.TextDocumentPublishDiagnostics {
        Uri = docUri
        Diagnostics = diagnostics }

    return ()
}

let logMessageWithLevel (lspClient: LspClient) l message =
    let messageParams = { Type = l ; Message = "csharp-ls: " + message }
    do lspClient.WindowShowMessage messageParams |> ignore

let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestHandlingWithStateChange fn =
        let wrappedHandler (s: CSharpLspServer) p = async {
            let! result = fn s.State p
            s.State <- result.State

            for mt, m in result.MessagesToLog do
                do logMessageWithLevel s.LspClient mt m

            for uri, doc in result.DiagnosticsToPublishOnDocument do
                do! publishDiagnosticsOnDocument s.LspClient uri doc

            return result.LspResult
        }

        requestHandling wrappedHandler

    let requestHandlingWithState fn =
        requestHandling (fun (s: CSharpLspServer) p -> fn s.State (fun m -> logMessageWithLevel s.LspClient MessageType.Log m) p)

    let requestHandlings =
        defaultRequestHandlings<CSharpLspServer> ()
        |> Map.add "initialize"                     (handleInitialize                    |> requestHandlingWithStateChange)
        |> Map.add "textDocument/didOpen"           (handleTextDocumentDidOpen           |> requestHandlingWithStateChange)
        |> Map.add "textDocument/didChange"         (handleTextDocumentDidChange         |> requestHandlingWithStateChange)
        |> Map.add "textDocument/didSave"           (handleTextDocumentDidSave           |> requestHandlingWithStateChange)
        |> Map.add "textDocument/didClose"          (handleTextDocumentDidClose          |> requestHandlingWithStateChange)
        |> Map.add "textDocument/codeAction"        (handleTextDocumentCodeAction        |> requestHandlingWithState)
        |> Map.add "codeAction/resolve"             (handleCodeActionResolve             |> requestHandlingWithState)
        |> Map.add "textDocument/codeLens"          (handleTextDocumentCodeLens          |> requestHandlingWithState)
        |> Map.add "codeLens/resolve"               (handleCodeLensResolve               |> requestHandlingWithState)
        |> Map.add "textDocument/definition"        (handleTextDocumentDefinition        |> requestHandlingWithStateChange)
        |> Map.add "textDocument/implementation"    (handleTextDocumentImplementation    |> requestHandlingWithStateChange)
        |> Map.add "textDocument/completion"        (handleTextDocumentCompletion        |> requestHandlingWithState)
        |> Map.add "textDocument/documentHighlight" (handleTextDocumentDocumentHighlight |> requestHandlingWithState)
        |> Map.add "textDocument/documentSymbol"    (handleTextDocumentDocumentSymbol    |> requestHandlingWithState)
        |> Map.add "textDocument/hover"             (handleTextDocumentHover             |> requestHandlingWithState)
        |> Map.add "textDocument/references"        (handleTextDocumentReferences        |> requestHandlingWithState)
        |> Map.add "textDocument/rename"            (handleTextDocumentRename            |> requestHandlingWithState)
        |> Map.add "workspace/symbol"               (handleWorkspaceSymbol               |> requestHandlingWithState)
        |> Map.add "csharp/metadata"                (handleCSharpMetadata                |> requestHandlingWithState)

    Ionide.LanguageServerProtocol.Server.start
        requestHandlings
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
