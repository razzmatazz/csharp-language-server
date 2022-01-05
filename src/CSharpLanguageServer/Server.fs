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

type ServerStateEffect =
    | StateReplace of ServerState
    | ClientCapabilityChange of ClientCapabilities option
    | SolutionChange of Solution
    | DecompiledMetadataAdd of string * DecompiledMetadata
    | OpenDocVersionAdd of string * int
    | OpenDocVersionRemove of string
    | PublishDiagnosticsOnDocument of string * Document

let emptyServerState = { ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty
                         Options = emptyOptions }

let getDocumentForUri state u =
    let uri = Uri u

    match state.Solution with
    | Some solution ->
        let matchingDocuments = solution.Projects
                                |> Seq.collect (fun p -> p.Documents)
                                |> Seq.filter (fun d -> Uri("file://" + d.FilePath) = uri) |> List.ofSeq

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
        let! ct = Async.CancellationToken
        let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
        let position = sourceText.Lines.GetPosition(LinePosition(pos.Line, pos.Character))
        let! symbolRef = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
        return if isNull symbolRef then None else Some (symbolRef, doc, position)
    }

    withDocumentOnUriOrNone state resolveSymbolOrNull uri

let makeDocumentFromMetadata
        (compilation: Microsoft.CodeAnalysis.Compilation)
        (project: Microsoft.CodeAnalysis.Project)
        (l: Microsoft.CodeAnalysis.Location)
        (fullName: string) =
    let mdLocation = l
    let reference = compilation.GetMetadataReference(mdLocation.MetadataModule.ContainingAssembly)
    let peReference = reference :?> PortableExecutableReference |> Option.ofObj
    let assemblyLocation = peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

    let decompiler = CSharpDecompiler(assemblyLocation, DecompilerSettings())

    // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
    // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
    decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

    let fullTypeName = ICSharpCode.Decompiler.TypeSystem.FullTypeName(fullName)

    let text = decompiler.DecompileTypeAsString(fullTypeName)

    let mdDocumentFilename = $"$metadata$/projects/{project.Name}/assemblies/{mdLocation.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
    let mdDocumentEmpty = project.AddDocument(mdDocumentFilename, String.Empty)

    let mdDocument = SourceText.From(text) |> mdDocumentEmpty.WithText
    (mdDocument, text)

let resolveSymbolLocation
        (state: ServerState)
        (compilation: Microsoft.CodeAnalysis.Compilation)
        (project: Microsoft.CodeAnalysis.Project)
        sym
        (l: Microsoft.CodeAnalysis.Location) = async {

    let! ct = Async.CancellationToken

    if l.IsInMetadata then
        let fullName = sym |> getContainingTypeOrThis |> getFullReflectionName
        let uri = $"csharp:/metadata/projects/{project.Name}/assemblies/{l.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"

        let mdDocument, stateChanges =
            match Map.tryFind uri state.DecompiledMetadata with
            | Some value ->
                (value.Document, [])
            | None ->
                let (documentFromMd, text) = makeDocumentFromMetadata compilation project l fullName

                let csharpMetadata = { ProjectName = project.Name
                                       AssemblyName = l.MetadataModule.ContainingAssembly.Name
                                       SymbolName = fullName
                                       Source = text }
                (documentFromMd, [
                     DecompiledMetadataAdd (uri, { Metadata = csharpMetadata; Document = documentFromMd })])

        // figure out location on the document (approx implementation)
        let! syntaxTree = mdDocument.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
        let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym.Name)
        collector.Visit(syntaxTree.GetRoot())

        let fallbackLocationInMetadata = {
            Uri = uri
            Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 1 } } }

        let resolved =
            match collector.GetLocations() with
            | [] -> [fallbackLocationInMetadata]
            | ls -> ls

        return resolved, stateChanges

    else if l.IsInSource then
        let resolved = [lspLocationForRoslynLocation l]

        return resolved, []
    else
        return [], []
}

let resolveSymbolLocations
        (state: ServerState)
        (project: Microsoft.CodeAnalysis.Project)
        (symbols: Microsoft.CodeAnalysis.ISymbol list) = async {
    let! ct = Async.CancellationToken
    let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

    let mutable aggregatedLspLocations = []
    let mutable aggregatedStateChanges = []

    for sym in symbols do
        for l in sym.Locations do
            let! (symLspLocations, stateChanges) =
                resolveSymbolLocation state compilation project sym l

            aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations
            aggregatedStateChanges <- aggregatedStateChanges @ stateChanges

    return (aggregatedLspLocations, aggregatedStateChanges)
}

type CodeLensData = { DocumentUri: string; Position: Position  }

let emptyCodeLensData = { DocumentUri=""; Position={ Line=0; Character=0 } }

type CSharpLspServer(lspClient: CSharpLspClient, options: Options) =
    inherit LspServer()

    let mutable _state: ServerState = { emptyServerState with Options = options }
    let stateRWLock = new Nito.AsyncEx.AsyncReaderWriterLock()

    let withStateRead asyncOp = async {
        let! ct = Async.CancellationToken
        use! rwLock = stateRWLock.ReaderLockAsync(ct).AsTask() |> Async.AwaitTask
        let! opResult = asyncOp _state
        return opResult
    }

    let withStateEffects asyncOp = async {
        let! ct = Async.CancellationToken
        use! rwLock = stateRWLock.WriterLockAsync(ct).AsTask() |> Async.AwaitTask
        let! opResult, effects = asyncOp _state

        let applyServerStateChange state change = async {
            match change with
            | StateReplace newState ->
                return newState

            | ClientCapabilityChange cc ->
                return { state with ClientCapabilities = cc }

            | SolutionChange s ->
                return { state with Solution = Some s }

            | DecompiledMetadataAdd (uri, md) ->
                let newDecompiledMd = Map.add uri md state.DecompiledMetadata
                return { state with DecompiledMetadata = newDecompiledMd }

            | OpenDocVersionAdd (doc, ver) ->
                let newOpenDocVersions = Map.add doc ver state.OpenDocVersions
                return { state with OpenDocVersions = newOpenDocVersions }

            | OpenDocVersionRemove uri ->
                let newOpenDocVersions = state.OpenDocVersions |> Map.remove uri
                return { state with OpenDocVersions = newOpenDocVersions }

            | PublishDiagnosticsOnDocument (docUri, doc) ->
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                let diagnostics =
                    semanticModel.GetDiagnostics()
                    |> Seq.map RoslynHelpers.roslynToLspDiagnostic
                    |> Array.ofSeq

                do! lspClient.TextDocumentPublishDiagnostics { Uri = docUri; Diagnostics = diagnostics }

                return state
        }

        for effect in effects do
            let! newState = applyServerStateChange _state effect
            _state <- newState

        return opResult
    }

    let logMessageWithLevel l message =
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do lspClient.WindowShowMessage messageParams |> Async.StartAsTask |> ignore

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info
    let warningMessage = logMessageWithLevel MessageType.Warning
    let errorMessage = logMessageWithLevel MessageType.Error

    override __.Dispose(): unit = ()

    override __.Initialize (p: InitializeParams): AsyncLspResult<InitializeResult> = withStateEffects <| fun state -> async {
        let options = state.Options

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
                                     Change = Some TextDocumentSyncKind.Incremental
                                 }
                        FoldingRangeProvider = None
                        SelectionRangeProvider = None
                        SemanticTokensProvider = None
                    }
            }

        let effects = [SolutionChange solution.Value
                       ClientCapabilityChange p.Capabilities]

        return (result |> success, effects)
    }

    override __.TextDocumentDidOpen (openParams: Types.DidOpenTextDocumentParams): Async<unit> = withStateEffects <| fun state -> async {
        let! ct = Async.CancellationToken
        let docFilePath = openParams.TextDocument.Uri.Substring("file://".Length)

        let effects =
            match getDocumentForUri state openParams.TextDocument.Uri with
            | Some doc ->
                // we want to load the document in case it has been changed since we have the solution loaded
                // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                // went out of sync with editor
                let updatedDoc = SourceText.From(openParams.TextDocument.Text) |> doc.WithText

                [ OpenDocVersionAdd (openParams.TextDocument.Uri, openParams.TextDocument.Version)
                  SolutionChange updatedDoc.Project.Solution
                  PublishDiagnosticsOnDocument (openParams.TextDocument.Uri, updatedDoc) ]

            | None ->
                // ok, this document is not on solution, register a new one
                let newDocMaybe = tryAddDocument logMessage
                                                 docFilePath
                                                 openParams.TextDocument.Text
                                                 state.Solution.Value
                match newDocMaybe with
                | Some newDoc -> [ SolutionChange newDoc.Project.Solution
                                   PublishDiagnosticsOnDocument (openParams.TextDocument.Uri, newDoc) ]

                | None -> []

        return (), effects
    }

    override __.TextDocumentDidChange (changeParams: Types.DidChangeTextDocumentParams): Async<unit> = withStateEffects <| fun state -> async {
        let! ct = Async.CancellationToken

        let isUriADecompiledDoc u = state.DecompiledMetadata |> Map.containsKey u

        if not (isUriADecompiledDoc changeParams.TextDocument.Uri) then
            match getDocumentForUri state changeParams.TextDocument.Uri with
            | Some doc ->
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                //logMessage (sprintf "TextDocumentDidChange: changeParams: %s" (string changeParams))
                //logMessage (sprintf "TextDocumentDidChange: sourceText: %s" (string sourceText))

                let updatedSourceText = sourceText |> applyLspContentChangesOnRoslynSourceText changeParams.ContentChanges
                let updatedDoc = doc.WithText(updatedSourceText)

                //logMessage (sprintf "TextDocumentDidChange: newSourceText: %s" (string updatedSourceText))

                let updatedSolution = updatedDoc.Project.Solution

                let effects = [ SolutionChange updatedSolution
                                OpenDocVersionAdd (changeParams.TextDocument.Uri, changeParams.TextDocument.Version |> Option.defaultValue 0)
                                PublishDiagnosticsOnDocument (changeParams.TextDocument.Uri, updatedDoc) ]

                return (), effects

            | None -> return (), []
        else
            return (), []
    }

    override __.TextDocumentDidSave (saveParams: Types.DidSaveTextDocumentParams): Async<unit> = withStateEffects <| fun state -> async {
        // we need to add this file to solution if not already
        match getDocumentForUri state saveParams.TextDocument.Uri with
        | Some _ ->
            return (), []

        | None ->
            let docFilePath = saveParams.TextDocument.Uri.Substring("file://".Length)
            let newDocMaybe = tryAddDocument logMessage
                                             docFilePath
                                             saveParams.Text.Value
                                             state.Solution.Value
            match newDocMaybe with
            | Some newDoc ->
                let effects = [ SolutionChange newDoc.Project.Solution
                                PublishDiagnosticsOnDocument (saveParams.TextDocument.Uri, newDoc) ]
                return (), effects

            | None ->
                return (), []
    }

    override __.TextDocumentDidClose (closeParams: Types.DidCloseTextDocumentParams): Async<unit> = withStateEffects <| fun state -> async {
        let effects = [ OpenDocVersionRemove closeParams.TextDocument.Uri ]
        return (), effects
    }

    override __.TextDocumentCodeAction (actionParams: Types.CodeActionParams): AsyncLspResult<Types.TextDocumentCodeActionResult option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state actionParams.TextDocument.Uri with
        | None ->
            return None |> success

        | Some doc ->
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan = actionParams.Range
                                          |> roslynLinePositionSpanForLspRange
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

            return
               lspCodeActions
               |> Seq.sortByDescending (fun ca -> ca.IsPreferred)
               |> Array.ofSeq
               |> TextDocumentCodeActionResult.CodeActions
               |> Some
               |> success
    }

    override __.CodeActionResolve (codeAction: CodeAction): AsyncLspResult<CodeAction option> = withStateRead <| fun state -> async {
        let resolutionData =
            codeAction.Data
            |> Option.map (fun x -> x :?> string)
            |> Option.map JsonConvert.DeserializeObject<CSharpCodeActionResolutionData>

        let! ct = Async.CancellationToken

        match getDocumentForUri state resolutionData.Value.TextDocumentUri with
        | None ->
            return None |> success

        | Some doc ->
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan =
                       resolutionData.Value.Range
                       |> roslynLinePositionSpanForLspRange
                       |> docText.Lines.GetTextSpan

            let! roslynCodeActions = getRoslynCodeActions doc textSpan

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = codeAction.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    state.Solution.Value
                                                    state.OpenDocVersions.TryFind
                                                    logMessage
                                                    doc

            let! maybeLspCodeAction =
                match selectedCodeAction with
                | Some ca -> async { return! toResolvedLspCodeAction ca }
                | None -> async { return None }

            return maybeLspCodeAction |> success
    }

    override __.TextDocumentCodeLens (lensParams: CodeLensParams): AsyncLspResult<CodeLens[] option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state lensParams.TextDocument.Uri with
        | Some doc ->
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForCodeLens(semanticModel)
            collector.Visit(syntaxTree.GetRoot())

            let makeCodeLens (_symbol: ISymbol, location: Microsoft.CodeAnalysis.Location): CodeLens =
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

        | None ->
            return None |> success
    }

    override __.CodeLensResolve (codeLens: CodeLens) : AsyncLspResult<CodeLens> = withStateRead <| fun state -> async {
        let lensData =
            codeLens.Data
            |> Option.map (fun t -> t.ToObject<CodeLensData>())
            |> Option.defaultValue emptyCodeLensData

        let doc = lensData.DocumentUri |> getDocumentForUri state |> fun o -> o.Value

        let! ct = Async.CancellationToken
        let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

        let position =
            LinePosition(lensData.Position.Line, lensData.Position.Character)
            |> sourceText.Lines.GetPosition

        let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

        let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, ct) |> Async.AwaitTask

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

    override __.TextDocumentDefinition (def: Types.TextDocumentPositionParams) : AsyncLspResult<Types.GotoResult option> = withStateEffects <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state def.TextDocument.Uri with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

            let symbols =
                match Option.ofObj symbolMaybe with
                | Some sym -> [sym]
                | None -> []

            let! (locations, stateChanges) = resolveSymbolLocations state doc.Project symbols

            let lspResult = locations |> Array.ofSeq |> GotoResult.Multiple |> Some |> success

            return lspResult, stateChanges

        | None ->
            return None |> success, []
    }

    override __.TextDocumentImplementation (def: Types.TextDocumentPositionParams): AsyncLspResult<Types.GotoResult option> = withStateEffects <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state def.TextDocument.Uri with
        | Some doc ->
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

            let! symbols = async {
                match Option.ofObj symbolMaybe with
                | Some sym ->
                    let! implSymbols =
                        SymbolFinder.FindImplementationsAsync(sym, state.Solution.Value)
                        |> Async.AwaitTask

                    return implSymbols |> List.ofSeq
                | None -> return []
            }

            let! (locations, stateChanges) = resolveSymbolLocations state doc.Project symbols

            let lspResult = locations |> Array.ofSeq |> GotoResult.Multiple |> Some |> success
            return lspResult, stateChanges

        | None ->
            return None |> success, []
    }

    override __.TextDocumentCompletion (posParams: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state posParams.TextDocument.Uri with
        | Some doc ->
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask
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

    override __.TextDocumentDocumentHighlight (docParams: Types.TextDocumentPositionParams): AsyncLspResult<Types.DocumentHighlight [] option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        let getSymbolLocations symbol doc solution = async {
            let docSet = ImmutableHashSet<Document>.Empty.Add(doc)
            let! refs = SymbolFinder.FindReferencesAsync(symbol, solution, docSet, ct) |> Async.AwaitTask
            let locationsFromRefs = refs |> Seq.collect (fun r -> r.Locations) |> Seq.map (fun rl -> rl.Location)

            let! defRef = SymbolFinder.FindSourceDefinitionAsync(symbol, solution, ct) |> Async.AwaitTask

            let locationsFromDef =
                match Option.ofObj defRef with
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
            | Some (symbol, doc, _) ->
                if shouldHighlight symbol then
                    let! locations = getSymbolLocations symbol doc solution
                    return locations |> Array.ofSeq |> Some |> success
                else
                    return None |> success

            | None -> return None |> success

        | None -> return None |> success
    }

    override __.TextDocumentDocumentSymbol (p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        match getDocumentForUri state p.TextDocument.Uri with
        | Some doc ->
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let showAttributes = false
            let collector = DocumentSymbolCollector(p.TextDocument.Uri, semanticModel, showAttributes)

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetSymbols() |> Some |> success

        | None ->
            return None |> success
    }

    override __.TextDocumentHover (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        let csharpMarkdownDocForSymbol (sym: ISymbol) (semanticModel: SemanticModel) pos =
            let symbolInfo = symbolToLspSymbolInformation true sym (Some semanticModel) (Some pos)

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
                    let docAssembly = semanticModel.Compilation.Assembly
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

        let! maybeSymbol = getSymbolAtPosition state hoverPos.TextDocument.Uri hoverPos.Position

        let! contents =
            match maybeSymbol with
            | Some (sym, doc, pos) -> async {
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                return csharpMarkdownDocForSymbol sym semanticModel pos
              }
            | _ -> async { return [] }

        return Some { Contents = contents |> Array.ofSeq |> MarkedStrings
                      Range = None } |> success
    }

    override __.TextDocumentReferences (refParams: Types.ReferenceParams): AsyncLspResult<Types.Location [] option> = withStateRead <| fun state -> async {
        let! ct = Async.CancellationToken

        match state.Solution with
        | Some solution ->
            let! maybeSymbol = getSymbolAtPosition state refParams.TextDocument.Uri refParams.Position

            match maybeSymbol with
            | Some (symbol, _, _) ->
                let! refs = SymbolFinder.FindReferencesAsync(symbol, solution, ct) |> Async.AwaitTask
                return refs
                        |> Seq.collect (fun r -> r.Locations)
                        |> Seq.map (fun rl -> lspLocationForRoslynLocation rl.Location)
                        |> Array.ofSeq
                        |> Some
                        |> success

            | None -> return None |> success

        | None -> return None |> success
    }

    override __.TextDocumentRename (rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = withStateRead <| fun state -> async {
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

        let! docChanges =
            match maybeSymbol with
                      | Some (symbol, doc, _) -> renameSymbolInDoc symbol doc
                      | None -> async { return [] }

        return WorkspaceEdit.Create (docChanges |> Array.ofList, state.ClientCapabilities.Value) |> Some |> success
    }

    override __.WorkspaceSymbol (symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = withStateRead <| fun state -> async {
        let! symbols = findSymbolsInSolution state.Solution.Value symbolParams.Query (Some 20)
        return symbols |> Array.ofSeq |> Some |> success
    }

    member __.CSharpMetadata (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = withStateRead <| fun state -> async {
        let uri = metadataParams.TextDocument.Uri

        let metadataMaybe =
            state.DecompiledMetadata
            |> Map.tryFind uri
            |> Option.map (fun x -> x.Metadata)

        return metadataMaybe |> success
    }

let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestHandlings =
        defaultRequestHandlings<CSharpLspServer> ()
        |> Map.add "csharp/metadata" (requestHandling (fun s p -> s.CSharpMetadata p))

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
