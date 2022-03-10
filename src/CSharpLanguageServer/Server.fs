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

type DecompiledMetadataDocument = { Metadata: CSharpMetadataResponse; Document: Document }

type ServerDocumentType =
     | UserDocument // user Document from solution, on disk
     | DecompiledDocument // Document decompiled from metadata, readonly
     | AnyDocument

type ServerState = {
    ClientCapabilities: ClientCapabilities option
    Solution: Solution option
    OpenDocVersions: Map<string, int>
    DecompiledMetadata: Map<string, DecompiledMetadataDocument>
    Options: Options
    RunningChangeRequest: AsyncReplyChannel<ServerState> option
    ChangeRequestQueue: AsyncReplyChannel<ServerState> list
    SolutionReloadPending: DateTime option
}

let emptyServerState = { ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty
                         Options = emptyOptions
                         RunningChangeRequest = None
                         ChangeRequestQueue = []
                         SolutionReloadPending = None }

let getDocumentOfTypeForUri state docType (u: string) =
    let uri = Uri u

    match state.Solution with
    | Some solution ->
        let matchingUserDocuments =
            solution.Projects
            |> Seq.collect (fun p -> p.Documents)
            |> Seq.filter (fun d -> Uri("file://" + d.FilePath) = uri) |> List.ofSeq

        let matchingUserDocumentMaybe =
            match matchingUserDocuments with
            | [d] -> Some (d, UserDocument)
            | _ -> None

        let matchingDecompiledDocumentMaybe =
            Map.tryFind u state.DecompiledMetadata
            |> Option.map (fun x -> (x.Document, DecompiledDocument))

        match docType with
        | UserDocument -> matchingUserDocumentMaybe
        | DecompiledDocument -> matchingDecompiledDocumentMaybe
        | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

    | None -> None

type ServerStateEvent =
    | ClientCapabilityChange of ClientCapabilities option
    | SolutionChange of Solution
    | DecompiledMetadataAdd of string * DecompiledMetadataDocument
    | OpenDocVersionAdd of string * int
    | OpenDocVersionRemove of string
    | GetState of AsyncReplyChannel<ServerState>
    | StartSolutionChange of AsyncReplyChannel<ServerState>
    | FinishSolutionChange
    | SolutionReloadRequest
    | SolutionReload
    | PeriodicTimerTick

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

let rec processServerEvent logMessage state msg: Async<ServerState> = async {
    let! ct = Async.CancellationToken
    match msg with
    | GetState replyChannel ->
        replyChannel.Reply(state)
        return state

    | StartSolutionChange replyChannel ->
        // only reply if we don't have another request that would change solution;
        // otherwise enqueue the `replyChannel` to the ChangeRequestQueue
        match state.RunningChangeRequest with
        | Some _req ->
            return { state with
                         ChangeRequestQueue = state.ChangeRequestQueue @ [replyChannel] }
        | None ->
            replyChannel.Reply(state)
            return { state with
                         RunningChangeRequest = Some replyChannel }

    | FinishSolutionChange ->
        let newState = { state with RunningChangeRequest = None }

        return match state.ChangeRequestQueue with
               | [] -> newState
               | nextPendingRequest :: queueRemainder ->
                    nextPendingRequest.Reply(newState)
                    { newState with
                          ChangeRequestQueue = queueRemainder
                          RunningChangeRequest = Some nextPendingRequest }

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

    | SolutionReloadRequest ->
        // we need to wait a bit before starting this so we
        // can buffer many incoming requests at once
        return { state with SolutionReloadPending = DateTime.Now.AddSeconds(5) |> Some }

    | SolutionReload ->
        let! solution =
            match state.Options.SolutionPath with
            | Some solutionPath -> async {
                logMessage (sprintf "loading specified solution file: %s.." solutionPath)
                return! tryLoadSolutionOnPath logMessage solutionPath
              }

            | None -> async {
                let cwd = Directory.GetCurrentDirectory()
                logMessage (sprintf "attempting to find and load solution based on cwd: \"%s\".." cwd)
                return! findAndLoadSolutionOnDir logMessage cwd
              }

        return { state with Solution = solution }

    | PeriodicTimerTick ->
        let solutionReloadTime = state.SolutionReloadPending
                                 |> Option.defaultValue (DateTime.Now.AddDays(1))

        match solutionReloadTime < DateTime.Now with
        | true ->
            let! newState = processServerEvent logMessage state SolutionReload

            return { newState with SolutionReloadPending = None }

        | false ->
            return state
}

let serverEventLoop logMessage initialState (inbox: MailboxProcessor<ServerStateEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()
        let! newState = msg |> processServerEvent logMessage state
        return! loop newState
    }

    loop initialState

type ServerRequestScope (state: ServerState, emitServerEvent, onDispose: unit -> unit) =
    let mutable solutionMaybe = state.Solution

    interface IDisposable with
        member _.Dispose() = onDispose()

    member _.State = state
    member _.ClientCapabilities = state.ClientCapabilities
    member _.Solution = solutionMaybe.Value
    member _.OpenDocVersions = state.OpenDocVersions
    member _.DecompiledMetadata = state.DecompiledMetadata

    member this.GetDocumentForUriOfType docType (u: string) =
        let uri = Uri u

        match solutionMaybe with
        | Some solution ->
            let matchingUserDocuments =
                solution.Projects
                |> Seq.collect (fun p -> p.Documents)
                |> Seq.filter (fun d -> Uri("file://" + d.FilePath) = uri) |> List.ofSeq

            let matchingUserDocumentMaybe =
                match matchingUserDocuments with
                | [d] -> Some (d, UserDocument)
                | _ -> None

            let matchingDecompiledDocumentMaybe =
                Map.tryFind u this.DecompiledMetadata
                |> Option.map (fun x -> (x.Document, DecompiledDocument))

            match docType with
            | UserDocument -> matchingUserDocumentMaybe
            | DecompiledDocument -> matchingDecompiledDocumentMaybe
            | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe
        | None -> None

    member x.GetUserDocumentForUri (u: string) = x.GetDocumentForUriOfType UserDocument u |> Option.map fst
    member x.GetAnyDocumentForUri (u: string) = x.GetDocumentForUriOfType AnyDocument u |> Option.map fst

    member x.GetSymbolAtPositionOfType docType uri pos = async {
        match x.GetDocumentForUriOfType docType uri with
        | Some (doc, _docType) ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(pos.Line, pos.Character))
            let! symbolRef = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
            return if isNull symbolRef then None else Some (symbolRef, doc, position)

        | None ->
            return None
    }

    member x.GetSymbolAtPositionOnAnyDocument uri pos = x.GetSymbolAtPositionOfType AnyDocument uri pos
    member x.GetSymbolAtPositionOnUserDocument uri pos = x.GetSymbolAtPositionOfType UserDocument uri pos

    member x.Emit ev =
        match ev with
        | SolutionChange newSolution ->
            solutionMaybe <- Some newSolution
        | _ -> ()

        emitServerEvent ev

    member x.EmitMany es = for e in es do x.Emit e

type DiagnosticsEvent =
    | DocumentOpenOrChange of string * DateTime
    | DocumentClose of string
    | ProcessPendingDiagnostics
    | DocumentBacklogUpdate
    | DocumentRemoval of string

type DiagnosticsState = {
    DocumentBacklog: string list
    DocumentChanges: Map<string, DateTime>
}

let emptyDiagnosticsState = { DocumentBacklog = []; DocumentChanges = Map.empty }

let processDiagnosticsEvent logMessage
                            (lspClient: CSharpLspClient)
                            (getDocumentForUri: string -> Async<(Document * ServerDocumentType) option>)
                            (state: DiagnosticsState)
                            event =
    match event with
    | DocumentRemoval uri -> async {
        let updatedDocumentChanges = state.DocumentChanges |> Map.remove uri
        let newState = { state with DocumentChanges = updatedDocumentChanges }
        return newState, [ DocumentBacklogUpdate ]
      }

    | DocumentOpenOrChange (uri, timestamp) -> async {
        let newDocChanges = state.DocumentChanges |> Map.add uri timestamp
        let newState = { state with DocumentChanges = newDocChanges }
        return newState, [ DocumentBacklogUpdate ]
      }

    | DocumentBacklogUpdate -> async {
        let newBacklog =
            state.DocumentChanges
            |> Seq.sortByDescending (fun kv -> kv.Value)
            |> Seq.map (fun kv -> kv.Key)
            |> List.ofSeq

        let newState = { state with DocumentBacklog = newBacklog }
        return newState, []
      }

    | DocumentClose uri -> async {
        let prunedBacklog = state.DocumentBacklog
                            |> Seq.filter (fun x -> x <> uri)
                            |> List.ofSeq

        let prunedDocumentChanges = state.DocumentChanges |> Map.remove uri

        let newState = { state with DocumentBacklog = prunedBacklog
                                    DocumentChanges = prunedDocumentChanges }
        return newState, []
      }

    | ProcessPendingDiagnostics -> async {
        let docUriMaybe, newBacklog =
            match state.DocumentBacklog with
            | [] -> (None, [])
            | uri :: remainder -> (Some uri, remainder)

        match docUriMaybe with
        | Some docUri ->
            let! docAndTypeMaybe = getDocumentForUri docUri
            match docAndTypeMaybe with
            | Some (doc, _) ->
                let! ct = Async.CancellationToken
                let! semanticModelMaybe = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                match semanticModelMaybe |> Option.ofObj with
                | Some semanticModel ->
                    let diagnostics =
                        semanticModel.GetDiagnostics()
                        |> Seq.map RoslynHelpers.roslynToLspDiagnostic
                        |> Array.ofSeq

                    do! lspClient.TextDocumentPublishDiagnostics { Uri = docUri; Diagnostics = diagnostics }
                | None -> ()
            | None -> ()
        | None -> ()

        let eventsToPost = match newBacklog with
                           | [] -> []
                           | _ -> [ ProcessPendingDiagnostics ]

        return { state with DocumentBacklog = newBacklog }, eventsToPost
      }

let setupServerHandlers options lspClient =
    let mutable logMessageCurrent = Action<string>(fun _ -> ())
    let logMessageInvoke m = logMessageCurrent.Invoke(m)

    let stateActor = MailboxProcessor.Start(
        serverEventLoop logMessageInvoke { emptyServerState with Options = options })

    let getDocumentForUriFromCurrentState docType uri = async {
        let! state = stateActor.PostAndAsyncReply(GetState)
        return getDocumentOfTypeForUri state docType uri
    }

    let diagnostics = MailboxProcessor.Start(
        fun inbox -> async {
            let rec loop state = async {
                try
                    let! msg = inbox.Receive()
                    let! (newState, eventsToPost) =
                        processDiagnosticsEvent logMessageInvoke
                                                lspClient
                                                (getDocumentForUriFromCurrentState AnyDocument)
                                                state
                                                msg

                    for ev in eventsToPost do inbox.Post(ev)

                    return! loop newState
                with
                | ex -> logMessageInvoke (sprintf "unhandled exception in `diagnostics`: %s" (ex|>string))
                        raise ex
            }

            return! loop emptyDiagnosticsState
        })

    let mutable timer: System.Threading.Timer option = None

    let setupTimer () =
        timer <- Some (new System.Threading.Timer(
            System.Threading.TimerCallback(
                fun _ -> do diagnostics.Post(ProcessPendingDiagnostics)
                         do stateActor.Post(PeriodicTimerTick)),
            null, dueTime=1000, period=250))

    let logMessageWithLevel l message =
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do lspClient.WindowShowMessage messageParams |> Async.StartAsTask |> ignore

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info
    let warningMessage = logMessageWithLevel MessageType.Warning
    let errorMessage = logMessageWithLevel MessageType.Error

    let handleInitialize (scope: ServerRequestScope) (p: InitializeParams): AsyncLspResult<InitializeResult> =
      async {
        logMessageCurrent <- Action<string>(logMessage)

        infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                            (typeof<CSharpLspClient>.Assembly.GetName().Version |> string)
                            (options |> serialize |> string))

        infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

        scope.Emit(ClientCapabilityChange p.Capabilities)

        // registering w/client for didChangeWatchedFiles notifications"
        let fileChangeWatcher = { GlobPattern = "**/*.{cs,csproj,sln}"
                                  Kind = None }

        let didChangeWatchedFilesRegistration: Types.Registration =
            { Id = "id:workspace/didChangeWatchedFiles"
              Method = "workspace/didChangeWatchedFiles"
              RegisterOptions = { Watchers = [| fileChangeWatcher |] } |> serialize |> Some
            }

        let! regResult = lspClient.ClientRegisterCapability(
            { Registrations = [| didChangeWatchedFilesRegistration |] })

        match regResult with
        | Ok _ -> ()
        | Error error -> infoMessage (sprintf "  ...didChangeWatchedFiles registration has failed with %s" (error |> string))

        // setup timer so actors get period ticks
        setupTimer ()

        // start solution loading (on stateActor)
        scope.Emit(SolutionReload)

        return success {
              InitializeResult.Default with
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
                        DocumentFormattingProvider = Some true
                        DocumentRangeFormattingProvider = Some true
                        DocumentOnTypeFormattingProvider =
                               Some
                                    { FirstTriggerCharacter = ';'
                                      MoreTriggerCharacter = Some([| '}'; ')' |]) }
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
    }

    let handleTextDocumentDidOpen (scope: ServerRequestScope) (openParams: Types.DidOpenTextDocumentParams): Async<unit> =
      async {
        match scope.GetDocumentForUriOfType AnyDocument openParams.TextDocument.Uri with
        | Some (doc, docType) ->
            match docType with
            | UserDocument ->
                // we want to load the document in case it has been changed since we have the solution loaded
                // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                // went out of sync with editor
                let updatedDoc = SourceText.From(openParams.TextDocument.Text) |> doc.WithText

                scope.Emit(OpenDocVersionAdd (openParams.TextDocument.Uri, openParams.TextDocument.Version))
                scope.Emit(SolutionChange updatedDoc.Project.Solution)

                diagnostics.Post(
                    DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))
            | _ -> ()
        | None ->
            match openParams.TextDocument.Uri.StartsWith("file://") with
            | true ->
                // ok, this document is not on solution, register a new one
                let docFilePath = openParams.TextDocument.Uri.Substring("file://".Length)
                let newDocMaybe = tryAddDocument logMessage
                                                docFilePath
                                                openParams.TextDocument.Text
                                                scope.Solution
                match newDocMaybe with
                | Some newDoc ->
                    scope.Emit(SolutionChange newDoc.Project.Solution)

                    diagnostics.Post(
                        DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))

                | None -> ()
            | false -> ()
    }

    let handleTextDocumentDidChange (scope: ServerRequestScope) (changeParams: Types.DidChangeTextDocumentParams): Async<unit> =
      async {
        let docMaybe = scope.GetUserDocumentForUri changeParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                //logMessage (sprintf "TextDocumentDidChange: changeParams: %s" (string changeParams))
                //logMessage (sprintf "TextDocumentDidChange: sourceText: %s" (string sourceText))

                let updatedSourceText = sourceText |> applyLspContentChangesOnRoslynSourceText changeParams.ContentChanges
                let updatedDoc = doc.WithText(updatedSourceText)

                //logMessage (sprintf "TextDocumentDidChange: newSourceText: %s" (string updatedSourceText))

                let updatedSolution = updatedDoc.Project.Solution

                scope.Emit(SolutionChange updatedSolution)
                scope.Emit(OpenDocVersionAdd (changeParams.TextDocument.Uri, changeParams.TextDocument.Version |> Option.defaultValue 0))

                diagnostics.Post(
                    DocumentOpenOrChange (changeParams.TextDocument.Uri, DateTime.Now))

        | None -> ()
    }

    let handleTextDocumentDidSave (scope: ServerRequestScope) (saveParams: Types.DidSaveTextDocumentParams): Async<unit> =
      async {
        // we need to add this file to solution if not already
        let doc = scope.GetAnyDocumentForUri saveParams.TextDocument.Uri
        match doc with
        | Some _ -> ()

        | None ->
            let docFilePath = saveParams.TextDocument.Uri.Substring("file://".Length)
            let newDocMaybe = tryAddDocument logMessage
                                            docFilePath
                                            saveParams.Text.Value
                                            scope.Solution
            match newDocMaybe with
            | Some newDoc ->
                scope.Emit(SolutionChange newDoc.Project.Solution)

                diagnostics.Post(
                    DocumentOpenOrChange (saveParams.TextDocument.Uri, DateTime.Now))

            | None -> ()
    }

    let handleTextDocumentDidClose (scope: ServerRequestScope) (closeParams: Types.DidCloseTextDocumentParams): Async<unit> =
        scope.Emit(OpenDocVersionRemove closeParams.TextDocument.Uri)
        diagnostics.Post(DocumentClose closeParams.TextDocument.Uri)
        async.Return ()

    let handleTextDocumentCodeAction (scope: ServerRequestScope) (actionParams: Types.CodeActionParams): AsyncLspResult<Types.TextDocumentCodeActionResult option> = async {
        let docMaybe = scope.GetUserDocumentForUri actionParams.TextDocument.Uri
        match docMaybe with
        | None ->
            return None |> success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan = actionParams.Range
                                          |> roslynLinePositionSpanForLspRange
                                          |> docText.Lines.GetTextSpan

            let! roslynCodeActions = getRoslynCodeActions doc textSpan

            let clientCapabilities = scope.ClientCapabilities
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
                                scope.Solution
                                scope.OpenDocVersions.TryFind
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

    let handleCodeActionResolve (scope: ServerRequestScope) (codeAction: CodeAction): AsyncLspResult<CodeAction option> = async {
        let resolutionData =
            codeAction.Data
            |> Option.map (fun x -> x :?> string)
            |> Option.map JsonConvert.DeserializeObject<CSharpCodeActionResolutionData>

        let docMaybe = scope.GetUserDocumentForUri resolutionData.Value.TextDocumentUri
        match docMaybe with
        | None ->
            return None |> success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan =
                       resolutionData.Value.Range
                       |> roslynLinePositionSpanForLspRange
                       |> docText.Lines.GetTextSpan

            let! roslynCodeActions = getRoslynCodeActions doc textSpan

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = codeAction.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    scope.Solution
                                                    scope.OpenDocVersions.TryFind
                                                    logMessage
                                                    doc

            let! maybeLspCodeAction =
                match selectedCodeAction with
                | Some ca -> async { return! toResolvedLspCodeAction ca }
                | None -> async { return None }

            return maybeLspCodeAction |> success
    }

    let handleTextDocumentCodeLens (scope: ServerRequestScope) (lensParams: CodeLensParams): AsyncLspResult<CodeLens[] option> = async {
        let docMaybe = scope.GetAnyDocumentForUri lensParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
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

    let handleCodeLensResolve (scope: ServerRequestScope) (codeLens: CodeLens) : AsyncLspResult<CodeLens> = async {
        let lensData =
            codeLens.Data
            |> Option.map (fun t -> t.ToObject<CodeLensData>())
            |> Option.defaultValue emptyCodeLensData

        let docMaybe = scope.GetAnyDocumentForUri lensData.DocumentUri
        let doc = docMaybe.Value

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

    let handleTextDocumentDefinition (scope: ServerRequestScope) (def: Types.TextDocumentPositionParams) : AsyncLspResult<Types.GotoResult option> = async {
        let docMaybe = scope.GetAnyDocumentForUri def.TextDocument.Uri

        return!
            match docMaybe with
            | Some doc -> async {
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
                let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

                let symbols =
                    match Option.ofObj symbolMaybe with
                    | Some sym -> [sym]
                    | None -> []

                let! (locations, stateChanges) = resolveSymbolLocations scope.State doc.Project symbols

                do scope.EmitMany stateChanges

                return locations |> Array.ofSeq |> GotoResult.Multiple |> Some |> success
              }

            | None ->
                None |> success |> async.Return
    }

    let handleTextDocumentImplementation (scope: ServerRequestScope) (def: Types.TextDocumentPositionParams): AsyncLspResult<Types.GotoResult option> = async {
        let docMaybe = scope.GetAnyDocumentForUri def.TextDocument.Uri

        return!
            match docMaybe with
            | Some doc -> async {
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
                let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

                let! symbols = async {
                    match Option.ofObj symbolMaybe with
                    | Some sym ->
                        let! implSymbols =
                            SymbolFinder.FindImplementationsAsync(sym, scope.Solution)
                            |> Async.AwaitTask

                        return implSymbols |> List.ofSeq
                    | None -> return []
                }

                let! (locations, stateChanges) = resolveSymbolLocations scope.State doc.Project symbols

                do scope.EmitMany stateChanges

                return locations |> Array.ofSeq |> GotoResult.Multiple |> Some |> success
              }

            | None -> async {
                return None |> success
              }
    }

    let handleTextDocumentCompletion (scope: ServerRequestScope) (posParams: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = async {
        let docMaybe = scope.GetUserDocumentForUri posParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
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

    let handleTextDocumentDocumentHighlight (scope: ServerRequestScope) (docParams: Types.TextDocumentPositionParams): AsyncLspResult<Types.DocumentHighlight [] option> = async {
        let getSymbolLocations symbol doc solution = async {
            let docSet = ImmutableHashSet<Document>.Empty.Add(doc)
            let! ct = Async.CancellationToken
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

        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument docParams.TextDocument.Uri docParams.Position

        match maybeSymbol with
        | Some (symbol, doc, _) ->
            if shouldHighlight symbol then
                let! locations = getSymbolLocations symbol doc scope.Solution
                return locations |> Array.ofSeq |> Some |> success
            else
                return None |> success

        | None -> return None |> success
    }

    let handleTextDocumentDocumentSymbol (scope: ServerRequestScope) (p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let docMaybe = scope.GetAnyDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let showAttributes = false
            let collector = DocumentSymbolCollector(p.TextDocument.Uri, semanticModel, showAttributes)

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetSymbols() |> Some |> success

        | None ->
            return None |> success
    }

    let handleTextDocumentHover (scope: ServerRequestScope) (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = async {
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

        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument hoverPos.TextDocument.Uri hoverPos.Position

        let! contents =
            match maybeSymbol with
            | Some (sym, doc, pos) -> async {
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                return csharpMarkdownDocForSymbol sym semanticModel pos
              }
            | _ -> async { return [] }

        return Some { Contents = contents |> Array.ofSeq |> MarkedStrings
                      Range = None } |> success
    }

    let handleTextDocumentReferences (scope: ServerRequestScope) (refParams: Types.ReferenceParams): AsyncLspResult<Types.Location [] option> = async {
        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument refParams.TextDocument.Uri refParams.Position
        match maybeSymbol with
        | Some (symbol, _, _) ->
            let! ct = Async.CancellationToken
            let! refs = SymbolFinder.FindReferencesAsync(symbol, scope.Solution, ct) |> Async.AwaitTask
            return refs
                    |> Seq.collect (fun r -> r.Locations)
                    |> Seq.map (fun rl -> lspLocationForRoslynLocation rl.Location)
                    |> Array.ofSeq
                    |> Some
                    |> success

        | None -> return None |> success
    }

    let handleTextDocumentRename (scope: ServerRequestScope) (rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = async {
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
                                          scope.OpenDocVersions.TryFind
                                          logMessage
                                          doc
            return docTextEdit
        }

        let! maybeSymbol = scope.GetSymbolAtPositionOnUserDocument rename.TextDocument.Uri rename.Position

        let! docChanges =
            match maybeSymbol with
                      | Some (symbol, doc, _) -> renameSymbolInDoc symbol doc
                      | None -> async { return [] }

        return WorkspaceEdit.Create (docChanges |> Array.ofList, scope.ClientCapabilities.Value) |> Some |> success
    }

    let handleWorkspaceSymbol (scope: ServerRequestScope) (symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let! symbols = findSymbolsInSolution scope.Solution symbolParams.Query (Some 20)
        return symbols |> Array.ofSeq |> Some |> success
    }

    let handleWorkspaceDidChangeWatchedFiles (scope: ServerRequestScope) (changeParams: Types.DidChangeWatchedFilesParams): Async<unit> = async {
        let tryReloadDocumentOnUri uri = async {
            match scope.GetDocumentForUriOfType UserDocument uri with
            | Some (doc, docType) ->
                let fileText = uri.Substring("file://".Length) |> File.ReadAllText
                let updatedDoc = SourceText.From(fileText) |> doc.WithText

                scope.Emit(SolutionChange updatedDoc.Project.Solution)
                diagnostics.Post(DocumentBacklogUpdate)
            | None ->
                match uri.StartsWith("file://") with
                | true ->
                    // ok, this document is not on solution, register a new one
                    let docFilePath = uri.Substring("file://".Length)
                    let fileText = docFilePath |> File.ReadAllText
                    let newDocMaybe = tryAddDocument logMessage
                                                     docFilePath
                                                     fileText
                                                     scope.Solution
                    match newDocMaybe with
                    | Some newDoc ->
                        scope.Emit(SolutionChange newDoc.Project.Solution)
                        diagnostics.Post(DocumentBacklogUpdate)
                    | None -> ()
                | false -> ()
        }

        for change in changeParams.Changes do
            let documentIsCSharpFile = change.Uri.EndsWith(".cs")
            match Path.GetExtension(change.Uri) with
            | ".csproj" ->
                logMessage "change to .csproj detected, will reload solution"
                scope.Emit(SolutionReloadRequest)

            | ".sln" ->
                logMessage "change to .sln detected, will reload solution"
                scope.Emit(SolutionReloadRequest)

            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Changed ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Deleted ->
                    match scope.GetDocumentForUriOfType UserDocument change.Uri with
                    | Some (existingDoc, docType) ->
                        let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

                        scope.Emit(SolutionChange updatedProject.Solution)
                        scope.Emit(OpenDocVersionRemove change.Uri)

                        diagnostics.Post(DocumentRemoval change.Uri)
                    | None -> ()
                | _ -> ()

            | _ -> ()
    }

    let handleCSharpMetadata (scope: ServerRequestScope) (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = async {
        let uri = metadataParams.TextDocument.Uri

        let metadataMaybe =
            scope.DecompiledMetadata
            |> Map.tryFind uri
            |> Option.map (fun x -> x.Metadata)

        return metadataMaybe |> success
    }

    let handleTextDocumentFormatting (scope: ServerRequestScope) (format: Types.DocumentFormattingParams) : AsyncLspResult<Types.TextEdit [] option> = async {
            let maybeDocument = scope.GetUserDocumentForUri format.TextDocument.Uri
            let! formattingChanges =
                match maybeDocument with
                | Some doc -> handleTextDocumentFormatAsync doc
                | None -> Array.empty |> async.Return
            return formattingChanges |> Some |> success
        }

    let handleTextDocumentRangeFormatting (scope: ServerRequestScope) (format: DocumentRangeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
             let maybeDocument = scope.GetUserDocumentForUri format.TextDocument.Uri
             let! formattingChanges =
                 match maybeDocument with
                 | Some doc -> handleTextDocumentRangeFormatAsync doc format.Range
                 | None -> Array.empty |> async.Return
             return formattingChanges |> Some |> success
        }

    let handleTextDocumentOnTypeFormatting (scope: ServerRequestScope) (format: DocumentOnTypeFormattingParams) : AsyncLspResult<TextEdit[] option> = async {
            let maybeDocument = scope.GetUserDocumentForUri format.TextDocument.Uri
            let! formattingChanges =
                match maybeDocument with
                | Some doc -> handleTextOnTypeFormatAsync doc format.Ch format.Position
                | None -> Array.empty |> async.Return
            return formattingChanges |> Some |> success
        }

    let withReadOnlyScope asyncFn param = async {
        let! stateSnapshot = stateActor.PostAndAsyncReply(GetState)
        use scope = new ServerRequestScope(stateSnapshot, stateActor.Post, fun () -> ())
        return! asyncFn scope param
    }

    let withReadWriteScope asyncFn param =
        // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object.)
        let stateSnapshot = stateActor.PostAndReply(StartSolutionChange)

        // we want to run asyncFn within scope as scope.Dispose() will send FinishSolutionChange and will actually
        // allow subsequent write request to run
        async {
            use scope = new ServerRequestScope(stateSnapshot, stateActor.Post, fun () -> stateActor.Post(FinishSolutionChange))
            return! asyncFn scope param
        }

    let withTimeoutOfMS (timeoutMS: int) asyncFn param = async {
        let! baseCT = Async.CancellationToken
        use timeoutCts = System.Threading.CancellationTokenSource.CreateLinkedTokenSource(baseCT)
        timeoutCts.CancelAfter(timeoutMS)
        return! Async.StartAsTask(asyncFn param, cancellationToken=timeoutCts.Token)
                |> Async.AwaitTask
    }

    let withNotificationSuccess asyncFn param =
        // we want to run asyncFn immediately outside of async scope so we still
        // handle request immediately as sent in by StreamJsonRpc and handler
        // has a change to process this before next request is accepted
        let asyncFnResult = asyncFn param

        // allow subsequent write request to run
        async {
            do! asyncFnResult
            return Result.Ok()
        }

    [
        "initialize"                     , handleInitialize                    |> withReadWriteScope |> requestHandling
        "textDocument/didChange"         , handleTextDocumentDidChange         |> withReadWriteScope |> withNotificationSuccess |> requestHandling
        "textDocument/didClose"          , handleTextDocumentDidClose          |> withReadWriteScope |> withNotificationSuccess |> requestHandling
        "textDocument/didOpen"           , handleTextDocumentDidOpen           |> withReadWriteScope |> withNotificationSuccess |> requestHandling
        "textDocument/didSave"           , handleTextDocumentDidSave           |> withReadWriteScope |> withNotificationSuccess |> requestHandling
        "textDocument/codeAction"        , handleTextDocumentCodeAction        |> withReadOnlyScope |> requestHandling
        "codeAction/resolve"             , handleCodeActionResolve             |> withReadOnlyScope |> requestHandling
        "textDocument/codeLens"          , handleTextDocumentCodeLens          |> withReadOnlyScope |> requestHandling
        "codeLens/resolve"               , handleCodeLensResolve               |> withReadOnlyScope |> withTimeoutOfMS 10000 |> requestHandling
        "textDocument/completion"        , handleTextDocumentCompletion        |> withReadOnlyScope |> requestHandling
        "textDocument/definition"        , handleTextDocumentDefinition        |> withReadOnlyScope |> requestHandling
        "textDocument/documentHighlight" , handleTextDocumentDocumentHighlight |> withReadOnlyScope |> requestHandling
        "textDocument/documentSymbol"    , handleTextDocumentDocumentSymbol    |> withReadOnlyScope |> requestHandling
        "textDocument/hover"             , handleTextDocumentHover             |> withReadOnlyScope |> requestHandling
        "textDocument/implementation"    , handleTextDocumentImplementation    |> withReadOnlyScope |> requestHandling
        "textDocument/formatting"        , handleTextDocumentFormatting        |> withReadOnlyScope |> requestHandling
        "textDocument/onTypeFormatting"  , handleTextDocumentOnTypeFormatting  |> withReadOnlyScope |> requestHandling
        "textDocument/rangeFormatting"   , handleTextDocumentRangeFormatting   |> withReadOnlyScope |> requestHandling
        "textDocument/references"        , handleTextDocumentReferences        |> withReadOnlyScope |> requestHandling
        "textDocument/rename"            , handleTextDocumentRename            |> withReadOnlyScope |> requestHandling
        "workspace/symbol"               , handleWorkspaceSymbol               |> withReadOnlyScope |> requestHandling
        "workspace/didChangeWatchedFiles", handleWorkspaceDidChangeWatchedFiles |> withReadWriteScope |> withNotificationSuccess |> requestHandling
        "csharp/metadata"                , handleCSharpMetadata                |> withReadOnlyScope |> requestHandling
    ]
    |> Map.ofList

let startCore options =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    Ionide.LanguageServerProtocol.Server.startWithSetup
        (setupServerHandlers options)
        input
        output
        CSharpLspClient

let start options =
    try
        let result = startCore options
        int result
    with
    | _ex ->
        // logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
