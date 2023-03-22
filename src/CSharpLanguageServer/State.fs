module CSharpLanguageServer.State

open System
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open RoslynHelpers
open Util

type ServerSettings = {
    SolutionPath: string option
    LogLevel: MessageType
}

let emptySettings = { SolutionPath = None; LogLevel = MessageType.Log }

type CSharpMetadata = {
    ProjectName: string
    AssemblyName: string
    SymbolName: string
    Source: string
}

type DecompiledMetadataDocument = {
    Metadata: CSharpMetadata
    Document: Document
}

type ServerRequestType = ReadOnly | ReadWrite

type ServerRequest = {
    Id: int
    Name: string
    Type: ServerRequestType
    Semaphore: SemaphoreSlim
    Priority: int // 0 is the highest priority, 1 is lower prio, etc.
                  // priority is used to order pending R/O requests and is ignored wrt R/W requests
    Enqueued: DateTime
}
and ServerState = {
    Settings: ServerSettings
    ClientCapabilities: ClientCapabilities option
    Solution: Solution option
    OpenDocVersions: Map<string, int>
    DecompiledMetadata: Map<string, DecompiledMetadataDocument>
    LastRequestId: int
    RunningRequests: Map<int, ServerRequest>
    RequestQueue: ServerRequest list
    SolutionReloadPending: DateTime option
}

let pullFirstRequestMaybe requestQueue =
    match requestQueue with
    | [] -> (None, [])
    | (firstRequest :: queueRemainder) -> (Some firstRequest, queueRemainder)

let pullNextRequestMaybe requestQueue =
    match requestQueue with
    | [] -> (None, requestQueue)

    | nonEmptyRequestQueue ->
        let requestIsReadOnly r = r.Type = ServerRequestType.ReadOnly

        // here we will try to take non-interrupted r/o request sequence at the front,
        // order it by priority and run the most prioritized one first
        let nextRoRequestByPriorityMaybe =
            nonEmptyRequestQueue
            |> Seq.takeWhile requestIsReadOnly
            |> Seq.sortBy (fun r -> r.Priority)
            |> Seq.tryHead

        // otherwise, if no r/o request by priority was found then we should just take the first request
        let nextRequest =
            nextRoRequestByPriorityMaybe
            |> Option.defaultValue (nonEmptyRequestQueue |> Seq.head)

        let queueRemainder =
            nonEmptyRequestQueue |> List.except [nextRequest]

        (Some nextRequest, queueRemainder)

let emptyServerState = { Settings = emptySettings
                         ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty
                         LastRequestId = 0
                         RunningRequests = Map.empty
                         RequestQueue = []
                         SolutionReloadPending = None }

type ServerDocumentType =
     | UserDocument // user Document from solution, on disk
     | DecompiledDocument // Document decompiled from metadata, readonly
     | AnyDocument

type ServerStateEvent =
    | SettingsChange of ServerSettings
    | ClientCapabilityChange of ClientCapabilities option
    | SolutionChange of Solution
    | DecompiledMetadataAdd of string * DecompiledMetadataDocument
    | OpenDocVersionAdd of string * int
    | OpenDocVersionRemove of string
    | GetState of AsyncReplyChannel<ServerState>
    | GetDocumentOfTypeForUri of ServerDocumentType * string * AsyncReplyChannel<Document option>
    | StartRequest of string * ServerRequestType * int * AsyncReplyChannel<int * SemaphoreSlim>
    | FinishRequest of int
    | ProcessRequestQueue
    | SolutionReloadRequest of TimeSpan
    | PeriodicTimerTick

let getDocumentForUriOfType state docType (u: string) =
    let uri = Uri(u.Replace("%3A", ":", true, null))

    match state.Solution with
    | Some solution ->
        let matchingUserDocuments =
            solution.Projects
            |> Seq.collect (fun p -> p.Documents)
            |> Seq.filter (fun d -> Uri(d.FilePath, UriKind.Absolute) = uri) |> List.ofSeq

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

let processServerEvent (logMessage: AsyncLogFn) state postMsg msg: Async<ServerState> = async {
    match msg with
    | SettingsChange newSettings ->
        let newState: ServerState = { state with Settings = newSettings }

        let solutionChanged = not (state.Settings.SolutionPath = newState.Settings.SolutionPath)

        if solutionChanged then
            postMsg (SolutionReloadRequest (TimeSpan.FromMilliseconds(250)))

        return newState

    | GetState replyChannel ->
        replyChannel.Reply(state)
        return state

    | GetDocumentOfTypeForUri (docType, uri, replyChannel) ->
        let documentAndTypeMaybe = getDocumentForUriOfType state docType uri
        replyChannel.Reply(documentAndTypeMaybe |> Option.map fst)

        return state

    | StartRequest (name, requestType, requestPriority, replyChannel) ->
        postMsg ProcessRequestQueue

        let newRequest = { Id=state.LastRequestId+1
                           Name=name
                           Type=requestType
                           Semaphore=new SemaphoreSlim(0, 1)
                           Priority=requestPriority
                           Enqueued=DateTime.Now }

        replyChannel.Reply((newRequest.Id, newRequest.Semaphore))

        return { state with LastRequestId=newRequest.Id
                            RequestQueue=state.RequestQueue @ [newRequest] }

    | FinishRequest requestId ->
        let request = state.RunningRequests |> Map.find requestId
        request.Semaphore.Dispose()

        let newRunningRequests = state.RunningRequests |> Map.remove requestId
        let newState = { state with RunningRequests = newRunningRequests }

        postMsg ProcessRequestQueue
        return newState

    | ProcessRequestQueue ->
        let runningRWRequestMaybe =
            state.RunningRequests
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.tryFind (fun r -> r.Type = ReadWrite)

        // let numRunningRequests = state.RunningRequests |> Map.count

        let canRunNextRequest =
            (Option.isNone runningRWRequestMaybe) // && (numRunningRequests < 4)

        return
            if not canRunNextRequest then
                state // block until current ReadWrite request is finished
            else
                let (nextRequestMaybe, queueRemainder) = pullNextRequestMaybe state.RequestQueue

                match nextRequestMaybe with
                | None -> state
                | Some nextRequest ->
                    // try to process next msg from the remainder, if possible, later
                    postMsg ProcessRequestQueue

                    let newState = { state with RequestQueue = queueRemainder
                                                RunningRequests = state.RunningRequests |> Map.add nextRequest.Id nextRequest }

                    // unblock this request to run by sending it current state
                    nextRequest.Semaphore.Release() |> ignore

                    newState

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

    | SolutionReloadRequest reloadNoLaterThanIn ->
        // we need to wait a bit before starting this so we
        // can buffer many incoming requests at once
        let newSolutionReloadDeadline =
            let suggestedDeadline = DateTime.Now + reloadNoLaterThanIn

            match state.SolutionReloadPending with
            | Some currentDeadline ->
                if (suggestedDeadline < currentDeadline) then suggestedDeadline else currentDeadline
            | None -> suggestedDeadline

        return { state with SolutionReloadPending = newSolutionReloadDeadline |> Some }

    | PeriodicTimerTick ->
        let solutionReloadTime = state.SolutionReloadPending
                                 |> Option.defaultValue (DateTime.Now.AddDays(1))

        match solutionReloadTime < DateTime.Now with
        | true ->
            let! newSolution = loadSolutionOnSolutionPathOrCwd logMessage state.Settings.SolutionPath

            return { state with Solution = newSolution
                                SolutionReloadPending = None }

        | false ->
            return state
}

let serverEventLoop (logMessage: AsyncLogFn) initialState (inbox: MailboxProcessor<ServerStateEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent logMessage state inbox.Post
            return! loop newState
        with ex ->
            do! logMessage (sprintf "serverEventLoop: crashed with %s" (string ex))
            raise ex
    }

    loop initialState

type ServerRequestScope (requestId: int, state: ServerState, emitServerEvent, logMessage: AsyncLogFn) =
    let mutable solutionMaybe = state.Solution

    member _.RequestId = requestId
    member _.State = state
    member _.ClientCapabilities = state.ClientCapabilities
    member _.Solution = solutionMaybe.Value
    member _.OpenDocVersions = state.OpenDocVersions
    member _.DecompiledMetadata = state.DecompiledMetadata

    member this.GetDocumentForUriOfType = getDocumentForUriOfType this.State

    member scope.GetUserDocumentForUri (u: string) =
        scope.GetDocumentForUriOfType UserDocument u |> Option.map fst

    member scope.GetAnyDocumentForUri (u: string) =
        scope.GetDocumentForUriOfType AnyDocument u |> Option.map fst

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

    member scope.GetSymbolAtPositionOnAnyDocument uri pos =
        scope.GetSymbolAtPositionOfType AnyDocument uri pos

    member scope.GetSymbolAtPositionOnUserDocument uri pos =
        scope.GetSymbolAtPositionOfType UserDocument uri pos

    member _.Emit ev =
        match ev with
        | SolutionChange newSolution ->
            solutionMaybe <- Some newSolution
        | _ -> ()

        emitServerEvent ev

    member scope.EmitMany es =
        for e in es do scope.Emit e

    member scope.ResolveSymbolLocation
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

            scope.EmitMany stateChanges

            // figure out location on the document (approx implementation)
            let! syntaxTree = mdDocument.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym)
            collector.Visit(syntaxTree.GetRoot())

            let fallbackLocationInMetadata = {
                Uri = uri
                Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 1 } } }

            return
                match collector.GetLocations() with
                | [] -> [fallbackLocationInMetadata]
                | ls -> ls

        else if l.IsInSource then
            return [lspLocationForRoslynLocation l]
        else
            return []
    }

    member scope.ResolveSymbolLocations
            (project: Microsoft.CodeAnalysis.Project)
            (symbols: Microsoft.CodeAnalysis.ISymbol list) = async {
        let! ct = Async.CancellationToken
        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

        let mutable aggregatedLspLocations = []

        for sym in symbols do
            for l in sym.Locations do
                let! symLspLocations = scope.ResolveSymbolLocation compilation project sym l

                aggregatedLspLocations <- aggregatedLspLocations @ symLspLocations

        return aggregatedLspLocations
    }

type DiagnosticsEvent =
    | DocumentOpenOrChange of string * DateTime
    | DocumentClose of string
    | ProcessPendingDiagnostics
    | DocumentBacklogUpdate
    | DocumentRemoval of string

type DiagnosticsState = {
    DocumentBacklog: string list
    DocumentChanges: Map<string, DateTime>
    LastPendingDiagnosticsProcessing: DateTime
}

let emptyDiagnosticsState = {
    DocumentBacklog = []
    DocumentChanges = Map.empty
    LastPendingDiagnosticsProcessing = DateTime.MinValue
}

let processDiagnosticsEvent
        (_logMessage: AsyncLogFn)
        (publishDiagnostics: string -> Diagnostic[] -> Async<unit>)
        (getDocumentForUri: string -> Async<Document option>)
        (state: DiagnosticsState)
        (currentEventQueueLength: int)
        msg =
    match msg with
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
        // here we build new backlog for background diagnostics processing
        // which will consider documents by their last modification date
        // for processing first
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

    | ProcessPendingDiagnostics ->
        let doProcessPendingDiagnostics = async {
            let docUriMaybe, newBacklog =
                match state.DocumentBacklog with
                | [] -> (None, [])
                | uri :: remainder -> (Some uri, remainder)

            match docUriMaybe with
            | Some docUri ->
                let! docAndTypeMaybe = getDocumentForUri docUri

                match docAndTypeMaybe with
                | Some doc ->
                    let! ct = Async.CancellationToken
                    let! semanticModelMaybe = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                    match semanticModelMaybe |> Option.ofObj with
                    | Some semanticModel ->
                        let diagnostics =
                            semanticModel.GetDiagnostics()
                            |> Seq.map RoslynHelpers.roslynToLspDiagnostic
                            |> Array.ofSeq

                        do! publishDiagnostics docUri diagnostics
                    | None -> ()
                | None -> ()
            | None -> ()

            let newState = { state with DocumentBacklog = newBacklog
                                        LastPendingDiagnosticsProcessing = DateTime.Now }

            let eventsToPost = match newBacklog with
                               | [] -> []
                               | _ -> [ ProcessPendingDiagnostics ]

            return newState, eventsToPost
        }

        let timeSinceLastProcessing =
            (DateTime.Now - state.LastPendingDiagnosticsProcessing)

        if timeSinceLastProcessing > TimeSpan.FromMilliseconds(250) || currentEventQueueLength = 0 then
            doProcessPendingDiagnostics
        else
            async { return state, [] }
