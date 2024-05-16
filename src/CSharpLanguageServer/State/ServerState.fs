module CSharpLanguageServer.State.ServerState

open System
open System.IO
open System.Threading

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol

open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions

type DecompiledMetadataDocument = {
    Metadata: CSharpMetadataInformation
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
    RootPath: string
    LspClient: ILspClient option
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

let emptyServerState = { Settings = ServerSettings.Default
                         RootPath = Directory.GetCurrentDirectory()
                         LspClient = None
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
    | RootPathChange of string
    | ClientChange of ILspClient option
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

let processServerEvent logger state postMsg msg: Async<ServerState> = async {
    let showMessage m =
        match state.LspClient with
        | Some lspClient -> lspClient.WindowShowMessage(
            { Type = MessageType.Info
              Message = sprintf "csharp-ls: %s" m })
        | None -> async.Return ()

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
        let request = state.RunningRequests |> Map.tryFind requestId
        match request with
        | Some(request) ->
            request.Semaphore.Dispose()
            let newRunningRequests = state.RunningRequests |> Map.remove requestId
            let newState = { state with RunningRequests = newRunningRequests }

            postMsg ProcessRequestQueue
            return newState
        | None -> return state

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

    | RootPathChange rootPath ->
        return { state with RootPath = rootPath }

    | ClientChange lspClient ->
        return { state with LspClient = lspClient }

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
            let! newSolution =
                loadSolutionOnSolutionPathOrDir
                    state.LspClient.Value
                    logger
                    showMessage
                    state.Settings.SolutionPath
                    state.RootPath

            return { state with Solution = newSolution
                                SolutionReloadPending = None }

        | false ->
            return state
}

let serverEventLoop initialState (inbox: MailboxProcessor<ServerStateEvent>) =
    let logger = LogProvider.getLoggerByName "serverEventLoop"

    let rec loop state = async {
        let! msg = inbox.Receive()

        try
            let! newState = msg |> processServerEvent logger state inbox.Post
            return! loop newState
        with ex ->
            logger.debug (
                Log.setMessage "serverEventLoop: crashed with {exception}"
                >> Log.addContext "exception" (string ex)
            )
            raise ex
    }

    loop initialState


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
                            |> Seq.map Diagnostic.fromRoslynDiagnostic
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

let diagnosticsEventLoop
        (lspClient: CSharpLspClient)
        getDocumentForUriFromCurrentState
        (inbox: MailboxProcessor<DiagnosticsEvent>) =
    let logger = LogProvider.getLoggerByName "DiagnosticsEventLoop"

    let rec loop state = async {
        try
            let! msg = inbox.Receive()
            let! (newState, eventsToPost) =
                processDiagnosticsEvent
                    // TODO: can we provide value for PublishDiagnosticsParams.Version?
                    (fun docUri diagnostics -> lspClient.TextDocumentPublishDiagnostics { Uri = docUri;
                                                                                            Version = None;
                                                                                            Diagnostics = diagnostics;
                                                                                        })
                    (getDocumentForUriFromCurrentState AnyDocument)
                    state
                    inbox.CurrentQueueLength
                    msg

            for ev in eventsToPost do inbox.Post(ev)

            return! loop newState
        with
        | ex ->
            logger.warn (
                Log.setMessage "unhandled exception in `diagnostics`: {message}"
                >> Log.addContext "message" (string ex))
            raise ex
    }

    loop emptyDiagnosticsState

type ServerSettingsDto = {
     csharp: ServerSettingsCSharpDto option
}
and ServerSettingsCSharpDto =
  { solution: string option }
  static member Default = { solution = None }
