module CSharpLanguageServer.State

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open RoslynHelpers

type Options = {
    SolutionPath: string option
    LogLevel: MessageType
}

let emptyOptions = { SolutionPath = None; LogLevel = MessageType.Log }

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
    Type: ServerRequestType
    ReplyChannel: AsyncReplyChannel<ServerState>
}
and ServerState = {
    ClientCapabilities: ClientCapabilities option
    Solution: Solution option
    OpenDocVersions: Map<string, int>
    DecompiledMetadata: Map<string, DecompiledMetadataDocument>
    Options: Options
    CurrentReadWriteRequest: ServerRequest option
    RequestQueue: ServerRequest list
    SolutionReloadPending: DateTime option
}

let emptyServerState = { ClientCapabilities = None
                         Solution = None
                         OpenDocVersions = Map.empty
                         DecompiledMetadata = Map.empty
                         Options = emptyOptions
                         CurrentReadWriteRequest = None
                         RequestQueue = []
                         SolutionReloadPending = None }

type ServerDocumentType =
     | UserDocument // user Document from solution, on disk
     | DecompiledDocument // Document decompiled from metadata, readonly
     | AnyDocument

type ServerStateEvent =
    | ClientCapabilityChange of ClientCapabilities option
    | SolutionChange of Solution
    | DecompiledMetadataAdd of string * DecompiledMetadataDocument
    | OpenDocVersionAdd of string * int
    | OpenDocVersionRemove of string
    | GetState of AsyncReplyChannel<ServerState>
    | GetDocumentOfTypeForUri of ServerDocumentType * string * AsyncReplyChannel<Document option>
    | StartReadOnlyScope of AsyncReplyChannel<ServerState>
    | StartReadWriteScope of AsyncReplyChannel<ServerState>
    | FinishReadWriteScope
    | ProcessRequestQueue
    | SolutionReloadRequest
    | SolutionReload
    | PeriodicTimerTick

let processServerEvent logMessage state postMsg msg: Async<ServerState> = async {
    let enqueueScope requestType replyChannel =
        let newRequestQueue = state.RequestQueue @ [{ Type=requestType; ReplyChannel=replyChannel }]
        let newState = { state with RequestQueue = newRequestQueue }
        postMsg ProcessRequestQueue
        newState

    match msg with
    | GetState replyChannel ->
        replyChannel.Reply(state)
        return state

    | GetDocumentOfTypeForUri (docType, uri, replyChannel) ->
        let documentAndTypeMaybe =
            match state.Solution with
            | Some solution ->
                let matchingUserDocuments =
                    solution.Projects
                    |> Seq.collect (fun p -> p.Documents)
                    |> Seq.filter (fun d -> Uri("file://" + d.FilePath) = Uri uri) |> List.ofSeq

                let matchingUserDocumentMaybe =
                    match matchingUserDocuments with
                    | [d] -> Some (d, UserDocument)
                    | _ -> None

                let matchingDecompiledDocumentMaybe =
                    Map.tryFind uri state.DecompiledMetadata
                    |> Option.map (fun x -> (x.Document, DecompiledDocument))

                match docType with
                | UserDocument -> matchingUserDocumentMaybe
                | DecompiledDocument -> matchingDecompiledDocumentMaybe
                | AnyDocument -> matchingUserDocumentMaybe |> Option.orElse matchingDecompiledDocumentMaybe

            | None -> None

        replyChannel.Reply(documentAndTypeMaybe |> Option.map fst)

        return state

    | StartReadOnlyScope replyChannel ->
        return enqueueScope ReadOnly replyChannel

    | StartReadWriteScope replyChannel ->
        return enqueueScope ReadWrite replyChannel

    | FinishReadWriteScope ->
        let newState = { state with CurrentReadWriteRequest = None }
        postMsg ProcessRequestQueue
        return newState

    | ProcessRequestQueue ->
        return
            match state.CurrentReadWriteRequest with
            | Some _req ->
                state // block until CurrentReadWriteRequest is finished
            | None ->
                match state.RequestQueue with
                | [] -> state
                | nextRequest :: queueRemainder ->
                    // try to process next msg from the remainder, if possible, later
                    postMsg ProcessRequestQueue

                    let newState = { state with RequestQueue = queueRemainder }

                    // unblock this scope/request to run by sending it current state
                    nextRequest.ReplyChannel.Reply(newState)

                    match nextRequest.Type with
                    | ReadWrite -> { newState with CurrentReadWriteRequest = Some nextRequest }
                    | ReadOnly -> newState

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
            postMsg SolutionReload
            return { state with SolutionReloadPending = None }

        | false ->
            return state
}

let serverEventLoop logMessage initialState (inbox: MailboxProcessor<ServerStateEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()
        let! newState = msg |> processServerEvent logMessage state inbox.Post
        return! loop newState
    }

    loop initialState

type ServerRequestScope (state: ServerState, emitServerEvent, onDispose: unit -> unit, logMessage: string -> unit) =
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
            let collector = DocumentSymbolCollectorForMatchingSymbolName(uri, sym, logMessage)
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
}

let emptyDiagnosticsState = { DocumentBacklog = []; DocumentChanges = Map.empty }

let processDiagnosticsEvent _logMessage
                            (publishDiagnostics: string -> Diagnostic[] -> Async<unit>)
                            (getDocumentForUri: string -> Async<Document option>)
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

        let eventsToPost = match newBacklog with
                           | [] -> []
                           | _ -> [ ProcessPendingDiagnostics ]

        return { state with DocumentBacklog = newBacklog }, eventsToPost
      }
