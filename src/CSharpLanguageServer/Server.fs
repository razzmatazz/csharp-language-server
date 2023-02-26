module CSharpLanguageServer.Server

open System
open System.Text
open System.IO
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Classification
open Microsoft.Build.Locator
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open RoslynHelpers
open State
open System.Threading.Tasks
open System.Threading
open Util

// TPL Task's wrap exceptions in AggregateException, -- this fn unpacks them
let rec unpackException (exn : Exception) =
    match exn with
    | :? AggregateException as agg ->
        match Seq.tryExactlyOne agg.InnerExceptions with
        | Some x -> unpackException x
        | None -> exn
    | _ -> exn

let runTaskWithNoneOnTimeout (timeoutMS:int) (baseCT:CancellationToken) taskFn = task {
    use timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(baseCT)
    timeoutCts.CancelAfter(timeoutMS)

    try
        let! taskFnResult = taskFn timeoutCts.Token
        return Some taskFnResult
    with
    | ex ->
        let unpackedEx = ex |> unpackException
        if (unpackedEx :? TaskCanceledException) || (unpackedEx :? OperationCanceledException) then
            return None
        else
            return raise ex
}

type CSharpMetadataParams = {
    TextDocument: TextDocumentIdentifier
}

type CSharpMetadataResponse = CSharpMetadata

type CSharpCodeActionResolutionData = {
    TextDocumentUri: string
    Range: Range
}

type CodeLensData = { DocumentUri: string; Position: Position  }

let emptyCodeLensData = { DocumentUri=""; Position={ Line=0; Character=0 } }

type InvocationContext = {
    Receiver: SyntaxNode
    ArgumentTypes: TypeInfo list
    Separators: SyntaxToken list
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

let getDotnetCliVersion () : string =
    use proc = new Process()
    proc.StartInfo <- ProcessStartInfo()
    proc.StartInfo.FileName <- "dotnet"
    proc.StartInfo.Arguments <- "--version"
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.CreateNoWindow <- true

    let startOK = proc.Start()
    if startOK then
        let sbuilder = StringBuilder()
        while (not (proc.StandardOutput.EndOfStream)) do
            sbuilder.Append(proc.StandardOutput.ReadLine()) |> ignore

        sbuilder.ToString()
    else
        "(could not launch `dotnet --version`)"

let setupServerHandlers options (lspClient: LspClient) =
    let success = LspResult.success
    let mutable logMessageCurrent: AsyncLogFn = fun _ -> async { return() }
    let logMessageInvoke m = logMessageCurrent(m)

    let stateActor = MailboxProcessor.Start(
        serverEventLoop
            (fun m -> logMessageInvoke m)
            { emptyServerState with Options = options })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let diagnostics = MailboxProcessor.Start(
        fun inbox -> async {
            let rec loop state = async {
                try
                    let! msg = inbox.Receive()
                    let! (newState, eventsToPost) =
                        processDiagnosticsEvent
                            logMessageInvoke
                            (fun docUri diagnostics -> lspClient.TextDocumentPublishDiagnostics { Uri = docUri; Diagnostics = diagnostics })
                            (getDocumentForUriFromCurrentState AnyDocument)
                            state
                            inbox.CurrentQueueLength
                            msg

                    for ev in eventsToPost do inbox.Post(ev)

                    return! loop newState
                with
                | ex ->
                    do! logMessageInvoke (sprintf "unhandled exception in `diagnostics`: %s" (ex|>string))
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

    let logMessageWithLevel l message = async {
        let messageParams = { Type = l ; Message = "csharp-ls: " + message }
        do! lspClient.WindowShowMessage messageParams
    }

    let logMessage = logMessageWithLevel MessageType.Log
    let infoMessage = logMessageWithLevel MessageType.Info

    let handleInitialize (scope: ServerRequestScope) (p: InitializeParams): AsyncLspResult<InitializeResult> = async {
      do! infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                               (typeof<CSharpLspClient>.Assembly.GetName().Version |> string)
                               (JsonConvert.SerializeObject(options)))

      do! infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

      do! infoMessage (sprintf "`dotnet --version`: %s"
                               (getDotnetCliVersion ()))

      let vsInstanceQueryOpt = VisualStudioInstanceQueryOptions.Default
      let vsInstanceList = MSBuildLocator.QueryVisualStudioInstances(vsInstanceQueryOpt)
      if Seq.isEmpty vsInstanceList then
         raise (InvalidOperationException("No instances of MSBuild could be detected." + Environment.NewLine + "Try calling RegisterInstance or RegisterMSBuildPath to manually register one."))

      do! infoMessage "MSBuildLocator instances found:"

      for vsInstance in vsInstanceList do
          do! infoMessage (sprintf "- SDK=\"%s\", Version=%s, MSBuildPath=\"%s\", DiscoveryType=%s"
                                   vsInstance.Name
                                   (string vsInstance.Version)
                                   vsInstance.MSBuildPath
                                   (string vsInstance.DiscoveryType))

      let vsInstance = vsInstanceList |> Seq.head

      do! infoMessage (sprintf "MSBuildLocator: will register \"%s\", Version=%s as default instance"
                               vsInstance.Name
                               (string vsInstance.Version))

      MSBuildLocator.RegisterInstance(vsInstance)

      scope.Emit(ClientCapabilityChange p.Capabilities)

      // setup timer so actors get period ticks
      setupTimer ()

      let clientSupportsRenameOptions =
          p.Capabilities
          |> Option.bind (fun x -> x.TextDocument)
          |> Option.bind (fun x -> x.Rename)
          |> Option.bind (fun x -> x.PrepareSupport)
          |> Option.defaultValue false

      let initializeResult = {
              InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider =
                               if clientSupportsRenameOptions then
                                   Second { PrepareProvider = Some true } |> Some
                               else
                                   true |> First |> Some
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
                        SignatureHelpProvider =
                            Some { TriggerCharacters = Some([| '('; ','; '<'; '{'; '[' |])
                                   RetriggerCharacters = None }
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
                                     OpenClose = Some true
                                     Save = Some { IncludeText = Some true }
                                     Change = Some TextDocumentSyncKind.Incremental
                                 }
                        FoldingRangeProvider = None
                        SelectionRangeProvider = None
                        SemanticTokensProvider =
                            Some { Legend = { TokenTypes = SemanticTokenTypes |> Seq.toArray
                                              TokenModifiers = SemanticTokenModifiers |> Seq.toArray }
                                   Range = Some true
                                   Full = true |> First |> Some
                                 }
                    }
              }

      // load solution (on stateActor)
      do! stateActor.PostAndAsyncReply(SolutionReloadInSync)

      return initializeResult |> success
    }

    let handleInitialized (_scope: ServerRequestScope) (_p: InitializedParams): Async<LspResult<unit>> =
        async {
            do! logMessage "\"initialized\" notification received from client"

            //
            // registering w/client for didChangeWatchedFiles notifications"
            //
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
            | Error error ->
                do! infoMessage (sprintf "  ...didChangeWatchedFiles registration has failed with %s" (error |> string))

            return LspResult.Ok()
        }

    let handleTextDocumentDidOpen (scope: ServerRequestScope) (openParams: Types.DidOpenTextDocumentParams): Async<LspResult<unit>> =
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

                LspResult.Ok() |> async.Return

            | _ ->
                LspResult.Ok() |> async.Return

        | None ->
            let docFilePathMaybe = Util.tryParseFileUri openParams.TextDocument.Uri

            match docFilePathMaybe with
            | Some docFilePath -> async {
                // ok, this document is not on solution, register a new document
                let! newDocMaybe =
                    tryAddDocument
                        logMessage
                        docFilePath
                        openParams.TextDocument.Text
                        scope.Solution

                match newDocMaybe with
                | Some newDoc ->
                    scope.Emit(SolutionChange newDoc.Project.Solution)

                    diagnostics.Post(
                        DocumentOpenOrChange (openParams.TextDocument.Uri, DateTime.Now))

                | None -> ()

                return LspResult.Ok()
              }

            | None ->
                LspResult.Ok() |> async.Return

    let handleTextDocumentDidChange (scope: ServerRequestScope) (changeParams: Types.DidChangeTextDocumentParams): Async<LspResult<unit>> =
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

        return Result.Ok()
    }

    let handleTextDocumentDidSave (scope: ServerRequestScope) (saveParams: Types.DidSaveTextDocumentParams): Async<LspResult<unit>> =
        // we need to add this file to solution if not already
        let doc = scope.GetAnyDocumentForUri saveParams.TextDocument.Uri

        match doc with
        | Some _ ->
            LspResult.Ok() |> async.Return

        | None -> async {
            let docFilePath = Util.parseFileUri saveParams.TextDocument.Uri
            let! newDocMaybe =
                tryAddDocument
                    logMessage
                    docFilePath
                    saveParams.Text.Value
                    scope.Solution

            match newDocMaybe with
            | Some newDoc ->
                scope.Emit(SolutionChange newDoc.Project.Solution)

                diagnostics.Post(
                    DocumentOpenOrChange (saveParams.TextDocument.Uri, DateTime.Now))

            | None -> ()

            return LspResult.Ok()
          }

    let handleTextDocumentDidClose (scope: ServerRequestScope) (closeParams: Types.DidCloseTextDocumentParams): Async<LspResult<unit>> =
        scope.Emit(OpenDocVersionRemove closeParams.TextDocument.Uri)
        diagnostics.Post(DocumentClose closeParams.TextDocument.Uri)
        LspResult.Ok() |> async.Return

    let handleTextDocumentCodeAction (scope: ServerRequestScope) (actionParams: Types.CodeActionParams): AsyncLspResult<Types.TextDocumentCodeActionResult option> = async {
        let docMaybe = scope.GetUserDocumentForUri actionParams.TextDocument.Uri
        match docMaybe with
        | None ->
            return None |> success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan =
                actionParams.Range
                |> roslynLinePositionSpanForLspRange
                |> docText.Lines.GetTextSpan

            let! roslynCodeActions =
                getRoslynCodeActions logMessage doc textSpan

            let clientSupportsCodeActionEditResolveWithEditAndData =
                scope.ClientCapabilities
                |> Option.bind (fun x -> x.TextDocument)
                |> Option.bind (fun x -> x.CodeAction)
                |> Option.bind (fun x -> x.ResolveSupport)
                |> Option.map (fun resolveSupport -> resolveSupport.Properties |> Array.contains "edit")
                |> Option.defaultValue false

            let! lspCodeActions =
                match clientSupportsCodeActionEditResolveWithEditAndData with
                | true -> async {
                    let toUnresolvedLspCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
                        let resolutionData: CSharpCodeActionResolutionData =
                            { TextDocumentUri = actionParams.TextDocument.Uri
                              Range = actionParams.Range }

                        let lspCa = roslynCodeActionToUnresolvedLspCodeAction ca
                        { lspCa with Data = resolutionData |> serialize |> Some }

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
               |> Seq.map U2<Command, CodeAction>.Second
               |> Array.ofSeq
               |> Some
               |> success
    }

    let handleCodeActionResolve (scope: ServerRequestScope) (codeAction: CodeAction): AsyncLspResult<CodeAction option> = async {
        let resolutionData =
            codeAction.Data
            |> Option.map deserialize<CSharpCodeActionResolutionData>

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

            let! roslynCodeActions =
                getRoslynCodeActions logMessage doc textSpan

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = codeAction.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    scope.Solution
                                                    scope.OpenDocVersions.TryFind
                                                    logMessage
                                                    doc

            let! maybeLspCodeAction =
                match selectedCodeAction with
                | Some ca -> async {
                    let! resolvedCA = toResolvedLspCodeAction ca

                    if resolvedCA.IsNone then
                       do! logMessage (sprintf "handleCodeActionResolve: could not resolve %s - null" (string ca))

                    return resolvedCA
                  }
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

    let handleCodeLensResolve
            (scope: ServerRequestScope)
            (codeLens: CodeLens)
            : AsyncLspResult<CodeLens> = async {

        let! ct = Async.CancellationToken
        let lensData =
            codeLens.Data
            |> Option.map (fun t -> t.ToObject<CodeLensData>())
            |> Option.defaultValue emptyCodeLensData

        let docMaybe = scope.GetAnyDocumentForUri lensData.DocumentUri
        let doc = docMaybe.Value

        let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

        let position =
            LinePosition(lensData.Position.Line, lensData.Position.Character)
            |> sourceText.Lines.GetPosition

        let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

        let resolveLocations (ct: CancellationToken) =
            task {
                let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, ct)
                return refs |> Seq.collect (fun r -> r.Locations)
            }

        let! locationsMaybe =
            runTaskWithNoneOnTimeout 3000 ct resolveLocations
            |> Async.AwaitTask

        let title =
            match locationsMaybe with
            | Some locations -> locations |> Seq.length |> sprintf "%d Reference(s)"
            | None -> "" // timeout

        let command =
            { Title = title
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

                let! locations =
                     scope.ResolveSymbolLocations doc.Project symbols

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

                let! locations =
                    scope.ResolveSymbolLocations doc.Project symbols

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

            let! maybeCompletionResults =
                completionService.GetCompletionsAsync(doc, posInText) |> Async.AwaitTask

            match Option.ofObj maybeCompletionResults with
            | Some completionResults ->
                let makeLspCompletionItem (item: Microsoft.CodeAnalysis.Completion.CompletionItem) =
                    let baseCompletionItem = CompletionItem.Create(item.DisplayText)

                    { baseCompletionItem with
                        Kind             = item.Tags |> Seq.head |> roslynTagToLspCompletion |> Some
                        SortText         = item.SortText |> Option.ofObj
                        FilterText       = item.FilterText |> Option.ofObj
                        InsertTextFormat = Some Types.InsertTextFormat.PlainText
                    }

                let completionItems =
                    completionResults.ItemsList
                    |> Seq.map makeLspCompletionItem
                    |> Array.ofSeq

                let completionList = {
                    IsIncomplete = false
                    Items        = completionItems
                }

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

    let handleTextDocumentDocumentSymbol (scope: ServerRequestScope) (p: Types.DocumentSymbolParams): AsyncLspResult<Types.DocumentSymbol [] option> = async {
        let canEmitDocSymbolHierarchy =
            scope.ClientCapabilities
            |> Option.bind (fun cc -> cc.TextDocument)
            |> Option.bind (fun cc -> cc.DocumentSymbol)
            |> Option.bind (fun cc -> cc.HierarchicalDocumentSymbolSupport)
            |> Option.defaultValue false

        let docMaybe = scope.GetAnyDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollector(docText, semanticModel)
            collector.Init(doc.Name)
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetDocumentSymbols(canEmitDocSymbolHierarchy)
                   |> Some
                   |> success

        | None ->
            return None |> success
    }

    let handleTextDocumentHover (scope: ServerRequestScope) (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = async {
        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument hoverPos.TextDocument.Uri hoverPos.Position

        let! contents =
            match maybeSymbol with
            | Some (sym, doc, pos) -> async {
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

                return Documentation.markdownDocForSymbolWithSignature sym semanticModel pos
                       |> fun s -> MarkedString.WithLanguage { Language = "markdown"; Value = s }
                       |> fun s -> [s]
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

    let handleTextDocumentPrepareRename (_scope: ServerRequestScope)
                                        (prepareRename: PrepareRenameParams)
                                        : AsyncLspResult<PrepareRenameResult option> =
        async {
            let! ct = Async.CancellationToken
            let! docMaybe = getDocumentForUriFromCurrentState UserDocument
                                                              prepareRename.TextDocument.Uri
            let! prepareResult =
                match docMaybe with
                | Some doc -> async {
                    let! docSyntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
                    let! docText = doc.GetTextAsync() |> Async.AwaitTask

                    let position = docText.Lines.GetPosition(LinePosition(prepareRename.Position.Line, prepareRename.Position.Character))
                    let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
                    let symbolIsFromMetadata =
                        symbolMaybe
                        |> Option.ofObj
                        |> Option.map (fun s -> s.MetadataToken <> 0)
                        |> Option.defaultValue false

                    let linePositionSpan = LinePositionSpan(
                        roslynLinePositionForLspPosition prepareRename.Position,
                        roslynLinePositionForLspPosition prepareRename.Position)

                    let textSpan = docText.Lines.GetTextSpan(linePositionSpan)

                    let! rootNode = docSyntaxTree.GetRootAsync() |> Async.AwaitTask
                    let nodeOnPos = rootNode.FindNode(textSpan, findInsideTrivia=false, getInnermostNodeForTie=true)

                    let! spanMaybe =
                        match nodeOnPos with
                        | :? PropertyDeclarationSyntax as propDec              -> propDec.Identifier.Span        |> Some |> async.Return
                        | :? MethodDeclarationSyntax as methodDec              -> methodDec.Identifier.Span      |> Some |> async.Return
                        | :? BaseTypeDeclarationSyntax as typeDec              -> typeDec.Identifier.Span        |> Some |> async.Return
                        | :? VariableDeclaratorSyntax as varDec                -> varDec.Identifier.Span         |> Some |> async.Return
                        | :? EnumMemberDeclarationSyntax as enumMemDec         -> enumMemDec.Identifier.Span     |> Some |> async.Return
                        | :? ParameterSyntax as paramSyn                       -> paramSyn.Identifier.Span       |> Some |> async.Return
                        | :? NameSyntax as nameSyn                             -> nameSyn.Span                   |> Some |> async.Return
                        | :? SingleVariableDesignationSyntax as designationSyn -> designationSyn.Identifier.Span |> Some |> async.Return
                        | :? ForEachStatementSyntax as forEachSyn              -> forEachSyn.Identifier.Span     |> Some |> async.Return
                        | :? LocalFunctionStatementSyntax as localFunStSyn     -> localFunStSyn.Identifier.Span  |> Some |> async.Return
                        | node -> async {
                            do! logMessage (sprintf "handleTextDocumentPrepareRename: unhandled Type=%s" (string (node.GetType().Name)))
                            return None
                          }

                    let rangeWithPlaceholderMaybe: PrepareRenameResult option =
                        match spanMaybe, symbolIsFromMetadata with
                        | Some span, false ->
                            let range =
                                docText.Lines.GetLinePositionSpan(span)
                                |> lspRangeForRoslynLinePosSpan

                            let text = docText.GetSubText(span) |> string
                                                
                            { Range = range; Placeholder = text }
                            |> Types.PrepareRenameResult.RangeWithPlaceholder
                            |> Some
                        | _, _ ->
                            None

                    return rangeWithPlaceholderMaybe
                  }
                | None -> None |> async.Return

            return prepareResult |> success
        }

    let handleTextDocumentRename (scope: ServerRequestScope) (rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = async {
        let! ct = Async.CancellationToken

        let renameSymbolInDoc symbol (doc: Document) = async {
            let originalSolution = doc.Project.Solution

            let! updatedSolution =
                Renamer.RenameSymbolAsync(doc.Project.Solution,
                                      symbol,
                                      SymbolRenameOptions(RenameOverloads=true, RenameFile=true),
                                      rename.NewName,
                                      ct)
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

    let handleTextDocumentSignatureHelp (scope: ServerRequestScope) (sigHelpParams: Types.SignatureHelpParams): AsyncLspResult<Types.SignatureHelp option> =
        let docMaybe = scope.GetUserDocumentForUri sigHelpParams.TextDocument.Uri
        match docMaybe with
        | Some doc -> async {
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

            let position =
                LinePosition(sigHelpParams.Position.Line, sigHelpParams.Position.Character)
                |> sourceText.Lines.GetPosition

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! root = syntaxTree.GetRootAsync() |> Async.AwaitTask

            let rec findInvocationContext (node: SyntaxNode): InvocationContext option =
                match node with
                | :? InvocationExpressionSyntax as invocation ->
                    match invocation.ArgumentList.Span.Contains(position) with
                    | true ->
                        Some { Receiver      = invocation.Expression
                               ArgumentTypes = (invocation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (invocation.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }
                    | false -> findInvocationContext node.Parent

                | :? BaseObjectCreationExpressionSyntax as objectCreation ->
                    let argumentListContainsPosition =
                        objectCreation.ArgumentList
                        |> Option.ofObj
                        |> Option.map (fun argList -> argList.Span.Contains(position))

                    match argumentListContainsPosition with
                    | Some true ->
                        Some { Receiver      = objectCreation
                               ArgumentTypes = (objectCreation.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (objectCreation.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }
                    | _ -> findInvocationContext node.Parent

                | :? AttributeSyntax as attributeSyntax ->
                    let argListContainsPosition =
                        attributeSyntax.ArgumentList
                        |> Option.ofObj
                        |> Option.map (fun argList -> argList.Span.Contains(position))

                    match argListContainsPosition with
                    | Some true ->
                        Some { Receiver      = attributeSyntax
                               ArgumentTypes = (attributeSyntax.ArgumentList.Arguments |> Seq.map (fun a -> semanticModel.GetTypeInfo(a.Expression))) |> List.ofSeq
                               Separators    = (attributeSyntax.ArgumentList.Arguments.GetSeparators()) |> List.ofSeq }

                    | _ -> findInvocationContext node.Parent

                | _ ->
                    match node |> Option.ofObj with
                    | Some nonNullNode -> findInvocationContext nonNullNode.Parent
                    | None -> None

            let invocationMaybe = root.FindToken(position).Parent
                                  |> findInvocationContext
            return
                match invocationMaybe with
                | Some invocation ->
                    let methodGroup =
                        (semanticModel.GetMemberGroup(invocation.Receiver)
                                      .OfType<IMethodSymbol>())
                        |> List.ofSeq

                    let methodScore (m: IMethodSymbol) =
                        if m.Parameters.Length < invocation.ArgumentTypes.Length then
                            -1
                        else
                            let maxParams = Math.Max(m.Parameters.Length, invocation.ArgumentTypes.Length)
                            let paramCountScore = maxParams - Math.Abs(m.Parameters.Length - invocation.ArgumentTypes.Length)

                            let minParams = Math.Min(m.Parameters.Length, invocation.ArgumentTypes.Length)
                            let paramMatchingScore =
                                [0..minParams-1]
                                |> Seq.map (fun pi -> (m.Parameters[pi], invocation.ArgumentTypes[pi]))
                                |> Seq.map (fun (pt, it) -> SymbolEqualityComparer.Default.Equals(pt.Type, it.ConvertedType))
                                |> Seq.map (fun m -> if m then 1 else 0)
                                |> Seq.sum

                            paramCountScore + paramMatchingScore

                    let matchingMethodMaybe =
                        methodGroup
                        |> Seq.map (fun m -> (m, methodScore m))
                        |> Seq.sortByDescending snd
                        |> Seq.map fst
                        |> Seq.tryHead

                    let signatureForMethod (m: IMethodSymbol) =
                        let parameters =
                            m.Parameters
                            |> Seq.map (fun p -> { Label = (string p); Documentation = None })
                            |> Array.ofSeq

                        let documentation =
                            Types.Documentation.Markup {
                                Kind = MarkupKind.Markdown
                                Value = Documentation.markdownDocForSymbol m
                            }

                        { Label         = formatSymbol m true (Some semanticModel) (Some position)
                          Documentation = Some documentation
                          Parameters    = Some parameters
                        }

                    let activeParameterMaybe =
                        match matchingMethodMaybe with
                        | Some _ ->
                            let mutable p = 0
                            for comma in invocation.Separators do
                                let commaBeforePos = comma.Span.Start < position
                                p <- p + (if commaBeforePos then 1 else 0)
                            Some p

                        | None -> None

                    let signatureHelpResult =
                        { Signatures = methodGroup
                                       |> Seq.map signatureForMethod
                                       |> Array.ofSeq

                          ActiveSignature = matchingMethodMaybe
                                            |> Option.map (fun m -> List.findIndex ((=) m) methodGroup)

                          ActiveParameter = activeParameterMaybe }

                    Some signatureHelpResult |> success

                | None ->
                    None |> success
          }

        | None ->
            None |> success |> async.Return

    let toSemanticToken (lines: TextLineCollection) (textSpan: TextSpan, spans: IEnumerable<ClassifiedSpan>) =
        let (typeId, modifiers) =
            spans
            |> Seq.fold (fun (t, m) s ->
                if ClassificationTypeNames.AdditiveTypeNames.Contains(s.ClassificationType) then
                    (t, m ||| (GetSemanticTokenModifierFlagFromClassification s.ClassificationType))
                else
                    (GetSemanticTokenIdFromClassification s.ClassificationType, m)
            ) (None, 0u)
        let pos = lines.GetLinePositionSpan(textSpan)
        (uint32 pos.Start.Line, uint32 pos.Start.Character, uint32 (pos.End.Character - pos.Start.Character), typeId, modifiers)

    let computePosition (((pLine, pChar, _, _, _), (cLine, cChar, cLen, cToken, cModifiers)): ((uint32 * uint32 * uint32 * uint32 * uint32) * (uint32 * uint32 * uint32 * uint32 * uint32))) =
        let deltaLine = cLine - pLine
        let deltaChar =
            if deltaLine = 0u then
                cChar - pChar
            else
                cChar
        (deltaLine, deltaChar, cLen, cToken, cModifiers)

    let getSemanticTokensRange (scope: ServerRequestScope) (uri: string) (range: Range option): AsyncLspResult<Types.SemanticTokens option> = async {
        let docMaybe = scope.GetUserDocumentForUri uri
        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let textSpan =
                match range with
                | Some r ->
                    r
                    |> roslynLinePositionSpanForLspRange
                    |> sourceText.Lines.GetTextSpan
                | None ->
                    TextSpan(0, sourceText.Length)
            let! spans = Classifier.GetClassifiedSpansAsync(doc, textSpan) |> Async.AwaitTask
            let tokens =
                spans
                |> Seq.groupBy (fun span -> span.TextSpan)
                |> Seq.map (toSemanticToken sourceText.Lines)
                |> Seq.filter (fun (_, _, _, oi, _) -> Option.isSome oi)
                |> Seq.map (fun (line, startChar, len, tokenId, modifiers) -> (line, startChar, len, Option.get tokenId, modifiers))

            let response =
                { Data =
                    Seq.zip (seq {yield (0u,0u,0u,0u,0u); yield! tokens}) tokens
                    |> Seq.map computePosition
                    |> Seq.map (fun (a,b,c,d,e) -> [a;b;c;d;e])
                    |> Seq.concat
                    |> Seq.toArray
                  ResultId = None } // TODO: add a result id after we support delta semantic tokens
            return Some response |> success
    }

    let handleSemanticTokensFull (scope: ServerRequestScope) (semParams: Types.SemanticTokensParams): AsyncLspResult<Types.SemanticTokens option> =
        getSemanticTokensRange scope semParams.TextDocument.Uri None

    let handleSemanticTokensRange (scope: ServerRequestScope) (semParams: Types.SemanticTokensRangeParams): AsyncLspResult<Types.SemanticTokens option> =
        getSemanticTokensRange scope semParams.TextDocument.Uri (Some semParams.Range)

    let handleWorkspaceSymbol (scope: ServerRequestScope) (symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let! symbols = findSymbolsInSolution scope.Solution symbolParams.Query (Some 20)
        return symbols |> Array.ofSeq |> Some |> success
    }

    let handleWorkspaceDidChangeWatchedFiles (scope: ServerRequestScope) (changeParams: Types.DidChangeWatchedFilesParams): Async<LspResult<unit>> = async {
        let tryReloadDocumentOnUri uri = async {
            match scope.GetDocumentForUriOfType UserDocument uri with
            | Some (doc, _) ->
                let fileText = uri |> Util.parseFileUri |> File.ReadAllText
                let updatedDoc = SourceText.From(fileText) |> doc.WithText

                scope.Emit(SolutionChange updatedDoc.Project.Solution)
                diagnostics.Post(DocumentBacklogUpdate)

            | None ->
                let docFilePathMaybe = uri |> Util.tryParseFileUri
                match docFilePathMaybe with
                | Some docFilePath ->
                    // ok, this document is not on solution, register a new one
                    let fileText = docFilePath |> File.ReadAllText
                    let! newDocMaybe = tryAddDocument logMessage
                                                     docFilePath
                                                     fileText
                                                     scope.Solution
                    match newDocMaybe with
                    | Some newDoc ->
                        scope.Emit(SolutionChange newDoc.Project.Solution)
                        diagnostics.Post(DocumentBacklogUpdate)
                    | None -> ()
                | None -> ()
        }

        for change in changeParams.Changes do
            match Path.GetExtension(change.Uri) with
            | ".csproj" ->
                do! logMessage "change to .csproj detected, will reload solution"
                scope.Emit(SolutionReloadRequest)

            | ".sln" ->
                do! logMessage "change to .sln detected, will reload solution"
                scope.Emit(SolutionReloadRequest)

            | ".cs" ->
                match change.Type with
                | FileChangeType.Created ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Changed ->
                    do! tryReloadDocumentOnUri change.Uri

                | FileChangeType.Deleted ->
                    match scope.GetDocumentForUriOfType UserDocument change.Uri with
                    | Some (existingDoc, _) ->
                        let updatedProject = existingDoc.Project.RemoveDocument(existingDoc.Id)

                        scope.Emit(SolutionChange updatedProject.Solution)
                        scope.Emit(OpenDocVersionRemove change.Uri)

                        diagnostics.Post(DocumentRemoval change.Uri)
                    | None -> ()
                | _ -> ()

            | _ -> ()

        return LspResult.Ok()
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

    let requestHandlingWithScope requestType requestPriority nameAndAsyncFn =
        let requestName = nameAndAsyncFn |> fst
        let asyncFn = nameAndAsyncFn |> snd

        let requestHandler param =
            logMessageCurrent <- logMessage

            // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
            //
            // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
            // handler until previous one returns a Task (in our case -- F# `async` object.)

            let startRequest rc = StartRequest (requestName, requestType, requestPriority, rc)
            let requestId, semaphore = stateActor.PostAndReply(startRequest)

            let stateAcquisitionAndHandlerInvocation = async {
                do! semaphore.WaitAsync() |> Async.AwaitTask

                let! state = stateActor.PostAndAsyncReply(GetState)

                let scope = ServerRequestScope(requestId, state, stateActor.Post, logMessage)

                return! asyncFn scope param
            }

            let wrapExceptionAsLspResult op =
                async {
                    let! resultOrExn = op |> Async.Catch

                    return
                        match resultOrExn with
                        | Choice1Of2 result -> result
                        | Choice2Of2 exn ->
                            match exn with
                            | :? TaskCanceledException -> LspResult.requestCancelled
                            | :? OperationCanceledException -> LspResult.requestCancelled
                            | _ -> LspResult.internalError (string exn)
                }

            stateAcquisitionAndHandlerInvocation
            |> wrapExceptionAsLspResult
            |> unwindProtect (fun () -> stateActor.Post(FinishRequest requestId))

        (requestName, requestHandler |> requestHandling)

    let requestHandlingWithReadOnlyScope nameAndAsyncFn =
        requestHandlingWithScope ReadOnly 0 nameAndAsyncFn

    let requestHandlingWithReadOnlyScopeWithPriority priority nameAndAsyncFn =
        requestHandlingWithScope ReadOnly priority nameAndAsyncFn

    let requestHandlingWithReadWriteScope nameAndAsyncFn =
        requestHandlingWithScope ReadWrite 0 nameAndAsyncFn

    [
        ("initialize"                     , handleInitialize)                    |> requestHandlingWithReadWriteScope
        ("initialized"                    , handleInitialized)                   |> requestHandlingWithReadWriteScope
        ("textDocument/didChange"         , handleTextDocumentDidChange)         |> requestHandlingWithReadWriteScope
        ("textDocument/didClose"          , handleTextDocumentDidClose)          |> requestHandlingWithReadWriteScope
        ("textDocument/didOpen"           , handleTextDocumentDidOpen)           |> requestHandlingWithReadWriteScope
        ("textDocument/didSave"           , handleTextDocumentDidSave)           |> requestHandlingWithReadWriteScope
        ("textDocument/codeAction"        , handleTextDocumentCodeAction)        |> requestHandlingWithReadOnlyScope
        ("codeAction/resolve"             , handleCodeActionResolve)             |> requestHandlingWithReadOnlyScope
        ("textDocument/codeLens"          , handleTextDocumentCodeLens)          |> requestHandlingWithReadOnlyScopeWithPriority 99
        ("codeLens/resolve"               , handleCodeLensResolve)               |> requestHandlingWithReadOnlyScope
        ("textDocument/completion"        , handleTextDocumentCompletion)        |> requestHandlingWithReadOnlyScope
        ("textDocument/definition"        , handleTextDocumentDefinition)        |> requestHandlingWithReadOnlyScope
        ("textDocument/documentHighlight" , handleTextDocumentDocumentHighlight) |> requestHandlingWithReadOnlyScope
        ("textDocument/documentSymbol"    , handleTextDocumentDocumentSymbol)    |> requestHandlingWithReadOnlyScope
        ("textDocument/hover"             , handleTextDocumentHover)             |> requestHandlingWithReadOnlyScope
        ("textDocument/implementation"    , handleTextDocumentImplementation)    |> requestHandlingWithReadOnlyScope
        ("textDocument/formatting"        , handleTextDocumentFormatting)        |> requestHandlingWithReadOnlyScope
        ("textDocument/onTypeFormatting"  , handleTextDocumentOnTypeFormatting)  |> requestHandlingWithReadOnlyScope
        ("textDocument/rangeFormatting"   , handleTextDocumentRangeFormatting)   |> requestHandlingWithReadOnlyScope
        ("textDocument/references"        , handleTextDocumentReferences)        |> requestHandlingWithReadOnlyScope
        ("textDocument/prepareRename"     , handleTextDocumentPrepareRename)     |> requestHandlingWithReadOnlyScope
        ("textDocument/rename"            , handleTextDocumentRename)            |> requestHandlingWithReadOnlyScope
        ("textDocument/signatureHelp"     , handleTextDocumentSignatureHelp)     |> requestHandlingWithReadOnlyScope
        ("textDocument/semanticTokens/full", handleSemanticTokensFull)           |> requestHandlingWithReadOnlyScope
        ("textDocument/semanticTokens/range", handleSemanticTokensRange)         |> requestHandlingWithReadOnlyScope
        ("workspace/symbol"               , handleWorkspaceSymbol)               |> requestHandlingWithReadOnlyScope
        ("workspace/didChangeWatchedFiles", handleWorkspaceDidChangeWatchedFiles) |> requestHandlingWithReadWriteScope
        ("csharp/metadata"                , handleCSharpMetadata)                |> requestHandlingWithReadOnlyScope
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
        defaultRpc

let start options =
    try
        let result = startCore options
        int result
    with
    | _ex ->
        // logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
