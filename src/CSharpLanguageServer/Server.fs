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
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CodeFixes
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open RoslynHelpers
open State
open System.Threading.Tasks
open System.Threading

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

let setupServerHandlers options (lspClient: LspClient) =
    let success = LspResult.success
    let mutable logMessageCurrent = Action<string>(fun _ -> ())
    let logMessageInvoke m = logMessageCurrent.Invoke(m)

    let stateActor = MailboxProcessor.Start(
        serverEventLoop logMessageInvoke { emptyServerState with Options = options })

    let getDocumentForUriFromCurrentState docType uri =
        stateActor.PostAndAsyncReply(fun rc -> GetDocumentOfTypeForUri (docType, uri, rc))

    let diagnostics = MailboxProcessor.Start(
        fun inbox -> async {
            let rec loop state = async {
                try
                    let! msg = inbox.Receive()
                    let! (newState, eventsToPost) =
                        processDiagnosticsEvent logMessageInvoke
                                                (fun docUri diagnostics -> lspClient.TextDocumentPublishDiagnostics { Uri = docUri; Diagnostics = diagnostics })
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
      logMessageCurrent <- Action<string>(logMessage)

      infoMessage (sprintf "initializing, csharp-ls version %s; options are: %s"
                            (typeof<CSharpLspClient>.Assembly.GetName().Version |> string)
                            (JsonConvert.SerializeObject(options)))

      infoMessage "csharp-ls is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"

      scope.Emit(ClientCapabilityChange p.Capabilities)

      // setup timer so actors get period ticks
      setupTimer ()

      let initializeResult = {
              InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = First true |> Some
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
                        SemanticTokensProvider = None
                    }
              }

      async {
          // load solution (on stateActor)
          do! stateActor.PostAndAsyncReply(SolutionReloadInSync)

          return initializeResult |> success
      }

    let handleInitialized (_scope: ServerRequestScope) (_p: InitializedParams): Async<LspResult<unit>> =
        logMessage "\"initialized\" notification received from client"
        async {
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
            | Error error -> infoMessage (sprintf "  ...didChangeWatchedFiles registration has failed with %s" (error |> string))

            return LspResult.Ok()
        }

    let handleTextDocumentDidOpen (scope: ServerRequestScope) (openParams: Types.DidOpenTextDocumentParams): Async<LspResult<unit>> =
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

        return LspResult.Ok()
    }

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

    let handleTextDocumentDocumentSymbol (scope: ServerRequestScope) (p: Types.DocumentSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let docMaybe = scope.GetAnyDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let collector = DocumentSymbolCollector(p.TextDocument.Uri, semanticModel)

            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetSymbols() |> Some |> success

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

    let handleWorkspaceSymbol (scope: ServerRequestScope) (symbolParams: Types.WorkspaceSymbolParams): AsyncLspResult<Types.SymbolInformation [] option> = async {
        let! symbols = findSymbolsInSolution scope.Solution symbolParams.Query (Some 20)
        return symbols |> Array.ofSeq |> Some |> success
    }

    let handleWorkspaceDidChangeWatchedFiles (scope: ServerRequestScope) (changeParams: Types.DidChangeWatchedFilesParams): Async<LspResult<unit>> = async {
        let tryReloadDocumentOnUri uri = async {
            match scope.GetDocumentForUriOfType UserDocument uri with
            | Some (doc, _) ->
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

    let withScope requestType requestPriority asyncFn param =
        // we want to be careful and lock solution for change immediately w/o entering async/returing an `async` workflow
        //
        // StreamJsonRpc lib we're using in Ionide.LanguageServerProtocol guarantees that it will not call another
        // handler until previous one returns a Task (in our case -- F# `async` object.)

        let requestId, semaphore = stateActor.PostAndReply(fun rc -> StartRequest (requestType, requestPriority, rc))

        async {
            do! semaphore.WaitAsync() |> Async.AwaitTask

            let state = stateActor.PostAndReply(GetState)

            try
                let scope = ServerRequestScope(state, stateActor.Post, logMessage)
                try
                    return! asyncFn scope param
                with
                | ex ->
                    let unpackedEx = ex |> unpackException
                    if (unpackedEx :? TaskCanceledException) || (unpackedEx :? OperationCanceledException) then
                        return LspResult.requestCancelled
                    else
                        return raise ex
            finally
                stateActor.Post(FinishRequest requestId)
        }

    let withReadOnlyScope asyncFn param = withScope ReadOnly 0 asyncFn param
    let withReadOnlyScopeWithPriority priority asyncFn param = withScope ReadOnly priority asyncFn param
    let withReadWriteScope asyncFn param = withScope ReadWrite 0 asyncFn param

    [
        "initialize"                     , handleInitialize                    |> withReadWriteScope |> requestHandling
        "initialized"                    , handleInitialized                   |> withReadWriteScope |> requestHandling
        "textDocument/didChange"         , handleTextDocumentDidChange         |> withReadWriteScope |> requestHandling
        "textDocument/didClose"          , handleTextDocumentDidClose          |> withReadWriteScope |> requestHandling
        "textDocument/didOpen"           , handleTextDocumentDidOpen           |> withReadWriteScope |> requestHandling
        "textDocument/didSave"           , handleTextDocumentDidSave           |> withReadWriteScope |> requestHandling
        "textDocument/codeAction"        , handleTextDocumentCodeAction        |> withReadOnlyScope |> requestHandling
        "codeAction/resolve"             , handleCodeActionResolve             |> withReadOnlyScope |> requestHandling
        "textDocument/codeLens"          , handleTextDocumentCodeLens          |> withReadOnlyScopeWithPriority 99 |> requestHandling
        "codeLens/resolve"               , handleCodeLensResolve               |> withReadOnlyScope |> requestHandling
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
        "textDocument/signatureHelp"     , handleTextDocumentSignatureHelp     |> withReadOnlyScope |> requestHandling
        "workspace/symbol"               , handleWorkspaceSymbol               |> withReadOnlyScope |> requestHandling
        "workspace/didChangeWatchedFiles", handleWorkspaceDidChangeWatchedFiles |> withReadWriteScope |> requestHandling
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
