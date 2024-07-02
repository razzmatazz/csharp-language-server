namespace CSharpLanguageServer.Handlers

open System
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Rename
open FSharpPlus
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Rename =
    let private logger = LogProvider.getLoggerByName "Rename"

    let private lspDocChangesFromSolutionDiff
        (ct: CancellationToken)
        (originalSolution: Solution)
        (updatedSolution: Solution)
        (tryGetDocVersionByUri: string -> int option)
        : Async<TextDocumentEdit[]> =
        let getEdits
            (originalSolution: Solution)
            (updatedSolution: Solution)
            (docId: DocumentId)
            : Async<TextDocumentEdit> = async {
            let originalDoc = originalSolution.GetDocument(docId)
            let! originalDocText = originalDoc.GetTextAsync(ct) |> Async.AwaitTask
            let updatedDoc = updatedSolution.GetDocument(docId)
            let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc, ct) |> Async.AwaitTask

            let diffEdits: U2<TextEdit, AnnotatedTextEdit> array =
                docChanges
                |> Seq.sortBy (fun c -> c.Span.Start)
                |> Seq.map (TextEdit.fromTextChange originalDocText.Lines)
                |> Seq.map U2.C1
                |> Array.ofSeq

            let uri = originalDoc.FilePath |> Path.toUri
            let textEditDocument =
                { Uri = uri
                  Version = tryGetDocVersionByUri uri }

            return
                { TextDocument = textEditDocument
                  Edits = diffEdits }
        }

        updatedSolution.GetChanges(originalSolution).GetProjectChanges()
        |> Seq.collect (fun projectChange -> projectChange.GetChangedDocuments())
        |> Seq.map (getEdits originalSolution updatedSolution)
        |> Async.Parallel
        |> map (Seq.distinct >> Array.ofSeq)

    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.Rename)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let private prepareSupport (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.Rename)
        |> Option.bind (fun x -> x.PrepareSupport)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities): U2<bool, RenameOptions> option =
        match dynamicRegistration clientCapabilities, prepareSupport clientCapabilities with
        | true, _ -> None
        | false, true -> Some (U2.C2 { PrepareProvider = Some true; WorkDoneProgress = None })
        | false, false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: RenameRegistrationOptions = {
                PrepareProvider = Some (prepareSupport clientCapabilities)
                DocumentSelector = Some defaultDocumentSelector
                WorkDoneProgress = None
            }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/rename"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare (context: ServerRequestContext)
                (p: PrepareRenameParams)
                : AsyncLspResult<PrepareRenameResult option> = async {
        match context.GetUserDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! docSyntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let position = Position.toRoslynPosition docText.Lines p.Position
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask
            let symbolIsFromMetadata =
                symbolMaybe
                |> Option.ofObj
                |> Option.map (fun s -> s.MetadataToken <> 0)
                |> Option.defaultValue false

            let linePositionSpan =
                Range.toLinePositionSpan docText.Lines { Start = p.Position; End = p.Position }

            let textSpan = docText.Lines.GetTextSpan(linePositionSpan)

            let! rootNode = docSyntaxTree.GetRootAsync(ct) |> Async.AwaitTask
            let nodeOnPos =
                rootNode.FindNode(textSpan, findInsideTrivia = false, getInnermostNodeForTie = true)

            let spanMaybe =
                match nodeOnPos with
                | :? PropertyDeclarationSyntax as propDec              -> propDec.Identifier.Span        |> Some
                | :? MethodDeclarationSyntax as methodDec              -> methodDec.Identifier.Span      |> Some
                | :? BaseTypeDeclarationSyntax as typeDec              -> typeDec.Identifier.Span        |> Some
                | :? VariableDeclaratorSyntax as varDec                -> varDec.Identifier.Span         |> Some
                | :? EnumMemberDeclarationSyntax as enumMemDec         -> enumMemDec.Identifier.Span     |> Some
                | :? ParameterSyntax as paramSyn                       -> paramSyn.Identifier.Span       |> Some
                | :? NameSyntax as nameSyn                             -> nameSyn.Span                   |> Some
                | :? SingleVariableDesignationSyntax as designationSyn -> designationSyn.Identifier.Span |> Some
                | :? ForEachStatementSyntax as forEachSyn              -> forEachSyn.Identifier.Span     |> Some
                | :? LocalFunctionStatementSyntax as localFunStSyn     -> localFunStSyn.Identifier.Span  |> Some
                | node ->
                    logger.debug (
                        Log.setMessage "textDocument/prepareRename: unhandled Type={type}"
                        >> Log.addContext "type" (node.GetType().Name)
                    )
                    None

            let rangeWithPlaceholderMaybe: PrepareRenameResult option =
                match spanMaybe, symbolIsFromMetadata with
                | Some span, false ->
                    let range = Range.fromTextSpan docText.Lines span

                    let text = docText.ToString(span)

                    { Range = range; Placeholder = text } |> U3.C2 |> Some
                | _, _ -> None

            return rangeWithPlaceholderMaybe |> success
    }

    let handle
            (context: ServerRequestContext)
            (p: RenameParams)
            : AsyncLspResult<WorkspaceEdit option> = async {
        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc) ->
            let! ct = Async.CancellationToken
            let originalSolution = doc.Project.Solution

            let! updatedSolution =
                Renamer.RenameSymbolAsync(
                    doc.Project.Solution,
                    symbol,
                    SymbolRenameOptions(RenameOverloads = true, RenameFile = true),
                    p.NewName,
                    ct
                )
                |> Async.AwaitTask

            let! docTextEdit =
                lspDocChangesFromSolutionDiff ct originalSolution updatedSolution context.OpenDocVersions.TryFind

            return WorkspaceEdit.Create(docTextEdit, context.ClientCapabilities) |> Some |> success
    }
