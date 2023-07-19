namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Rename
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Rename =
    let private logger = LogProvider.getLoggerByName "Rename"

    let private lspDocChangesFromSolutionDiff
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
            let! originalDocText = originalDoc.GetTextAsync() |> Async.AwaitTask
            let updatedDoc = updatedSolution.GetDocument(docId)
            let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc) |> Async.AwaitTask

            let diffEdits: TextEdit array =
                docChanges
                |> Seq.sortBy (fun c -> c.Span.Start)
                |> Seq.map (TextEdit.fromTextChange originalDocText.Lines)
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

    let provider (clientCapabilities: ClientCapabilities option): U2<bool, RenameOptions> option =
        let prepareSupport =
            clientCapabilities
            |> Option.bind (fun x -> x.TextDocument)
            |> Option.bind (fun x -> x.Rename)
            |> Option.bind (fun x -> x.PrepareSupport)
            |> Option.defaultValue false
        if prepareSupport then
            Some (Second { PrepareProvider = Some true })
        else
            Some (First true)

    let prepare (wm: IWorkspaceManager) (p: PrepareRenameParams) : AsyncLspResult<PrepareRenameResult option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! docSyntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
            let! docText = doc.GetTextAsync() |> Async.AwaitTask

            let position = Position.toRoslynPosition docText.Lines p.Position
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask
            let symbolIsFromMetadata =
                symbolMaybe
                |> Option.ofObj
                |> Option.map (fun s -> s.MetadataToken <> 0)
                |> Option.defaultValue false

            let linePositionSpan =
                Range.toLinePositionSpan docText.Lines { Start = p.Position; End = p.Position }

            let textSpan = docText.Lines.GetTextSpan(linePositionSpan)

            let! rootNode = docSyntaxTree.GetRootAsync() |> Async.AwaitTask
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

                    { Range = range; Placeholder = text }
                    |> PrepareRenameResult.RangeWithPlaceholder
                    |> Some
                | _, _ -> None

            return rangeWithPlaceholderMaybe |> success
    }

    let handle
        (wm: IWorkspaceManager)
        (clientCapabilities: ClientCapabilities option)
        (p: RenameParams)
        : AsyncLspResult<WorkspaceEdit option> = async {
        match! wm.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc) ->
            let originalSolution = doc.Project.Solution

            let! updatedSolution =
                Renamer.RenameSymbolAsync(
                    doc.Project.Solution,
                    symbol,
                    SymbolRenameOptions(RenameOverloads = true, RenameInStrings = true, RenameInComments = true),
                    p.NewName
                )
                |> Async.AwaitTask

            let! docTextEdit = lspDocChangesFromSolutionDiff originalSolution updatedSolution wm.GetDocumentVersion

            let clientCapabilities =
                clientCapabilities
                |> Option.defaultValue
                    { Workspace = None
                      TextDocument = None
                      Experimental = None
                      Window = None
                      General = None }
            return WorkspaceEdit.Create(docTextEdit, clientCapabilities) |> Some |> success
    }
