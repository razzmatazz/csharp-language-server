namespace CSharpLanguageServer.Handlers

open System
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Rename
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Microsoft.Extensions.Logging

open CSharpLanguageServer.State
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Rename =
    let private logger = Logging.getLoggerByName "Rename"

    let private lspDocChangesFromSolutionDiff
        (ct: CancellationToken)
        (wf: LspWorkspaceFolder)
        (originalSolution: Solution)
        (updatedSolution: Solution)
        (tryGetDocVersionByUri: string -> int option)
        : Async<TextDocumentEdit[]> =
        let getEdits
            (originalSolution: Solution)
            (updatedSolution: Solution)
            (docId: DocumentId)
            : Async<TextDocumentEdit> =
            async {
                let originalDoc =
                    match originalSolution.GetDocument(docId) with
                    | null -> failwith "could not originalSolution.GetDocument(docId)"
                    | x -> x

                let! originalDocText = originalDoc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedDoc =
                    match updatedSolution.GetDocument(docId) with
                    | null -> failwith "could not updatedSolution.GetDocument(docId)"
                    | x -> x

                let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc, ct) |> Async.AwaitTask

                let diffEdits: U2<TextEdit, AnnotatedTextEdit> array =
                    docChanges
                    |> Seq.sortBy (fun c -> c.Span.Start)
                    |> Seq.map (TextEdit.fromTextChange originalDocText.Lines)
                    |> Seq.map U2.C1
                    |> Array.ofSeq

                let uri = originalDoc.FilePath |> workspaceFolderPathToUri wf

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
        |> Async.map (Seq.distinct >> Array.ofSeq)

    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Rename
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let private prepareSupport (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Rename
        |> Option.bind _.PrepareSupport
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, RenameOptions> option =
        match dynamicRegistration cc, prepareSupport cc with
        | true, _ -> None
        | false, true ->
            Some(
                U2.C2
                    { PrepareProvider = Some true
                      WorkDoneProgress = None }
            )
        | false, false -> Some(U2.C1 true)

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: RenameRegistrationOptions =
                { PrepareProvider = Some(prepareSupport cc)
                  DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/rename"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let prepare (context: ServerRequestContext) (p: PrepareRenameParams) : AsyncLspResult<PrepareRenameResult option> = async {

        let wf, docForUri =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match docForUri with
        | None -> return None |> LspResult.success
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
                | :? PropertyDeclarationSyntax as propDec -> propDec.Identifier.Span |> Some
                | :? MethodDeclarationSyntax as methodDec -> methodDec.Identifier.Span |> Some
                | :? BaseTypeDeclarationSyntax as typeDec -> typeDec.Identifier.Span |> Some
                | :? VariableDeclaratorSyntax as varDec -> varDec.Identifier.Span |> Some
                | :? EnumMemberDeclarationSyntax as enumMemDec -> enumMemDec.Identifier.Span |> Some
                | :? ParameterSyntax as paramSyn -> paramSyn.Identifier.Span |> Some
                | :? NameSyntax as nameSyn -> nameSyn.Span |> Some
                | :? SingleVariableDesignationSyntax as designationSyn -> designationSyn.Identifier.Span |> Some
                | :? ForEachStatementSyntax as forEachSyn -> forEachSyn.Identifier.Span |> Some
                | :? LocalFunctionStatementSyntax as localFunStSyn -> localFunStSyn.Identifier.Span |> Some
                | node ->
                    logger.LogDebug("textDocument/prepareRename: unhandled Type={type}", (node.GetType().Name))
                    None

            let rangeWithPlaceholderMaybe: PrepareRenameResult option =
                match spanMaybe, symbolIsFromMetadata with
                | Some span, false ->
                    let range = Range.fromTextSpan docText.Lines span

                    let text = docText.ToString(span)

                    { Range = range; Placeholder = text } |> U3.C2 |> Some
                | _, _ -> None

            return rangeWithPlaceholderMaybe |> LspResult.success
    }

    let handle (context: ServerRequestContext) (p: RenameParams) : AsyncLspResult<WorkspaceEdit option> = async {
        match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
        | Some wf, Some(symbol, project, _) ->
            let! ct = Async.CancellationToken
            let originalSolution = project.Solution

            let! updatedSolution =
                Renamer.RenameSymbolAsync(
                    project.Solution,
                    symbol,
                    SymbolRenameOptions(RenameOverloads = true, RenameFile = true),
                    p.NewName,
                    ct
                )
                |> Async.AwaitTask


            let! docTextEdit =
                lspDocChangesFromSolutionDiff
                    ct
                    wf
                    originalSolution
                    updatedSolution
                    (workspaceDocumentVersion context.Workspace)

            return
                WorkspaceEdit.Create(docTextEdit, context.ClientCapabilities)
                |> Some
                |> LspResult.success

        | _, _ -> return None |> LspResult.success
    }
