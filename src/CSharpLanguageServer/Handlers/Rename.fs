namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

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

    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Rename)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let private prepareSupport (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Rename)
        |> Option.bind (fun x -> x.PrepareSupport)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option): U2<bool, RenameOptions> option =
        match dynamicRegistration clientCapabilities, prepareSupport clientCapabilities with
        | true, _ -> None
        | false, true -> Some (Second { PrepareProvider = Some true })
        | false, false -> Some (First true)

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/rename"
                  RegisterOptions =
                      { PrepareProvider = Some (prepareSupport clientCapabilities)
                        DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

    let prepare (getDocumentForUriFromCurrentState: ServerDocumentType -> string -> Async<Document option>)
                (_scope: ServerRequestScope)
                (p: PrepareRenameParams)
                : AsyncLspResult<PrepareRenameResult option> = async {
        let! docMaybe = getDocumentForUriFromCurrentState UserDocument
                                                            p.TextDocument.Uri
        return!
          match docMaybe with
          | None -> None |> success |> async.Return
          | Some doc -> async {
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
    }

    let handle
        (scope: ServerRequestScope)
        (p: RenameParams)
        : AsyncLspResult<WorkspaceEdit option> = async {
        let clientCapabilities = scope.ClientCapabilities
        let! maybeSymbol = scope.GetSymbolAtPositionOnUserDocument p.TextDocument.Uri p.Position

        return!
          match maybeSymbol with
          | None -> None |> success |> async.Return
          | Some (symbol, doc, _) -> async {
            let originalSolution = doc.Project.Solution

            let! updatedSolution =
                Renamer.RenameSymbolAsync(
                    doc.Project.Solution,
                    symbol,
                    SymbolRenameOptions(RenameOverloads = true, RenameInStrings = true, RenameInComments = true),
                    p.NewName
                )
                |> Async.AwaitTask

            let! docTextEdit = lspDocChangesFromSolutionDiff originalSolution updatedSolution scope.OpenDocVersions.TryFind

            let clientCapabilities =
                clientCapabilities
                |> Option.defaultValue
                    { Workspace = None
                      TextDocument = None
                      General = None
                      Experimental = None
                      Window = None }
            return WorkspaceEdit.Create(docTextEdit, clientCapabilities) |> Some |> success
          }
    }
