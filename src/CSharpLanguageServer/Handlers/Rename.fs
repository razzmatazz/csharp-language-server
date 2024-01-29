namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Rename
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Classification

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module Rename =
    let provider (clientCapabilities: ClientCapabilities option) : U2<bool, Types.RenameOptions> option =
        let clientSupportsRenameOptions =
            clientCapabilities
            |> Option.bind (fun x -> x.TextDocument)
            |> Option.bind (fun x -> x.Rename)
            |> Option.bind (fun x -> x.PrepareSupport)
            |> Option.defaultValue false

        if clientSupportsRenameOptions then
            U2.Second { PrepareProvider = Some true } |> Some
        else
            true |> U2.First |> Some

    let prepare (getDocumentForUriFromCurrentState: ServerDocumentType -> string -> Async<Document option>)
                (logMessage: Util.AsyncLogFn)
                (_scope: ServerRequestScope)
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
                        roslynLinePositionForLspPosition docText.Lines prepareRename.Position,
                        roslynLinePositionForLspPosition docText.Lines prepareRename.Position)

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

    let handle (scope: ServerRequestScope) (rename: Types.RenameParams): AsyncLspResult<Types.WorkspaceEdit option> = async {
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
                                          scope.logMessage
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
