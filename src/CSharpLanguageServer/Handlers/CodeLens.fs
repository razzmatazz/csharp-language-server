namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Newtonsoft.Json.Linq

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

type private DocumentSymbolCollectorForCodeLens (semanticModel: SemanticModel) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols = []

    let collect (node: SyntaxNode) (nameSpan: TextSpan) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collectedSymbols <- (symbol, nameSpan) :: collectedSymbols

    member __.GetSymbols() = collectedSymbols |> List.rev |> Array.ofList

    override __.VisitEnumDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitEnumDeclaration(node)

    override __.VisitEnumMemberDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitEnumMemberDeclaration(node)

    override __.VisitClassDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitClassDeclaration(node)

    override __.VisitRecordDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitRecordDeclaration(node)

    override __.VisitStructDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitStructDeclaration(node)

    override __.VisitInterfaceDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitInterfaceDeclaration(node)

    override __.VisitDelegateDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitDelegateDeclaration(node)

    override __.VisitConstructorDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitConstructorDeclaration(node)

    override __.VisitDestructorDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitDestructorDeclaration(node)

    override __.VisitOperatorDeclaration(node) =
        collect node node.OperatorToken.Span
        base.VisitOperatorDeclaration(node)

    override __.VisitIndexerDeclaration(node) =
        collect node node.ThisKeyword.Span
        base.VisitIndexerDeclaration(node)

    override __.VisitConversionOperatorDeclaration(node) =
        collect node node.Type.Span
        base.VisitConversionOperatorDeclaration(node)

    override __.VisitMethodDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitMethodDeclaration(node)

    override __.VisitPropertyDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitPropertyDeclaration(node)

    override __.VisitVariableDeclarator(node) =
        let grandparent =
            node.Parent |> Option.ofObj
            |> Option.bind (fun node -> node.Parent |> Option.ofObj)
        // Only show field variables and ignore local variables
        if grandparent.IsSome && grandparent.Value :? FieldDeclarationSyntax then
            collect node node.Identifier.Span
            base.VisitVariableDeclarator(node)
        else
            base.VisitVariableDeclarator(node)

    override __.VisitEventDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitEventDeclaration(node)


[<RequireQualifiedAccess>]
module CodeLens =
    type CodeLensData = { DocumentUri: string; Position: Position  }

    let emptyCodeLensData = { DocumentUri=""; Position={ Line=0; Character=0 } }

    let provider (clientCapabilities: ClientCapabilities option) : CodeLensOptions option =
        Some { ResolveProvider = Some true }

    let handle (scope: ServerRequestScope) (lensParams: CodeLensParams): AsyncLspResult<CodeLens[] option> = async {
        let docMaybe = scope.GetAnyDocumentForUri lensParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForCodeLens(semanticModel)
            collector.Visit(syntaxTree.GetRoot())

            let makeCodeLens (_symbol: ISymbol, nameSpan: TextSpan): CodeLens =
                let start = nameSpan.Start |> docText.Lines.GetLinePosition

                let lensData: CodeLensData = {
                    DocumentUri = lensParams.TextDocument.Uri
                    Position = start |> Position.fromLinePosition
                }

                { Range = nameSpan |> docText.Lines.GetLinePositionSpan |> Range.fromLinePositionSpan
                  Command = None
                  Data = lensData |> JToken.FromObject |> Some
                }

            let codeLens = collector.GetSymbols() |> Seq.map makeCodeLens

            return codeLens |> Array.ofSeq |> Some |> LspResult.success

        | None ->
            return None |> LspResult.success
    }

    let resolve (scope: ServerRequestScope)
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

        let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, ct) |> Async.AwaitTask
        let locations = refs |> Seq.collect (fun r -> r.Locations)

        let title = locations |> Seq.length |> sprintf "%d Reference(s)"

        let command =
            { Title = title
              Command = "csharp.showReferences"
              Arguments = None // TODO: we really want to pass some more info to the client
            }

        return { codeLens with Command=Some command } |> LspResult.success
    }
