namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Newtonsoft.Json.Linq

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

type private DocumentSymbolCollectorForCodeLens(semanticModel: SemanticModel) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols = []

    let collect (node: SyntaxNode) (nameSpan: TextSpan) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collectedSymbols <- (symbol, nameSpan) :: collectedSymbols

    member __.GetSymbols() =
        collectedSymbols |> List.rev |> Array.ofList

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
            node.Parent
            |> Option.ofObj
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
    type CodeLensData =
        { DocumentUri: string
          Position: Position }
        static member Default =
            { DocumentUri = ""
              Position = { Line = 0; Character = 0 } }

    let provider: CodeLensOptions option = Some { ResolveProvider = Some true }

    // TODO: Add configuration to disable some kind of code lens. For example, I think showing the reference
    // number of a private member/class is noisy since I usually don't care that.
    let handle (wm: IWorkspaceManager) (p: CodeLensParams) : AsyncLspResult<CodeLens[] option> = async {
        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync() |> Async.AwaitTask
            let! docText = doc.GetTextAsync() |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForCodeLens(semanticModel)
            collector.Visit(syntaxTree.GetRoot())

            let makeCodeLens (_symbol: ISymbol, nameSpan: TextSpan) : CodeLens =
                let start = nameSpan.Start |> docText.Lines.GetLinePosition

                let lensData: CodeLensData =
                    { DocumentUri = p.TextDocument.Uri
                      Position = start |> Position.fromLinePosition }

                { Range = nameSpan |> Range.fromTextSpan docText.Lines
                  Command = None
                  Data = lensData |> JToken.FromObject |> Some }

            let codeLens = collector.GetSymbols() |> Seq.map makeCodeLens

            return codeLens |> Array.ofSeq |> Some |> success
    }

    let resolve (wm: IWorkspaceManager) (p: CodeLens) : AsyncLspResult<CodeLens> = async {
        let lensData =
            p.Data
            |> Option.map (fun t -> t.ToObject<CodeLensData>())
            |> Option.defaultValue CodeLensData.Default

        match! wm.FindSymbol lensData.DocumentUri lensData.Position with
        | None -> return p |> success
        | Some symbol ->
            let! refs = wm.FindReferences symbol
            // FIXME: refNum is wrong. There are lots of false positive even if we distinct locations by
            // (l.Location.SourceTree.FilePath, l.Location.SourceSpan)
            let refNum = refs |> Seq.map (fun r -> r.Locations |> Seq.length) |> Seq.fold (+) 0
            let title = sprintf "%d Reference(s)" refNum

            let command =
                { Title = title
                  Command = "csharp.showReferences"
                  // TODO: we really want to pass some more info to the client
                  Arguments = None }

            return { p with Command = Some command } |> success
    }
