namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

type private DocumentSymbolCollectorForCodeLens(semanticModel: SemanticModel) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols = []

    let collect (node: SyntaxNode) (nameSpan: TextSpan) =
        match semanticModel.GetDeclaredSymbol(node) |> Option.ofObj with
        | Some symbol -> collectedSymbols <- (symbol, nameSpan) :: collectedSymbols
        | _ -> ()

    member __.GetSymbols() =
        collectedSymbols |> List.rev |> Array.ofList

    override __.VisitEnumDeclaration(node) =
        collect node node.Identifier.Span
        base.VisitEnumDeclaration(node)

    override __.VisitEnumMemberDeclaration(node) = collect node node.Identifier.Span

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

    override __.VisitDelegateDeclaration(node) = collect node node.Identifier.Span

    override __.VisitConstructorDeclaration(node) = collect node node.Identifier.Span

    override __.VisitDestructorDeclaration(node) = collect node node.Identifier.Span

    override __.VisitOperatorDeclaration(node) = collect node node.OperatorToken.Span

    override __.VisitIndexerDeclaration(node) = collect node node.ThisKeyword.Span

    override __.VisitConversionOperatorDeclaration(node) = collect node node.Type.Span

    override __.VisitMethodDeclaration(node) = collect node node.Identifier.Span

    override __.VisitPropertyDeclaration(node) = collect node node.Identifier.Span

    override __.VisitVariableDeclarator(node) =
        let grandparent =
            node.Parent
            |> Option.ofObj
            |> Option.bind (fun node -> node.Parent |> Option.ofObj)
        // Only show field variables and ignore local variables
        if grandparent.IsSome && grandparent.Value :? FieldDeclarationSyntax then
            collect node node.Identifier.Span

    override __.VisitEventDeclaration(node) = collect node node.Identifier.Span

[<RequireQualifiedAccess>]
module CodeLens =
    type CodeLensData =
        { DocumentUri: string
          Position: Position }

        static member Default =
            { DocumentUri = ""
              Position = { Line = 0u; Character = 0u } }

    let provider (clientCapabilities: ClientCapabilities) : CodeLensOptions option =
        Some
            { ResolveProvider = Some true
              WorkDoneProgress = None }

    let handle (context: ServerRequestContext) (p: CodeLensParams) : AsyncLspResult<CodeLens[] option> = async {
        let docForUri =
            p.TextDocument.Uri |> workspaceDocument context.Workspace AnyDocument

        match docForUri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollectorForCodeLens(semanticModel)
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask
            collector.Visit(root)

            let makeCodeLens (_symbol: ISymbol, nameSpan: TextSpan) : CodeLens =
                let start = nameSpan.Start |> docText.Lines.GetLinePosition

                let lensData: CodeLensData =
                    { DocumentUri = p.TextDocument.Uri
                      Position = start |> Position.fromLinePosition }

                { Range = nameSpan |> Range.fromTextSpan docText.Lines
                  Command = None
                  Data = lensData |> serialize |> Some }

            let codeLens = collector.GetSymbols() |> Seq.map makeCodeLens

            return codeLens |> Array.ofSeq |> Some |> LspResult.success
    }

    let resolve (context: ServerRequestContext) (p: CodeLens) : AsyncLspResult<CodeLens> = async {
        let! ct = Async.CancellationToken

        let lensData: CodeLensData =
            p.Data
            |> Option.map _.ToObject<CodeLensData>()
            |> Option.bind Option.ofObj
            |> Option.defaultValue CodeLensData.Default

        match! context.FindSymbol lensData.DocumentUri lensData.Position with
        | None -> return p |> LspResult.success
        | Some symbol ->
            let! refs =
                SymbolFinder.FindReferencesAsync(symbol, context.Solution, cancellationToken = ct)
                |> Async.AwaitTask

            // FIXME: refNum is wrong. There are lots of false positive even if we distinct locations by
            // (l.SourceTree.FilePath, l.SourceSpan)
            let refNum =
                refs
                |> Seq.collect _.Locations
                |> Seq.map _.Location
                |> Seq.distinctBy (fun l -> (l.GetMappedLineSpan().Path, l.SourceSpan))
                |> Seq.length

            let title = sprintf "%d Reference(s)" refNum

            let arg: ReferenceParams =
                { TextDocument = { Uri = lensData.DocumentUri }
                  Position = lensData.Position
                  WorkDoneToken = None
                  PartialResultToken = None
                  Context = { IncludeDeclaration = true } }

            let command =
                { Title = title
                  Command = "textDocument/references"
                  Arguments = Some [| arg |> serialize |] }

            return { p with Command = Some command } |> LspResult.success
    }
