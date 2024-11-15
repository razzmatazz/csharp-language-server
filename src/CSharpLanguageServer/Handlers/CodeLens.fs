namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Newtonsoft.Json.Linq

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

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

    override __.VisitConstructorDeclaration(node) =
        collect node node.Identifier.Span

    override __.VisitDestructorDeclaration(node) =
        collect node node.Identifier.Span

    override __.VisitOperatorDeclaration(node) =
        collect node node.OperatorToken.Span

    override __.VisitIndexerDeclaration(node) =
        collect node node.ThisKeyword.Span

    override __.VisitConversionOperatorDeclaration(node) =
        collect node node.Type.Span

    override __.VisitMethodDeclaration(node) =
        collect node node.Identifier.Span

    override __.VisitPropertyDeclaration(node) =
        collect node node.Identifier.Span

    override __.VisitVariableDeclarator(node) =
        let grandparent =
            node.Parent
            |> Option.ofObj
            |> Option.bind (fun node -> node.Parent |> Option.ofObj)
        // Only show field variables and ignore local variables
        if grandparent.IsSome && grandparent.Value :? FieldDeclarationSyntax then
            collect node node.Identifier.Span

    override __.VisitEventDeclaration(node) =
        collect node node.Identifier.Span

[<RequireQualifiedAccess>]
module CodeLens =
    type CodeLensData =
        { DocumentUri: string
          Position: Position }
        static member Default =
            { DocumentUri = ""
              Position = { Line = 0u; Character = 0u } }

    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.CodeLens)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : CodeLensOptions option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some { ResolveProvider = Some true
                          WorkDoneProgress = None }

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: CodeLensRegistrationOptions =
                { ResolveProvider = Some true
                  WorkDoneProgress = None
                  DocumentSelector = Some defaultDocumentSelector }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/codeLens"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: CodeLensParams): AsyncLspResult<CodeLens[] option> = async {
        let docMaybe = context.GetDocument p.TextDocument.Uri
        match docMaybe with
        | None -> return None |> success
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
                  Data = lensData |> JToken.FromObject |> Some }

            let codeLens = collector.GetSymbols() |> Seq.map makeCodeLens

            return codeLens |> Array.ofSeq |> Some |> success
    }

    let resolve (context: ServerRequestContext)
                (p: CodeLens)
            : AsyncLspResult<CodeLens> = async {
        let lensData =
            p.Data
            |> Option.map (fun t -> t.ToObject<CodeLensData>())
            |> Option.defaultValue CodeLensData.Default

        match! context.FindSymbol lensData.DocumentUri lensData.Position with
        | None -> return p |> success
        | Some symbol ->
            let! locations = context.FindReferences symbol false
            // FIXME: refNum is wrong. There are lots of false positive even if we distinct locations by
            // (l.SourceTree.FilePath, l.SourceSpan)
            let refNum =
                locations
                |> Seq.distinctBy (fun l -> (l.SourceTree.FilePath, l.SourceSpan))
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

            return { p with Command = Some command } |> success
    }
