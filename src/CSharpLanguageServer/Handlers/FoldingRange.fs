namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

[<RequireQualifiedAccess>]
module FoldingRange =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.FoldingRange
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, FoldingRangeOptions, FoldingRangeRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: FoldingRangeRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  WorkDoneProgress = None
                  Id = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/foldingRange"
                  RegisterOptions = registerOptions |> serialize |> Some }

    /// Convert a TextSpan to a FoldingRange, optionally omitting character offsets (for lineFoldingOnly clients).
    let private makeFoldingRange
        (lines: TextLineCollection)
        (span: TextSpan)
        (kind: FoldingRangeKind option)
        (lineFoldingOnly: bool)
        : FoldingRange option =
        let lineSpan = lines.GetLinePositionSpan(span)
        let startLine = uint32 lineSpan.Start.Line
        let endLine = uint32 lineSpan.End.Line

        // Only emit ranges that actually span multiple lines
        if startLine >= endLine then
            None
        else
            let startChar =
                if lineFoldingOnly then
                    None
                else
                    Some(uint32 lineSpan.Start.Character)

            let endChar =
                if lineFoldingOnly then
                    None
                else
                    Some(uint32 lineSpan.End.Character)

            Some
                { StartLine = startLine
                  StartCharacter = startChar
                  EndLine = endLine
                  EndCharacter = endChar
                  Kind = kind
                  CollapsedText = None }

    type private FoldingRangeCollector(lines: TextLineCollection, lineFoldingOnly: bool) =
        inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Trivia)

        let ranges = ResizeArray<FoldingRange>()

        let add span kind =
            makeFoldingRange lines span kind lineFoldingOnly |> Option.iter ranges.Add

        member __.Ranges = ranges |> Seq.toArray

        // ── Type declarations ──────────────────────────────────────────────

        override this.VisitClassDeclaration(node: ClassDeclarationSyntax) =
            add node.Span None
            base.VisitClassDeclaration(node)

        override this.VisitStructDeclaration(node: StructDeclarationSyntax) =
            add node.Span None
            base.VisitStructDeclaration(node)

        override this.VisitInterfaceDeclaration(node: InterfaceDeclarationSyntax) =
            add node.Span None
            base.VisitInterfaceDeclaration(node)

        override this.VisitEnumDeclaration(node: EnumDeclarationSyntax) =
            add node.Span None
            base.VisitEnumDeclaration(node)

        override this.VisitRecordDeclaration(node: RecordDeclarationSyntax) =
            add node.Span None
            base.VisitRecordDeclaration(node)

        // ── Namespace declarations ─────────────────────────────────────────

        override this.VisitNamespaceDeclaration(node: NamespaceDeclarationSyntax) =
            add node.Span None
            // Fold the using group inside the namespace if there are multiple usings
            let usings = node.Usings

            if usings.Count > 1 then
                let span =
                    TextSpan.FromBounds(usings.[0].Span.Start, usings.[usings.Count - 1].Span.End)

                add span (Some FoldingRangeKind.Imports)

            base.VisitNamespaceDeclaration(node)

        // FileScopedNamespaceDeclarationSyntax intentionally not folded
        // (it spans the entire file and would be confusing)

        // ── Member declarations ────────────────────────────────────────────

        override this.VisitMethodDeclaration(node: MethodDeclarationSyntax) =
            add node.Span None
            base.VisitMethodDeclaration(node)

        override this.VisitConstructorDeclaration(node: ConstructorDeclarationSyntax) =
            add node.Span None
            base.VisitConstructorDeclaration(node)

        override this.VisitDestructorDeclaration(node: DestructorDeclarationSyntax) =
            add node.Span None
            base.VisitDestructorDeclaration(node)

        override this.VisitPropertyDeclaration(node: PropertyDeclarationSyntax) =
            // Only fold properties that have an accessor list (i.e. { get; set; } or a body)
            if not (isNull node.AccessorList) then
                add node.Span None

            base.VisitPropertyDeclaration(node)

        override this.VisitIndexerDeclaration(node: IndexerDeclarationSyntax) =
            add node.Span None
            base.VisitIndexerDeclaration(node)

        override this.VisitOperatorDeclaration(node: OperatorDeclarationSyntax) =
            add node.Span None
            base.VisitOperatorDeclaration(node)

        override this.VisitConversionOperatorDeclaration(node: ConversionOperatorDeclarationSyntax) =
            add node.Span None
            base.VisitConversionOperatorDeclaration(node)

        override this.VisitEventDeclaration(node: EventDeclarationSyntax) =
            add node.Span None
            base.VisitEventDeclaration(node)

        // ── Using directives — folded as a group ───────────────────────────

        override this.VisitCompilationUnit(node: CompilationUnitSyntax) =
            let usings = node.Usings

            if usings.Count > 1 then
                let span =
                    TextSpan.FromBounds(usings.[0].Span.Start, usings.[usings.Count - 1].Span.End)

                add span (Some FoldingRangeKind.Imports)

            base.VisitCompilationUnit(node)

        // ── Trivia: block comments ─────────────────────────────────────────

        override this.VisitTrivia(trivia: SyntaxTrivia) =
            match trivia.Kind() with
            | SyntaxKind.MultiLineCommentTrivia
            | SyntaxKind.MultiLineDocumentationCommentTrivia -> add trivia.Span (Some FoldingRangeKind.Comment)
            | _ -> ()

            base.VisitTrivia(trivia)

    let handle (context: RequestContext) (p: FoldingRangeParams) : AsyncLspResult<FoldingRange array option> = async {
        let lineFoldingOnly =
            context.ClientCapabilities.TextDocument
            |> Option.bind _.FoldingRange
            |> Option.bind _.LineFoldingOnly
            |> Option.defaultValue false

        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        let docMaybe =
            wf |> Option.bind (workspaceFolderDocument AnyDocument p.TextDocument.Uri)

        match docMaybe with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
            let! root = syntaxTree.GetRootAsync(ct) |> Async.AwaitTask

            let collector = FoldingRangeCollector(sourceText.Lines, lineFoldingOnly)
            collector.Visit(root)

            // Also collect #region ranges that span across tokens (they appear in the root's
            // descendant trivia when not found via sibling-trivia scan above)
            let regionRanges =
                let regionStarts = ResizeArray<SyntaxTrivia>()
                let mutable result = ResizeArray<FoldingRange>()

                for trivia in root.DescendantTrivia() do
                    match trivia.Kind() with
                    | SyntaxKind.RegionDirectiveTrivia -> regionStarts.Add(trivia)
                    | SyntaxKind.EndRegionDirectiveTrivia ->
                        if regionStarts.Count > 0 then
                            let start = regionStarts.[regionStarts.Count - 1]
                            regionStarts.RemoveAt(regionStarts.Count - 1)
                            let span = TextSpan.FromBounds(start.Span.Start, trivia.Span.End)

                            makeFoldingRange sourceText.Lines span (Some FoldingRangeKind.Region) lineFoldingOnly
                            |> Option.iter result.Add
                    | _ -> ()

                result

            let allRanges =
                collector.Ranges
                |> Array.append (regionRanges |> Seq.toArray)
                |> Array.distinctBy (fun r -> r.StartLine, r.EndLine)
                |> Array.sortBy (fun r -> r.StartLine)

            return allRanges |> Some |> LspResult.success
    }
