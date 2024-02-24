namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module InlayHint =
    let private toInlayHint (semanticModel: SemanticModel) (lines: TextLineCollection) (node: SyntaxNode): InlayHint option =
        let validateType (ty: ITypeSymbol) =
            if isNull ty || ty :? IErrorTypeSymbol || ty.Name = "var" then
                None
            else
                Some ty
        let typeDisplayStyle = SymbolDisplayFormat(
            genericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
            miscellaneousOptions = (SymbolDisplayMiscellaneousOptions.AllowDefaultLiteral ||| SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier ||| SymbolDisplayMiscellaneousOptions.UseSpecialTypes))
        let toTypeInlayHint (pos: int) (ty: ITypeSymbol): InlayHint =
            { Position = pos |> lines.GetLinePosition |> Position.fromLinePosition
              Label = InlayHintLabel.String (": " + ty.ToDisplayString(typeDisplayStyle))
              Kind = Some InlayHintKind.Type
              TextEdits = None
              Tooltip = None
              PaddingLeft = Some false
              PaddingRight = Some false
              Data = None
            }

        let validateParameter (arg: SyntaxNode) (par: IParameterSymbol) =
            match arg.Parent with
            // Don't show hint for indexer
            | :? BracketedArgumentListSyntax -> None
            // Don't show hint if parameter name is empty
            | _ when String.IsNullOrEmpty(par.Name) -> None
            // Don't show hint if argument matches parameter name
            | _ when String.Equals(arg.GetText().ToString(), par.Name, StringComparison.CurrentCultureIgnoreCase) -> None
            | _ -> Some par
        let toParameterInlayHint (pos: int) (par: IParameterSymbol): InlayHint =
            { Position = pos |> lines.GetLinePosition |> Position.fromLinePosition
              Label = InlayHintLabel.String (par.Name + ":")
              Kind = Some InlayHintKind.Parameter
              TextEdits = None
              Tooltip = None
              PaddingLeft = Some false
              PaddingRight = Some true
              Data = None
            }

        // It's a rewrite of https://github.com/dotnet/roslyn/blob/main/src/Features/CSharp/Portable/InlineHints/CSharpInlineTypeHintsService.cs &
        // https://github.com/dotnet/roslyn/blob/main/src/Features/CSharp/Portable/InlineHints/CSharpInlineParameterNameHintsService.cs.
        // If Roslyn exposes the classes, then we can just do a type convert.
        match node with
        | :? VariableDeclarationSyntax as var when
            var.Type.IsVar && var.Variables.Count = 1 && not var.Variables[0].Identifier.IsMissing
            ->
            semanticModel.GetTypeInfo(var.Type).Type
            |> validateType
            |> Option.map (toTypeInlayHint var.Variables[0].Identifier.Span.End)
        // We handle individual variables of ParenthesizedVariableDesignationSyntax separately.
        // For example, in `var (x, y) = (0, "")`, we should `int` for `x` and `string` for `y`.
        // It's redundant to show `(int, string)` for `var`
        | :? DeclarationExpressionSyntax as dec when
            dec.Type.IsVar && not (dec.Designation :? ParenthesizedVariableDesignationSyntax)
            ->
            semanticModel.GetTypeInfo(dec.Type).Type
            |> validateType
            |> Option.map (toTypeInlayHint dec.Designation.Span.End)
        | :? SingleVariableDesignationSyntax as var when
            not (var.Parent :? DeclarationPatternSyntax) && not (var.Parent :? DeclarationExpressionSyntax)
            ->
            semanticModel.GetDeclaredSymbol(var)
            |> fun sym ->
                match sym with
                | :? ILocalSymbol as local -> Some local
                | _ -> None
            |> Option.map (fun local -> local.Type)
            |> Option.bind validateType
            |> Option.map (toTypeInlayHint var.Identifier.Span.End)
        | :? ForEachStatementSyntax as forEach when
            forEach.Type.IsVar
            ->
            semanticModel.GetForEachStatementInfo(forEach).ElementType
            |> validateType
            |> Option.map (toTypeInlayHint forEach.Identifier.Span.End)
        | :? ParameterSyntax as parameterNode when
            isNull parameterNode.Type
            ->
            let parameter = semanticModel.GetDeclaredSymbol(parameterNode)
            parameter
            |> fun parameter ->
                match parameter.ContainingSymbol with
                | :? IMethodSymbol as methSym when methSym.MethodKind = MethodKind.AnonymousFunction -> Some parameter
                | _ -> None
            |> Option.map (fun parameter -> parameter.Type)
            |> Option.bind validateType
            |> Option.map (toTypeInlayHint parameterNode.Identifier.Span.End)
        | :? ImplicitObjectCreationExpressionSyntax as implicitNew
            ->
            semanticModel.GetTypeInfo(implicitNew).Type
            |> validateType
            |> Option.map (toTypeInlayHint implicitNew.NewKeyword.Span.End)

        | :? ArgumentSyntax as argument when
            isNull argument.NameColon
            ->
            argument
            |> getParameterForArgumentSyntax semanticModel
            |> Option.bind (validateParameter node)
            |> Option.map (toParameterInlayHint argument.Span.Start)
        | :? AttributeArgumentSyntax as argument when
            isNull argument.NameEquals && isNull argument.NameColon
            ->
            argument
            |> getParameterForAttributeArgumentSyntax semanticModel
            |> Option.bind (validateParameter node)
            |> Option.map (toParameterInlayHint argument.Span.Start)

        | _ -> None

    let provider (clientCapabilities: ClientCapabilities option) : InlayHintOptions option =
        Some { ResolveProvider = Some false }

    let handle (scope: ServerRequestScope) (inlayHintParams: InlayHintParams): AsyncLspResult<InlayHint [] option> = async {
        match scope.GetUserDocumentForUri inlayHintParams.TextDocument.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask
            let! root = doc.GetSyntaxRootAsync() |> Async.AwaitTask
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let textSpan =
                inlayHintParams.Range
                |> Range.toLinePositionSpan sourceText.Lines
                |> sourceText.Lines.GetTextSpan

            let inlayHints =
                root.DescendantNodes(textSpan, fun node -> node.Span.IntersectsWith(textSpan))
                |> Seq.map (toInlayHint semanticModel sourceText.Lines)
                |> Seq.filter Option.isSome
                |> Seq.map Option.get

            return inlayHints |> Seq.toArray |> Some |> LspResult.success
    }
