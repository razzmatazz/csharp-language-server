namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module InlayHint =
    // TODO: Do we keep it now or refactor it to use reflection?
    let private getBestOrAllSymbols (info: SymbolInfo) =
        let best = if isNull info.Symbol then None else Some([| info.Symbol |])

        let all =
            if info.CandidateSymbols.IsEmpty then
                None
            else
                Some(info.CandidateSymbols |> Array.ofSeq)

        best |> Option.orElse all |> Option.defaultValue Array.empty

    // rewrite of https://github.com/dotnet/roslyn/blob/main/src/Workspaces/SharedUtilitiesAndExtensions/Compiler/CSharp/Extensions/ArgumentSyntaxExtensions.cs
    let private getParameterForArgumentSyntax
        (semanticModel: SemanticModel)
        (argument: ArgumentSyntax)
        : IParameterSymbol option =
        match argument.Parent with
        | :? BaseArgumentListSyntax as argumentList when not (isNull argumentList.Parent) ->
            let argumentListParent = argumentList.Parent |> nonNull "argumentList.Parent"
            let symbols = semanticModel.GetSymbolInfo(argumentListParent) |> getBestOrAllSymbols

            match symbols with
            | [| symbol |] ->
                let parameters =
                    match symbol with
                    | :? IMethodSymbol as m -> m.Parameters
                    | :? IPropertySymbol as nt -> nt.Parameters
                    | _ -> ImmutableArray<IParameterSymbol>.Empty

                let namedParameter =
                    argument.NameColon
                    |> Option.ofObj
                    |> Option.bind (fun anc -> if anc.IsMissing then None else Some anc)
                    |> Option.bind (fun anc ->
                        parameters |> Seq.tryFind (fun p -> p.Name = anc.Name.Identifier.ValueText))

                let positionalParameter =
                    match argumentList.Arguments.IndexOf(argument) with
                    | index when 0 <= index && index < parameters.Length ->
                        let parameter = parameters[index]

                        if
                            argument.RefOrOutKeyword.Kind() = SyntaxKind.OutKeyword
                            && parameter.RefKind <> RefKind.Out
                            || argument.RefOrOutKeyword.Kind() = SyntaxKind.RefKeyword
                               && parameter.RefKind <> RefKind.Ref
                        then
                            None
                        else
                            Some parameter
                    | _ -> None

                namedParameter |> Option.orElse positionalParameter
            | _ -> None
        | _ -> None

    // rewrite of https://github.com/dotnet/roslyn/blob/main/src/Workspaces/SharedUtilitiesAndExtensions/Compiler/CSharp/Extensions/AttributeArgumentSyntaxExtensions.cs
    let private getParameterForAttributeArgumentSyntax
        (semanticModel: SemanticModel)
        (argument: AttributeArgumentSyntax)
        : IParameterSymbol option =
        match argument.Parent with
        | :? AttributeArgumentListSyntax as argumentList when not (isNull argument.NameEquals) ->
            match argumentList.Parent with
            | :? AttributeSyntax as invocable ->
                let symbols = semanticModel.GetSymbolInfo(invocable) |> getBestOrAllSymbols

                match symbols with
                | [| symbol |] ->
                    let parameters =
                        match symbol with
                        | :? IMethodSymbol as m -> m.Parameters
                        | :? IPropertySymbol as nt -> nt.Parameters
                        | _ -> ImmutableArray<IParameterSymbol>.Empty

                    let namedParameter =
                        argument.NameColon
                        |> Option.ofObj
                        |> Option.bind (fun anc -> if anc.IsMissing then None else Some anc)
                        |> Option.bind (fun anc ->
                            parameters |> Seq.tryFind (fun p -> p.Name = anc.Name.Identifier.ValueText))

                    let positionalParameter =
                        match argumentList.Arguments.IndexOf(argument) with
                        | index when 0 <= index && index < parameters.Length -> Some parameters[index]
                        | _ -> None

                    namedParameter |> Option.orElse positionalParameter
                | _ -> None
            | _ -> None
        | _ -> None

    let private toInlayHint
        (semanticModel: SemanticModel)
        (lines: TextLineCollection)
        (node: SyntaxNode)
        : InlayHint option =
        let validateType (ty: ITypeSymbol | null) =
            match ty |> Option.ofObj with
            | None -> None
            | Some ty ->
                if ty :? IErrorTypeSymbol || ty.Name = "var" then
                    None
                else
                    Some ty

        let typeDisplayStyle =
            SymbolDisplayFormat(
                genericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
                miscellaneousOptions =
                    (SymbolDisplayMiscellaneousOptions.AllowDefaultLiteral
                     ||| SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
                     ||| SymbolDisplayMiscellaneousOptions.UseSpecialTypes)
            )

        let toTypeInlayHint (pos: int) (ty: ITypeSymbol) : InlayHint =
            { Position = pos |> lines.GetLinePosition |> Position.fromLinePosition
              Label = U2.C1(": " + SymbolName.fromSymbol typeDisplayStyle ty)
              Kind = Some InlayHintKind.Type
              TextEdits = None
              Tooltip = None
              PaddingLeft = Some false
              PaddingRight = Some false
              Data = None }

        let validateParameter (arg: SyntaxNode) (par: IParameterSymbol) =
            match arg.Parent with
            // Don't show hint for indexer
            | :? BracketedArgumentListSyntax -> None
            // Don't show hint if parameter name is empty
            | _ when String.IsNullOrEmpty(par.Name) -> None
            // Don't show hint if argument matches parameter name
            | _ when String.Equals(arg.GetText().ToString(), par.Name, StringComparison.CurrentCultureIgnoreCase) ->
                None
            | _ -> Some par

        let toParameterInlayHint (pos: int) (par: IParameterSymbol) : InlayHint =
            { Position = pos |> lines.GetLinePosition |> Position.fromLinePosition
              Label = U2.C1(par.Name + ":")
              Kind = Some InlayHintKind.Parameter
              TextEdits = None
              Tooltip = None
              PaddingLeft = Some false
              PaddingRight = Some true
              Data = None }

        // It's a rewrite of https://github.com/dotnet/roslyn/blob/main/src/Features/CSharp/Portable/InlineHints/CSharpInlineTypeHintsService.cs &
        // https://github.com/dotnet/roslyn/blob/main/src/Features/CSharp/Portable/InlineHints/CSharpInlineParameterNameHintsService.cs.
        // If Roslyn exposes the classes, then we can just do a type convert.
        // TODO: Support the configuration whether or not to show some kinds of inlay hints.
        match node with
        | :? VariableDeclarationSyntax as var when
            var.Type.IsVar
            && var.Variables.Count = 1
            && not var.Variables[0].Identifier.IsMissing
            ->
            semanticModel.GetTypeInfo(var.Type).Type
            |> validateType
            |> Option.map (toTypeInlayHint var.Variables[0].Identifier.Span.End)
        // We handle individual variables of ParenthesizedVariableDesignationSyntax separately.
        // For example, in `var (x, y) = (0, "")`, we should `int` for `x` and `string` for `y`.
        // It's redundant to show `(int, string)` for `var`
        | :? DeclarationExpressionSyntax as dec when
            dec.Type.IsVar
            && not (dec.Designation :? ParenthesizedVariableDesignationSyntax)
            ->
            semanticModel.GetTypeInfo(dec.Type).Type
            |> validateType
            |> Option.map (toTypeInlayHint dec.Designation.Span.End)
        | :? SingleVariableDesignationSyntax as var when
            not (var.Parent :? DeclarationPatternSyntax)
            && not (var.Parent :? DeclarationExpressionSyntax)
            ->
            semanticModel.GetDeclaredSymbol(var)
            |> fun sym ->
                match sym with
                | :? ILocalSymbol as local -> Some local
                | _ -> None
            |> Option.map (fun local -> local.Type)
            |> Option.bind validateType
            |> Option.map (toTypeInlayHint var.Identifier.Span.End)
        | :? ForEachStatementSyntax as forEach when forEach.Type.IsVar ->
            semanticModel.GetForEachStatementInfo(forEach).ElementType
            |> validateType
            |> Option.map (toTypeInlayHint forEach.Identifier.Span.End)
        | :? ParameterSyntax as parameterNode when isNull parameterNode.Type ->
            let parameter = semanticModel.GetDeclaredSymbol(parameterNode)

            parameter
            |> fun parameter ->
                match parameter.ContainingSymbol with
                | :? IMethodSymbol as methSym when methSym.MethodKind = MethodKind.AnonymousFunction -> Some parameter
                | _ -> None
            |> Option.map (fun parameter -> parameter.Type)
            |> Option.bind validateType
            |> Option.map (toTypeInlayHint parameterNode.Identifier.Span.End)
        | :? ImplicitObjectCreationExpressionSyntax as implicitNew ->
            semanticModel.GetTypeInfo(implicitNew).Type
            |> validateType
            |> Option.map (toTypeInlayHint implicitNew.NewKeyword.Span.End)

        | :? ArgumentSyntax as argument when isNull argument.NameColon ->
            argument
            |> getParameterForArgumentSyntax semanticModel
            |> Option.bind (validateParameter node)
            |> Option.map (toParameterInlayHint argument.Span.Start)
        | :? AttributeArgumentSyntax as argument when isNull argument.NameEquals && isNull argument.NameColon ->
            argument
            |> getParameterForAttributeArgumentSyntax semanticModel
            |> Option.bind (validateParameter node)
            |> Option.map (toParameterInlayHint argument.Span.Start)

        | _ -> None

    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.InlayHint
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, InlayHintOptions, InlayHintRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            let inlayHintOptions: InlayHintOptions =
                { ResolveProvider = Some false
                  WorkDoneProgress = None }

            Some(U3.C2 inlayHintOptions)

    let registration (_settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: InlayHintRegistrationOptions =
                { ResolveProvider = Some false
                  DocumentSelector = documentSelectorForCSharpDocuments |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/inlayHint"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: InlayHintParams) : AsyncLspResult<InlayHint[] option> = async {
        let wf, docForUri =
            p.TextDocument.Uri |> workspaceDocument context.Workspace UserDocument

        match docForUri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! root = doc.GetSyntaxRootAsync(ct) |> Async.AwaitTask
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let textSpan = Range.toTextSpan sourceText.Lines p.Range

            let inlayHints =
                root.DescendantNodes(textSpan, fun node -> node.Span.IntersectsWith(textSpan))
                |> Seq.choose (toInlayHint semanticModel sourceText.Lines)

            return inlayHints |> Seq.toArray |> Some |> LspResult.success
    }

    let resolve (_context: ServerRequestContext) (_p: InlayHint) : AsyncLspResult<InlayHint> =
        LspResult.notImplemented<InlayHint> |> async.Return
