namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable
open System.Text.RegularExpressions

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
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

    // A parameter name matching this pattern (`arg0`, `val1`, `path2`, …) ends in a numeric
    // suffix and carries essentially no information beyond its argument's position -- see
    // `hasUninformativeParameterName` below. Module-level (rather than local to `toInlayHint`)
    // so it's compiled once, not once per syntax node visited per request.
    let private numberedSuffixParameterName = Regex(@"^\w*?\d+$", RegexOptions.Compiled)

    // Splits a PascalCase/camelCase identifier into its constituent words (e.g.
    // `settingChangeCondition` -> `setting`, `Change`, `Condition`), so the last word can be
    // compared across identifiers using different casing conventions (a local variable:
    // camelCase; a type name: PascalCase). Module-level for the same reason as
    // numberedSuffixParameterName above.
    let private camelCaseWordBoundary = Regex(@"(?<=[a-z0-9])(?=[A-Z])", RegexOptions.Compiled)

    let private lastWord (identifier: string) : string =
        let words = camelCaseWordBoundary.Split(identifier)
        if words.Length = 0 then identifier else words[words.Length - 1]

    // True when `identifier` already "echoes" `typeName` -- either a full case-insensitive match
    // (`resourceStatus` vs. `ResourceStatus`, `messageDispatcherAsync` vs.
    // `MessageDispatcherAsync`), or their last words match (`settingChangeCondition` vs.
    // `ResourceCondition`, both ending in "Condition"). In either case, a hint would just restate
    // what the identifier's own name already conveys. See plans/inlay-hint-reduction.md for the
    // real-world evidence behind this heuristic.
    let private identifierEchoesTypeName (identifier: string) (typeName: string) : bool =
        String.Equals(identifier, typeName, StringComparison.CurrentCultureIgnoreCase)
        || String.Equals(lastWord identifier, lastWord typeName, StringComparison.CurrentCultureIgnoreCase)

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

        // Returns true when `initializer` is a generic invocation (optionally on a member access,
        // e.g. `Enum.Parse<DayOfWeek>(...)`, or unqualified, e.g. a local `CreateLocal<T>()`)
        // whose explicit type-argument list already contains a type symbol-equal to `ty` -- in
        // that case a `var` type hint would be purely redundant, since the type is already
        // spelled out one step to the left in the very same expression (`Enum.Parse<`,
        // `JsonSerializer.Deserialize<`, `Activator.CreateInstance<`, a local factory method,
        // ...). Parenthesized expressions are unwrapped. Compares by symbol (not just by name),
        // so it correctly doesn't fire when the invocation's return type differs from its type
        // argument (e.g. a hypothetical `Describe<Widget>(...)` returning `string`).
        let rec isTypeSpelledOutInGenericInvocation (initializer: ExpressionSyntax) (ty: ITypeSymbol) : bool =
            match initializer with
            | :? ParenthesizedExpressionSyntax as paren -> isTypeSpelledOutInGenericInvocation paren.Expression ty
            | :? InvocationExpressionSyntax as invocation ->
                let genericName =
                    match invocation.Expression with
                    | :? GenericNameSyntax as generic -> Some generic
                    | :? MemberAccessExpressionSyntax as memberAccess ->
                        match memberAccess.Name with
                        | :? GenericNameSyntax as generic -> Some generic
                        | _ -> None
                    | _ -> None

                genericName
                |> Option.map (fun generic ->
                    generic.TypeArgumentList.Arguments
                    |> Seq.exists (fun typeArgSyntax ->
                        match semanticModel.GetTypeInfo(typeArgSyntax).Type |> Option.ofObj with
                        | Some typeArgSymbol -> SymbolEqualityComparer.Default.Equals(typeArgSymbol, ty)
                        | None -> false))
                |> Option.defaultValue false
            | _ -> false

        // Returns the significant (trivia-free) name an argument expression is "known by", so it
        // can be compared against a parameter name to detect a redundant hint:
        // - a bare identifier (`resourceName`) contributes its own text
        // - a qualified member access (`this.ResourceName`, `foo.Bar.ResourceName`) contributes
        //   just its last segment, since that's the name a reader actually associates the value
        //   with
        // - a parenthesized expression is unwrapped
        // - anything else falls back to its own (trivia-free) text via ToString(), which is what
        //   the previous, buggy `arg.GetText()` (a trivia-inclusive, ToFullString()-equivalent
        //   call) was trying, but failing, to do
        let rec significantArgumentName (expr: ExpressionSyntax) : string =
            match expr with
            | :? IdentifierNameSyntax as ident -> ident.Identifier.ValueText
            | :? MemberAccessExpressionSyntax as memberAccess -> memberAccess.Name.Identifier.ValueText
            | :? ParenthesizedExpressionSyntax as paren -> significantArgumentName paren.Expression
            | _ -> expr.ToString()

        // A parameter name that's very short, or ends in a numeric suffix, carries essentially
        // no information beyond its argument's position (`d`, `s`, `val1`, `arg0`, `path2`, the
        // real .NET BCL parameter names of e.g. `Guid`'s constructor, `Math.Min`/`Math.Max`,
        // `Path.Combine`, and composite-format-string overloads like `string.Format`) -- a hint
        // for it can't help a reader, only add clutter. See plans/inlay-hint-reduction.md for the
        // real-world evidence behind this heuristic. Deliberately narrow/mechanical: it doesn't
        // try to catch every opaque BCL name (e.g. `Math.Round`'s `decimals`/`mode`), only the
        // shapes confirmed safe against genuinely-disambiguating counter-examples like
        // `string.Substring(startIndex, length)` and `Stream.Write(buffer, offset, count)`.
        let hasUninformativeParameterName (name: string) =
            name.Length <= 2 || numberedSuffixParameterName.IsMatch(name)

        // A `CancellationToken` argument is a conventional, non-essential "pass-through"
        // parameter on async APIs, so its presence shouldn't disqualify the single-lambda-argument
        // rule below -- e.g. `WhereAsync(x => x.IsActive, cancellationToken)` should be treated
        // the same as `Where(x => x.IsActive)`. Uses the argument's converted type (rather than
        // its natural type) so a `default`/`default(CancellationToken)` argument is still
        // recognized. Matched by name only, not a full compilation-wide symbol lookup, since this
        // is meant to be a cheap, conventional signal rather than a rigorous type check.
        let isCancellationTokenArgument (argument: ArgumentSyntax) : bool =
            let typeInfo = semanticModel.GetTypeInfo(argument.Expression)

            [ typeInfo.ConvertedType; typeInfo.Type ]
            |> List.tryPick Option.ofObj
            |> Option.map (fun ty -> ty.Name = "CancellationToken")
            |> Option.defaultValue false

        let validateParameter (arg: SyntaxNode) (argExpr: ExpressionSyntax) (par: IParameterSymbol) =
            match arg.Parent with
            // Don't show hint for indexer
            | :? BracketedArgumentListSyntax -> None
            // Don't show hint if parameter name is empty
            | _ when String.IsNullOrEmpty(par.Name) -> None
            // Don't show hint if the parameter name itself is too short/generic to be informative
            | _ when hasUninformativeParameterName par.Name -> None
            // Don't show hint if the parameter's own name is just a (decapitalized) echo of its
            // own declared type (e.g. a `MessageDispatcherAsync messageDispatcherAsync`
            // parameter) -- the parameter-hint analogue of the identifier-echoes-type-name rule
            // applied to type hints below
            | _ when identifierEchoesTypeName par.Name par.Type.Name -> None
            // Don't show hint for the sole (non-CancellationToken) argument of a call (the
            // enclosing method symbol is already known to have resolved unambiguously -- see
            // getParameterForArgumentSyntax above) when that argument is a lambda -- the method's
            // own name already conveys the lambda's role (`Where(predicate)`, `Select(selector)`,
            // NHibernate-style `Fetch(relatedObjectSelector)`/`ThenFetch(...)`, ...), so a
            // parameter-name hint here is redundant no matter how generic or specific that name
            // is. Deliberately scoped to lambda expressions only (not method-group arguments) and
            // to single-(effective-)argument calls only -- multiple lambda parameters in one call
            // aren't each automatically self-describing from the method name alone.
            | :? ArgumentListSyntax as argList when
                (argExpr :? LambdaExpressionSyntax)
                && (argList.Arguments |> Seq.filter (isCancellationTokenArgument >> not) |> Seq.length) = 1
                ->
                None
            // Don't show hint if argument's own name (or, for a qualified member access, its last
            // segment) matches the parameter name
            | _ when String.Equals(significantArgumentName argExpr, par.Name, StringComparison.CurrentCultureIgnoreCase) ->
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
            // Don't show hint if the initializer is a generic invocation whose explicit
            // type-argument list already spells out this exact type (`Enum.Parse<T>()`,
            // `JsonSerializer.Deserialize<T>()`, a local generic factory method, ...)
            |> Option.filter (fun ty ->
                var.Variables[0].Initializer
                |> Option.ofObj
                |> Option.map (fun eq -> not (isTypeSpelledOutInGenericInvocation eq.Value ty))
                |> Option.defaultValue true)
            // Don't show hint if the variable's own identifier already echoes the type name
            // (`resourceCondition`/`settingChangeCondition` vs. `ResourceCondition`)
            |> Option.filter (fun ty -> not (identifierEchoesTypeName var.Variables[0].Identifier.ValueText ty.Name))
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
            |> Option.filter (fun ty ->
                match dec.Designation with
                | :? SingleVariableDesignationSyntax as designation ->
                    not (identifierEchoesTypeName designation.Identifier.ValueText ty.Name)
                | _ -> true)
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
            |> Option.filter (fun ty -> not (identifierEchoesTypeName var.Identifier.ValueText ty.Name))
            |> Option.map (toTypeInlayHint var.Identifier.Span.End)
        | :? ForEachStatementSyntax as forEach when forEach.Type.IsVar ->
            semanticModel.GetForEachStatementInfo(forEach).ElementType
            |> validateType
            |> Option.filter (fun ty -> not (identifierEchoesTypeName forEach.Identifier.ValueText ty.Name))
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
            |> Option.bind (validateParameter node argument.Expression)
            |> Option.map (toParameterInlayHint argument.Span.Start)
        | :? AttributeArgumentSyntax as argument when isNull argument.NameEquals && isNull argument.NameColon ->
            argument
            |> getParameterForAttributeArgumentSyntax semanticModel
            |> Option.bind (validateParameter node argument.Expression)
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

    let registration (_config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
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

    let handle
        (context: RequestContext)
        (p: InlayHintParams)
        : Async<LspResult<InlayHint[] option> * LspWorkspaceUpdate> =
        async {
            let! wf, _ = context.LoadWorkspaceFolder(p.TextDocument.Uri)

            let docForUri =
                wf |> Option.bind (workspaceFolderDocument UserDocument p.TextDocument.Uri)

            match docForUri with
            | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
            | Some doc ->
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
                let! root = doc.GetSyntaxRootAsync(ct) |> Async.AwaitTask
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
                let textSpan = Range.toTextSpan sourceText.Lines p.Range

                let inlayHints =
                    root.DescendantNodes(textSpan, fun node -> node.Span.IntersectsWith(textSpan))
                    |> Seq.choose (toInlayHint semanticModel sourceText.Lines)

                return inlayHints |> Seq.toArray |> Some |> LspResult.success, LspWorkspaceUpdate.Empty
        }

    let resolve (_context: RequestContext) (_p: InlayHint) : Async<LspResult<InlayHint> * LspWorkspaceUpdate> = async {
        return LspResult.notImplemented<InlayHint>, LspWorkspaceUpdate.Empty
    }
