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

    // Parameter names that are just a generic placeholder for "the argument", carrying no more
    // information than the argument's own name/shape already does -- e.g. NHibernate's
    // `ISession.SaveAsync(object obj, ...)`. Unlike `hasUninformativeParameterName`'s
    // length/numbered-suffix checks below, these are specific, known-opaque names rather than a
    // mechanical shape, so they're kept in an explicit, easy-to-extend set instead of a regex.
    // Applied unconditionally, regardless of how many arguments the call has -- safe only for
    // names that stay uninformative no matter the position (unlike `soleArgumentGenericParameterNames`
    // below).
    let private genericParameterNames = set [ "obj" ]

    // Parameter names that are only uninformative when they're the call's sole (effective)
    // argument -- e.g. `value` in `validStatusList.Contains(this.Order.Status)`, or `item` in
    // `orderProduct.SupplierOrderRequestProducts.Add(requestProduct)`, where `Contains`/`Add`
    // already convey the single argument's role just as clearly as a lambda does for `Where`.
    // Deliberately kept out of the always-on `genericParameterNames` set above: both legitimately
    // help disambiguate a *multi*-argument call (e.g. `Dictionary<TKey, TValue>.Add(key, value)`,
    // `List<T>.Insert(int index, T item)`), so they're only safe to suppress when there's nothing
    // to disambiguate -- see their scoped use in `validateParameter` below, alongside the
    // sole-lambda-argument rule this set extends.
    let private soleArgumentGenericParameterNames = set [ "value"; "item" ]

    // Splits a PascalCase/camelCase identifier into its constituent words (e.g.
    // `settingChangeCondition` -> `setting`, `Change`, `Condition`), so the last word can be
    // compared across identifiers using different casing conventions (a local variable:
    // camelCase; a type name: PascalCase). Module-level for the same reason as
    // numberedSuffixParameterName above.
    let private camelCaseWordBoundary =
        Regex(@"(?<=[a-z0-9])(?=[A-Z])", RegexOptions.Compiled)

    let private lastWord (identifier: string) : string =
        let words = camelCaseWordBoundary.Split(identifier)

        if words.Length = 0 then
            identifier
        else
            words[words.Length - 1]

    // True when `identifier` already "echoes" `typeName` -- either a full case-insensitive match
    // (`resourceStatus` vs. `ResourceStatus`, `messageDispatcherAsync` vs.
    // `MessageDispatcherAsync`), or their last words match (`settingChangeCondition` vs.
    // `ResourceCondition`, both ending in "Condition"). In either case, a hint would just restate
    // what the identifier's own name already conveys. See plans/inlay-hint-reduction.md for the
    // real-world evidence behind this heuristic.
    let private identifierEchoesTypeName (identifier: string) (typeName: string) : bool =
        String.Equals(identifier, typeName, StringComparison.CurrentCultureIgnoreCase)
        || String.Equals(lastWord identifier, lastWord typeName, StringComparison.CurrentCultureIgnoreCase)

    // A rendered type name longer than this (in characters, not counting the leading ": ") gets
    // middle-truncated by `truncateMiddle` below rather than shown in full. Most named types
    // (even fairly descriptive domain entity names) comfortably fit under this; it's compound
    // types -- anonymous types (`<anonymous type: int Year, int Month, ..., int InvoiceCount>`)
    // and tuples (`(string Code, string Value, string Name, string UIValue)`) -- that tend to blow
    // past it, since they grow without bound with the number of members/elements. See
    // plans/inlay-hint-reduction.md's "anonymous/tuple-type verbosity" follow-up for the
    // real-world motivation. Module-level so it's easy to find/tune in one place.
    let private maxTypeHintLabelLength = 40

    // Middle-truncates `s` with a `"..."` ellipsis once it exceeds `maxLength` characters,
    // otherwise returns it unchanged. Elides the *middle* (rather than just the tail) so both ends
    // stay visible: the head usually carries the outermost/generic type name (`<anonymous type: `,
    // `List<`, the leading element name of a tuple, ...), while the tail often carries the last,
    // sometimes most-distinguishing member/element (`..., int InvoiceCount>`). Splits the
    // available (non-ellipsis) budget as evenly as possible between head and tail, biasing any odd
    // leftover character to the head. `maxLength` values too small to fit even the ellipsis itself
    // degrade gracefully to the ellipsis alone (or less), rather than throwing.
    let private truncateMiddle (maxLength: int) (s: string) : string =
        if s.Length <= maxLength then
            s
        else
            let ellipsis = "..."
            let keep = max 0 (maxLength - ellipsis.Length)
            let headLen = (keep + 1) / 2
            let tailLen = keep - headLen
            s.Substring(0, headLen) + ellipsis + s.Substring(s.Length - tailLen, tailLen)

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

        // Strips `await` (and parenthesization) off the *outside* of a `var` initializer before
        // any of the `isTypeSpelledOutIn*` shape checks below inspect it. `await` can only ever
        // wrap the entire initializer expression once, at the very outside (you can't call more
        // members after awaiting mid-chain -- `await x.Foo().Bar()` awaits the whole `Foo().Bar()`
        // chain's result at the end, not `x.Foo()` alone), so a single, shared, top-level unwrap
        // here covers every one of the `isTypeSpelledOutIn*` checks without each of them needing
        // their own `AwaitExpressionSyntax` case. Almost every real-world async data-access call
        // site is awaited (e.g. `await db.GetAsync<T>(id)`, `await
        // db.Query<T>().Where(...).ToListAsync()`), so without this, every one of those checks
        // would silently fall through to its default "not spelled out" case and needlessly keep
        // the hint -- see plans/inlay-hint-reduction.md for two real, independently-discovered
        // examples of exactly this gap.
        let rec unwrapAwaitAndParens (expr: ExpressionSyntax) : ExpressionSyntax =
            match expr with
            | :? AwaitExpressionSyntax as await -> unwrapAwaitAndParens await.Expression
            | :? ParenthesizedExpressionSyntax as paren -> unwrapAwaitAndParens paren.Expression
            | _ -> expr

        let typeDisplayStyle =
            SymbolDisplayFormat(
                genericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
                miscellaneousOptions =
                    (SymbolDisplayMiscellaneousOptions.AllowDefaultLiteral
                     ||| SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
                     ||| SymbolDisplayMiscellaneousOptions.UseSpecialTypes)
            )

        let toTypeInlayHint (pos: int) (ty: ITypeSymbol) : InlayHint =
            let fullTypeName = SymbolName.fromSymbol typeDisplayStyle ty
            let displayTypeName = truncateMiddle maxTypeHintLabelLength fullTypeName

            { Position = pos |> lines.GetLinePosition |> Position.fromLinePosition
              Label = U2.C1(": " + displayTypeName)
              Kind = Some InlayHintKind.Type
              TextEdits = None
              // When the label was truncated, surface the full, untruncated type name as the
              // hint's hover tooltip, so the information eliding the middle removed from the
              // label is still just a hover away rather than lost entirely.
              Tooltip =
                if displayTypeName <> fullTypeName then
                    Some(U2.C1 fullTypeName)
                else
                    None
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

        // Returns true when `ty` is a constructed generic type (e.g. `List<DBSalesOrder>`) and
        // one of its own type arguments (e.g. `DBSalesOrder`) was already spelled out *earlier* in
        // the same fluent invocation chain that produced `initializer`, via an explicit generic
        // invocation's type-argument list -- the materializing-call sibling of
        // `isTypeSpelledOutInGenericInvocation` above, which only looks at the initializer's own,
        // outermost invocation. A real example (illustrative names, see
        // plans/inlay-hint-reduction.md): `var orders = await
        // db.Query<DBSalesOrder>().Where(o => ...).ToListAsync();` -- the outermost invocation is
        // `ToListAsync()`, which has no explicit type arguments of its own (its `List<DBSalesOrder>`
        // result is entirely inferred), so `isTypeSpelledOutInGenericInvocation` doesn't catch
        // this shape. But `DBSalesOrder` -- one of the inferred `List<DBSalesOrder>` type's own
        // type arguments -- is still spelled out two calls to the left, at `Query<DBSalesOrder>()`,
        // in the very same expression. Walks the invocation chain leftwards (each invocation's own
        // receiver, when that receiver is itself an invocation reached through a member access)
        // looking for any explicit generic invocation whose type-argument list contains a type
        // symbol-equal to one of `ty`'s type arguments. Parenthesized expressions are unwrapped.
        // Deliberately does not require the outermost call to be a specific, named method (e.g.
        // `ToList`/`ToListAsync`/`ToArray`) -- restricting to `ty` being a *constructed generic*
        // type whose element was spelled out is enough of a signal on its own, and avoids having
        // to maintain a per-method allow-list.
        let rec isElementTypeSpelledOutEarlierInInvocationChain
            (initializer: ExpressionSyntax)
            (ty: ITypeSymbol)
            : bool =
            match ty with
            | :? INamedTypeSymbol as namedTy when not namedTy.TypeArguments.IsEmpty ->
                let rec walkChain (expr: ExpressionSyntax) : bool =
                    match expr with
                    | :? ParenthesizedExpressionSyntax as paren -> walkChain paren.Expression
                    | :? InvocationExpressionSyntax as invocation ->
                        let genericName =
                            match invocation.Expression with
                            | :? GenericNameSyntax as generic -> Some generic
                            | :? MemberAccessExpressionSyntax as memberAccess ->
                                match memberAccess.Name with
                                | :? GenericNameSyntax as generic -> Some generic
                                | _ -> None
                            | _ -> None

                        let spelledOutHere =
                            genericName
                            |> Option.map (fun generic ->
                                generic.TypeArgumentList.Arguments
                                |> Seq.exists (fun typeArgSyntax ->
                                    match semanticModel.GetTypeInfo(typeArgSyntax).Type |> Option.ofObj with
                                    | Some typeArgSymbol ->
                                        namedTy.TypeArguments
                                        |> Seq.exists (fun elemTy ->
                                            SymbolEqualityComparer.Default.Equals(elemTy, typeArgSymbol))
                                    | None -> false))
                            |> Option.defaultValue false

                        if spelledOutHere then
                            true
                        else
                            match invocation.Expression with
                            | :? MemberAccessExpressionSyntax as memberAccess -> walkChain memberAccess.Expression
                            | _ -> false
                    | _ -> false

                walkChain initializer
            | _ -> false

        // Returns true when `initializer` is an explicit object-creation expression (`new
        // SomeType(...)` / `new SomeType { ... }`) whose spelled-out type is symbol-equal to
        // `ty` -- in that case a `var` type hint would be purely redundant, since the type is
        // already spelled out immediately after `new`, one step to the left in the very same
        // expression (mirrors `isTypeSpelledOutInGenericInvocation` above, but for plain
        // object-creation rather than an explicit generic type argument). Parenthesized
        // expressions are unwrapped. Deliberately restricted to explicit
        // `ObjectCreationExpressionSyntax` -- NOT `ImplicitObjectCreationExpressionSyntax`
        // (target-typed `new()`), where the type is *not* spelled out and the hint remains the
        // useful, intended case (handled separately, right after `new`, by the
        // `ImplicitObjectCreationExpressionSyntax` match arm below). Compares by symbol so it
        // doesn't fire on a mismatching type (e.g. an upcast via `as`, which isn't unwrapped
        // here, so its `ObjectCreationExpressionSyntax` operand is never reached in the first
        // place).
        let rec isTypeSpelledOutInObjectCreation (initializer: ExpressionSyntax) (ty: ITypeSymbol) : bool =
            match initializer with
            | :? ParenthesizedExpressionSyntax as paren -> isTypeSpelledOutInObjectCreation paren.Expression ty
            | :? ObjectCreationExpressionSyntax as objectCreation ->
                match semanticModel.GetTypeInfo(objectCreation.Type).Type |> Option.ofObj with
                | Some createdType -> SymbolEqualityComparer.Default.Equals(createdType, ty)
                | None -> false
            | _ -> false

        // Returns true when `initializer` is an invocation of a static member accessed through
        // its declaring type's own name (`string.Format(...)`, `Guid.NewGuid()`, `int.Parse(...)`,
        // ...) where that qualifying type is itself symbol-equal to `ty` -- in that case a `var`
        // type hint would be purely redundant, since the type is already spelled out immediately
        // to the left of `.` in the very same expression (the qualifier-based sibling of
        // `isTypeSpelledOutInGenericInvocation`'s type-argument-based check above). Parenthesized
        // expressions are unwrapped. `GetSymbolInfo` (not `GetTypeInfo`) is used on the qualifier
        // because a type used as a static-member-access receiver doesn't have a "value type" of
        // its own -- its symbol *is* the type. Compares by symbol, so it correctly doesn't fire
        // when the qualifier is an unrelated type whose name doesn't match the return type (e.g.
        // `Convert.ToInt32(...)`, qualifier `Convert` vs. return type `int`) or when the qualifier
        // is an instance (a local/field/property access resolves to that member's symbol, not a
        // type, so it never matches `:? ITypeSymbol`).
        let rec isTypeSpelledOutInStaticInvocationQualifier (initializer: ExpressionSyntax) (ty: ITypeSymbol) : bool =
            match initializer with
            | :? ParenthesizedExpressionSyntax as paren ->
                isTypeSpelledOutInStaticInvocationQualifier paren.Expression ty
            | :? InvocationExpressionSyntax as invocation ->
                match invocation.Expression with
                | :? MemberAccessExpressionSyntax as memberAccess ->
                    match semanticModel.GetSymbolInfo(memberAccess.Expression).Symbol |> Option.ofObj with
                    | Some(:? ITypeSymbol as qualifierType) -> SymbolEqualityComparer.Default.Equals(qualifierType, ty)
                    | _ -> false
                | _ -> false
            | _ -> false

        // Returns true when `initializer` is an `as` cast (`expr as SomeType`) whose target type
        // is symbol-equal to `ty` -- in that case a `var` type hint would be purely redundant,
        // since the type is already spelled out immediately after `as`, in the very same
        // expression (a real example: `obj as DBBankAccount` (illustrative name), from a private
        // codebase's `Equals` override, cited in plans/inlay-hint-reduction.md). Parenthesized
        // expressions are unwrapped.
        // `GetSymbolInfo` (not `GetTypeInfo`) is used on the right-hand side for the same reason
        // as `isTypeSpelledOutInStaticInvocationQualifier` above: it's a type reference, not a
        // value-producing expression, so it has no "value type" of its own -- its symbol *is* the
        // type. Mirrors Roslyn's own `CSharpInlineTypeHintsService`'s `BinaryExpressionSyntax`
        // with `AsExpression` kind candidate (see plans/inlay-hint-reduction.md).
        let rec isTypeSpelledOutInAsExpression (initializer: ExpressionSyntax) (ty: ITypeSymbol) : bool =
            match initializer with
            | :? ParenthesizedExpressionSyntax as paren -> isTypeSpelledOutInAsExpression paren.Expression ty
            | :? BinaryExpressionSyntax as binary when binary.Kind() = SyntaxKind.AsExpression ->
                match semanticModel.GetSymbolInfo(binary.Right).Symbol |> Option.ofObj with
                | Some(:? ITypeSymbol as castType) -> SymbolEqualityComparer.Default.Equals(castType, ty)
                | _ -> false
            | _ -> false

        // Returns true when `initializer` is a literal expression whose kind unambiguously
        // implies a specific type (a numeric, string, or boolean literal), or an interpolated
        // string expression (whose natural type, absent any target typing -- which a `var`
        // declaration never provides -- is always `string`) -- in either case a `var` type hint
        // would be purely redundant, since the type is directly implied by the initializer's own
        // syntax (`var x = "hi"` and `var x = $"hi {name}"` are both obviously `string`, `var x =
        // 42` is obviously `int`, ...). Parenthesized expressions are unwrapped. Deliberately
        // excludes the `null` literal (its type can't be inferred from the literal alone -- it
        // could be any reference/nullable-value type) and the `default` literal (same reasoning,
        // plus it's its own distinct syntax node, DefaultExpressionSyntax, not
        // LiteralExpressionSyntax). Also deliberately narrow about *which* expressions count:
        // e.g. a unary-negated numeric literal (`var x = -1`) is a PrefixUnaryExpressionSyntax,
        // not itself a LiteralExpressionSyntax, so it's intentionally left alone by this rule.
        // See plans/inlay-hint-reduction.md's "LiteralExpressionSyntax" Roslyn prior-art candidate
        // and the real interpolated-string example from a private codebase's trace/log-handler
        // class (illustrative name) that prompted extending it to interpolated strings too.
        let rec isTypeSpelledOutInLiteralOrInterpolatedString (initializer: ExpressionSyntax) : bool =
            match initializer with
            | :? ParenthesizedExpressionSyntax as paren ->
                isTypeSpelledOutInLiteralOrInterpolatedString paren.Expression
            | :? InterpolatedStringExpressionSyntax -> true
            | :? LiteralExpressionSyntax as literal ->
                match literal.Kind() with
                | SyntaxKind.NumericLiteralExpression
                | SyntaxKind.StringLiteralExpression
                | SyntaxKind.CharacterLiteralExpression
                | SyntaxKind.TrueLiteralExpression
                | SyntaxKind.FalseLiteralExpression -> true
                | _ -> false
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
        // `string.Substring(startIndex, length)` and `Stream.Write(buffer, offset, count)`. Also
        // suppressed for the explicit, known-opaque names in `genericParameterNames` (e.g.
        // `obj`), which aren't short/numbered enough for the mechanical checks alone to catch.
        let hasUninformativeParameterName (name: string) =
            name.Length <= 2
            || numberedSuffixParameterName.IsMatch(name)
            || genericParameterNames.Contains(name)

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
            // getParameterForArgumentSyntax above) when either:
            // - that argument is a lambda -- the method's own name already conveys the lambda's
            //   role (`Where(predicate)`, `Select(selector)`, NHibernate-style
            //   `Fetch(relatedObjectSelector)`/`ThenFetch(...)`, ...), so a parameter-name hint
            //   here is redundant no matter how generic or specific that name is. Deliberately
            //   scoped to lambda expressions only (not method-group arguments); or
            // - the parameter's own name is in `soleArgumentGenericParameterNames` (e.g. `value`
            //   in `Contains(value)`, or `item` in `someCollection.Add(item)`) -- with only one
            //   argument there's no position to disambiguate, so the name adds nothing beyond
            //   what the method name (`Contains`, `Add`, ...) already conveys, regardless of the
            //   argument expression's shape.
            // Both cases are scoped to single-(effective-)argument calls only -- multiple
            // arguments in one call aren't each automatically self-describing from the method
            // name alone (e.g. `Dictionary<TKey, TValue>.Add(key, value)`,
            // `List<T>.Insert(int index, T item)` keep all their hints).
            | :? ArgumentListSyntax as argList when
                ((argExpr :? LambdaExpressionSyntax)
                 || soleArgumentGenericParameterNames.Contains(par.Name))
                && (argList.Arguments
                    |> Seq.filter (isCancellationTokenArgument >> not)
                    |> Seq.length) = 1
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
            // `JsonSerializer.Deserialize<T>()`, a local generic factory method, ...), an explicit
            // object-creation expression that already spells out this exact type (`new
            // SomeType(...)` / `new SomeType { ... }`), a static invocation qualified by this
            // exact type's own name (`string.Format(...)`, `Guid.NewGuid()`, ...), an `as` cast to
            // this exact type (`obj as SomeType`), a literal/interpolated-string expression whose
            // type is directly implied by its own syntax (`"hi"`, `42`, `true`, `$"hi {name}"`,
            // ...), or a constructed generic type (e.g. `List<DBSalesOrder>`) one of whose own
            // type arguments was already spelled out earlier in the same fluent invocation chain
            // (e.g. `db.Query<DBSalesOrder>().Where(...).ToListAsync()`)
            |> Option.filter (fun ty ->
                var.Variables[0].Initializer
                |> Option.ofObj
                |> Option.map (fun eq ->
                    let initializerValue = unwrapAwaitAndParens eq.Value

                    not (isTypeSpelledOutInGenericInvocation initializerValue ty)
                    && not (isTypeSpelledOutInObjectCreation initializerValue ty)
                    && not (isTypeSpelledOutInStaticInvocationQualifier initializerValue ty)
                    && not (isTypeSpelledOutInAsExpression initializerValue ty)
                    && not (isTypeSpelledOutInLiteralOrInterpolatedString initializerValue)
                    && not (isElementTypeSpelledOutEarlierInInvocationChain initializerValue ty))
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
        // Deliberately no longer emitting implicit lambda-parameter type hints (the
        // `(p: DBEntity) => ...` style hint) at all -- see plans/inlay-hint-reduction.md for the
        // rationale: real-world evidence across multiple samples showed this hint category is
        // almost always redundant (spelled out by an immediately-preceding generic invocation,
        // inferable from a property access one hop to the left, or simply repeated identically
        // for the same type across multiple lambdas in one short fluent chain), and Visual
        // Studio's own default configuration ships with all inline hints -- including this one --
        // off by default.
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
