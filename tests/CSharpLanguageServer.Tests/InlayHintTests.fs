module CSharpLanguageServer.Tests.InlayHintTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

/// Requests inlay hints for the whole document. `Position.toLinePosition` clamps an
/// out-of-range line to the document's last line, so a large End.Line is a convenient way to
/// cover "the whole file" without knowing its exact length.
let private wholeDocumentInlayHintParams (doc: LspDocumentHandle) : InlayHintParams =
    { TextDocument = { Uri = doc.Uri }
      Range =
        { Start = { Line = 0u; Character = 0u }
          End = { Line = 1000000u; Character = 0u } }
      WorkDoneToken = None }

let private getHints (client: LspTestClient) (doc: LspDocumentHandle) : InlayHint array =
    let result: InlayHint array option =
        client.Request("textDocument/inlayHint", wholeDocumentInlayHintParams doc)

    result |> Option.defaultValue Array.empty

let private labelText (hint: InlayHint) : string =
    match hint.Label with
    | U2.C1 s -> s
    | U2.C2 _ -> ""

let private hintsOnLine (line: uint32) (hints: InlayHint array) : InlayHint array =
    hints |> Array.filter (fun h -> h.Position.Line = line)

let private hasHintWithLabel (label: string) (hints: InlayHint array) : bool =
    hints |> Array.exists (fun h -> labelText h = label)

[<Test>]
let ``textDocument/inlayHint shows a type hint for a var declaration initialized from a method call`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 25 (0-indexed): `var count = GetCount();`
    let onLine = hints |> hintsOnLine 25u

    Assert.IsTrue(
        onLine |> hasHintWithLabel ": int",
        sprintf "Expected a \": int\" type hint on line 25, got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint shows parameter hints for a regular call with differently-named arguments`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 30 (0-indexed): `helper.Add(1, 2);`
    let onLine = hints |> hintsOnLine 30u

    Assert.IsTrue(onLine |> hasHintWithLabel "first:", sprintf "Expected a \"first:\" hint on line 30, got: %A" onLine)

    Assert.IsTrue(
        onLine |> hasHintWithLabel "second:",
        sprintf "Expected a \"second:\" hint on line 30, got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a same-name single-line argument (regression guard)`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 36 (0-indexed): `helper.WithResource(resourceName, "other");`
    let onLine = hints |> hintsOnLine 36u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "resourceName:",
        sprintf "Expected no \"resourceName:\" hint on line 36 (argument matches parameter name), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "other:",
        sprintf "Expected an \"other:\" hint on line 36 (argument doesn't match parameter name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a same-name argument on its own indented line`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // lines 42-44 (0-indexed):
    //     helper.WithResource(
    //         resourceName,
    //         "other");
    let onLine = hints |> hintsOnLine 43u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "resourceName:",
        sprintf
            "Expected no \"resourceName:\" hint on line 43 (argument matches parameter name, only\
             differs by leading whitespace/newline trivia), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a qualified member-access argument matching the parameter name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // lines 49-51 (0-indexed):
    //     helper.WithResource(
    //         this.ResourceName,
    //         "other");
    let onLine = hints |> hintsOnLine 50u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "resourceName:",
        sprintf
            "Expected no \"resourceName:\" hint on line 50 (this.ResourceName's last segment matches\
             the parameter name), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a hint when the argument name doesn't match the parameter name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 57 (0-indexed): `helper.WithResource(somethingElse, "other");`
    let onLine = hints |> hintsOnLine 57u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "resourceName:",
        sprintf "Expected a \"resourceName:\" hint on line 57 (argument doesn't match parameter name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a hint when a qualified member-access argument's last segment doesn't match the parameter name``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // lines 62-64 (0-indexed):
    //     helper.WithResource(
    //         this.OtherValue,
    //         "other");
    let onLine = hints |> hintsOnLine 63u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "resourceName:",
        sprintf
            "Expected a \"resourceName:\" hint on line 63 (this.OtherValue's last segment doesn't\
             match the parameter name), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a hint for a very short (1-character) parameter name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 85 (0-indexed): `helper.SetX(5);` -- parameter is `x`
    let onLine = hints |> hintsOnLine 85u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "x:",
        sprintf "Expected no \"x:\" hint on line 85 (parameter name is too short to be informative), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses numbered-suffix parameter names but keeps a normal one`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 90 (0-indexed): `string.Format("{0} {1}", 1, 2);` -- parameters are `format`, `arg0`, `arg1`
    let onLine = hints |> hintsOnLine 90u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "arg0:",
        sprintf "Expected no \"arg0:\" hint on line 90 (numbered-suffix parameter name), got: %A" onLine
    )

    Assert.IsFalse(
        onLine |> hasHintWithLabel "arg1:",
        sprintf "Expected no \"arg1:\" hint on line 90 (numbered-suffix parameter name), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "format:",
        sprintf "Expected a \"format:\" hint on line 90 (not a numbered-suffix parameter name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses Math.Min's numbered-suffix parameter names`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 95 (0-indexed): `System.Math.Min(1, 2);` -- parameters are `val1`, `val2`
    let onLine = hints |> hintsOnLine 95u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "val1:",
        sprintf "Expected no \"val1:\" hint on line 95 (numbered-suffix parameter name), got: %A" onLine
    )

    Assert.IsFalse(
        onLine |> hasHintWithLabel "val2:",
        sprintf "Expected no \"val2:\" hint on line 95 (numbered-suffix parameter name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses Math.Round's short parameter name but keeps its longer, still-opaque ones`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 100 (0-indexed): `System.Math.Round(1.23m, 2, System.MidpointRounding.AwayFromZero);`
    // -- parameters are `d`, `decimals`, `mode`
    let onLine = hints |> hintsOnLine 100u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "d:",
        sprintf "Expected no \"d:\" hint on line 100 (parameter name is too short to be informative), got: %A" onLine
    )

    // `decimals`/`mode` aren't short or numbered-suffix, so the narrow mechanical rule
    // intentionally doesn't suppress them (unlike `d`), even though they're arguably still
    // low-information BCL names -- see plans/inlay-hint-reduction.md for the rationale.
    Assert.IsTrue(
        onLine |> hasHintWithLabel "decimals:",
        sprintf "Expected a \"decimals:\" hint on line 100 (not short or numbered-suffix), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "mode:",
        sprintf "Expected a \"mode:\" hint on line 100 (not short or numbered-suffix), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps hints for string.Substring's genuinely-disambiguating parameter names`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 105 (0-indexed): `"hello".Substring(1, 2);` -- parameters are `startIndex`, `length`
    let onLine = hints |> hintsOnLine 105u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "startIndex:",
        sprintf "Expected a \"startIndex:\" hint on line 105, got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "length:",
        sprintf "Expected a \"length:\" hint on line 105, got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps hints for buffer/offset/count-style parameter names`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 111 (0-indexed): `helper.WriteBuffer(data, 0, data.Length);`
    // -- parameters are `buffer`, `offset`, `count`
    let onLine = hints |> hintsOnLine 111u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "buffer:",
        sprintf "Expected a \"buffer:\" hint on line 111, got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "offset:",
        sprintf "Expected an \"offset:\" hint on line 111, got: %A" onLine
    )

    Assert.IsTrue(onLine |> hasHintWithLabel "count:", sprintf "Expected a \"count:\" hint on line 111, got: %A" onLine)

[<Test>]
let ``textDocument/inlayHint suppresses a hint for the sole lambda argument of a fluent LINQ-style call`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 144 (0-indexed): `new FluentQuery().Where(x => x > 0);` -- parameter is `predicate`
    let onLine = hints |> hintsOnLine 144u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "predicate:",
        sprintf
            "Expected no \"predicate:\" hint on line 144 (sole lambda argument, method name conveys role), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses hints for the sole lambda argument of chained fluent ORM-style calls`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 149 (0-indexed): `new FluentQuery().Fetch(x => x).ThenFetch(x => x);`
    // -- both parameters are `relatedObjectSelector`
    let onLine = hints |> hintsOnLine 149u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "relatedObjectSelector:",
        sprintf
            "Expected no \"relatedObjectSelector:\" hint on line 149 (sole lambda argument of each\
             call, method name conveys role), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps hints when a call takes more than one lambda argument`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 154 (0-indexed): `new FluentQuery().Combine(x => x, x => x);`
    // -- parameters are `first`, `second`
    let onLine = hints |> hintsOnLine 154u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "first:",
        sprintf "Expected a \"first:\" hint on line 154 (call has more than one lambda argument), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "second:",
        sprintf "Expected a \"second:\" hint on line 154 (call has more than one lambda argument), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a hint when the sole argument is a method group rather than a lambda`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 159 (0-indexed): `new FluentQuery().Where(IsPositive);` -- parameter is `predicate`
    let onLine = hints |> hintsOnLine 159u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "predicate:",
        sprintf
            "Expected a \"predicate:\" hint on line 159 (sole argument is a method group, not a lambda\
             expression -- out of scope for this rule), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses composite-format-string positional argument hints (rule #4, subsumed by rule #2)``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 177 (0-indexed): `new Logger().DebugFormat("{0}: {1} did {2}", 1, 2, 3);`
    // -- parameters are `format`, `arg0`, `arg1`, `arg2` (mirrors log4net's real ILog.DebugFormat
    // overload shapes). No dedicated implementation was needed for this: `arg0`/`arg1`/`arg2`
    // already match rule #2's numbered-suffix pattern (`hasUninformativeParameterName`).
    let onLine = hints |> hintsOnLine 177u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "format:",
        sprintf "Expected a \"format:\" hint on line 177, got: %A" onLine
    )

    Assert.IsFalse(
        onLine |> hasHintWithLabel "arg0:",
        sprintf "Expected no \"arg0:\" hint on line 177 (numbered-suffix parameter name), got: %A" onLine
    )

    Assert.IsFalse(
        onLine |> hasHintWithLabel "arg1:",
        sprintf "Expected no \"arg1:\" hint on line 177 (numbered-suffix parameter name), got: %A" onLine
    )

    Assert.IsFalse(
        onLine |> hasHintWithLabel "arg2:",
        sprintf "Expected no \"arg2:\" hint on line 177 (numbered-suffix parameter name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a hint for a single lambda argument followed by a trailing CancellationToken``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 205 (0-indexed): `new FluentQueryAsync().WhereAsync(x => x > 0, default);`
    // -- parameters are `predicate`, `cancellationToken`
    let onLine = hints |> hintsOnLine 205u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "predicate:",
        sprintf
            "Expected no \"predicate:\" hint on line 205 (sole non-CancellationToken argument is a\
             lambda, method name conveys role), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps hints for multiple lambda arguments even with a trailing CancellationToken`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 210 (0-indexed): `new FluentQueryAsync().CombineAsync(x => x, x => x, default);`
    // -- parameters are `first`, `second`, `cancellationToken`
    let onLine = hints |> hintsOnLine 210u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "first:",
        sprintf "Expected a \"first:\" hint on line 210 (more than one non-CancellationToken argument), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "second:",
        sprintf "Expected a \"second:\" hint on line 210 (more than one non-CancellationToken argument), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when a qualified generic invocation's type argument matches``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 240 (0-indexed): `var widget = GenericFactory.Create<Widget>();`
    let onLine = hints |> hintsOnLine 240u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": Widget",
        sprintf
            "Expected no \": Widget\" hint on line 240 (type is already spelled out in the\
             invocation's explicit type argument), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a var type hint when a generic invocation's type argument doesn't match the inferred type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 245 (0-indexed): `var description = GenericFactory.Describe<Widget>(new Widget());`
    // -- `Describe<Widget>` returns `string`, not `Widget`
    let onLine = hints |> hintsOnLine 245u

    Assert.IsTrue(
        onLine |> hasHintWithLabel ": string",
        sprintf
            "Expected a \": string\" hint on line 245 (inferred type doesn't match the invocation's\
             type argument), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when an unqualified generic invocation's type argument matches``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 250 (0-indexed): `var widget = CreateLocal<Widget>();`
    let onLine = hints |> hintsOnLine 250u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": Widget",
        sprintf
            "Expected no \": Widget\" hint on line 250 (type is already spelled out in the\
             unqualified invocation's explicit type argument), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint for the real Enum.Parse<T> BCL example`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 255 (0-indexed): `var day = System.Enum.Parse<System.DayOfWeek>("Monday");`
    let onLine = hints |> hintsOnLine 255u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": DayOfWeek",
        sprintf
            "Expected no \": DayOfWeek\" hint on line 255 (type is already spelled out in\
             Enum.Parse's explicit type argument), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when the identifier fully matches the inferred type name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 287 (0-indexed): `var resourceCondition = GetCondition();`
    let onLine = hints |> hintsOnLine 287u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": ResourceCondition",
        sprintf
            "Expected no \": ResourceCondition\" hint on line 287 (identifier is a full\
             case-insensitive match of the inferred type name), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when the identifier's last word matches the inferred type's last word``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 292 (0-indexed): `var settingChangeCondition = GetCondition();`
    let onLine = hints |> hintsOnLine 292u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": ResourceCondition",
        sprintf
            "Expected no \": ResourceCondition\" hint on line 292 (identifier's last word\
             \"Condition\" matches the inferred type's last word), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a var type hint when the identifier doesn't echo the inferred type name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 297 (0-indexed): `var outcome = GetCondition();`
    let onLine = hints |> hintsOnLine 297u

    Assert.IsTrue(
        onLine |> hasHintWithLabel ": ResourceCondition",
        sprintf
            "Expected a \": ResourceCondition\" hint on line 297 (identifier doesn't echo the\
             inferred type name), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a foreach variable's type hint when the identifier echoes the element type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 302 (0-indexed): `foreach (var widget in GetWidgets())`
    let onLine = hints |> hintsOnLine 302u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": Widget",
        sprintf
            "Expected no \": Widget\" hint on line 302 (identifier is a full case-insensitive\
             match of the element type name), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a foreach variable's type hint when the identifier doesn't echo the element type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 309 (0-indexed): `foreach (var item in GetWidgets())`
    let onLine = hints |> hintsOnLine 309u

    Assert.IsTrue(
        onLine |> hasHintWithLabel ": Widget",
        sprintf "Expected a \": Widget\" hint on line 309 (identifier doesn't echo the element type), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a parameter-name hint when the parameter name echoes its own declared type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 316 (0-indexed): `Dispatch(null);` -- parameter is `MessageDispatcherAsync messageDispatcherAsync`
    let onLine = hints |> hintsOnLine 316u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "messageDispatcherAsync:",
        sprintf
            "Expected no \"messageDispatcherAsync:\" hint on line 316 (parameter name is a\
             decapitalized echo of its own declared type), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a parameter-name hint when the parameter name doesn't echo its own declared type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 321 (0-indexed): `DispatchWithGenericName(null);` -- parameter is `MessageDispatcherAsync handler`
    let onLine = hints |> hintsOnLine 321u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "handler:",
        sprintf
            "Expected a \"handler:\" hint on line 321 (parameter name doesn't echo its own declared type), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a hint for the generic "obj" parameter name`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 342 (0-indexed): `helper.Save(5);` -- parameter is `object obj`
    let onLine = hints |> hintsOnLine 342u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "obj:",
        sprintf "Expected no \"obj:\" hint on line 342 (\"obj\" is a generic placeholder name), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a hint for a same-length parameter name that isn't "obj"`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 347 (0-indexed): `helper.Log("hi");` -- parameter is `string msg`
    let onLine = hints |> hintsOnLine 347u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "msg:",
        sprintf
            "Expected a \"msg:\" hint on line 347 (\"msg\" isn't the generic \"obj\" exception, only\
             coincidentally the same length), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when an explicit object-creation expression spells out the type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 360 (0-indexed): `var widget = new Widget();`
    let onLine = hints |> hintsOnLine 360u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": Widget",
        sprintf
            "Expected no \": Widget\" hint on line 360 (type is already spelled out right after `new`), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when an explicit object-creation expression with an initializer spells out the type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 365 (0-indexed): `var widget = new WidgetWithProperty { Value = 1 };`
    let onLine = hints |> hintsOnLine 365u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": WidgetWithProperty",
        sprintf
            "Expected no \": WidgetWithProperty\" hint on line 365 (type is already spelled out\
             right after `new`, even with an object initializer), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a hint for the sole "value" parameter of a call`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 387 (0-indexed): `helper.Contains(5);` -- sole parameter is `value`
    let onLine = hints |> hintsOnLine 387u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "value:",
        sprintf "Expected no \"value:\" hint on line 387 (sole argument, method name conveys role), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a "value" parameter hint when the call has more than one argument`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 392 (0-indexed): `helper.Add(1, 2);` -- parameters are `key`, `value`
    let onLine = hints |> hintsOnLine 392u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "key:",
        sprintf "Expected a \"key:\" hint on line 392 (call has more than one argument), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "value:",
        sprintf
            "Expected a \"value:\" hint on line 392 (call has more than one argument, so \"value\"\
             still helps disambiguate from \"key\"), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when a static invocation's qualifier matches the return type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 400 (0-indexed): `var reason = string.Format("{0}", 1);`
    let onLine = hints |> hintsOnLine 400u

    Assert.IsFalse(
        onLine |> hasHintWithLabel ": string",
        sprintf
            "Expected no \": string\" hint on line 400 (type is already spelled out as the\
             invocation's qualifier), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps a var type hint when a static invocation's qualifier doesn't match the return type``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 405 (0-indexed): `var converted = System.Convert.ToInt32("5");`
    // -- qualifier is `Convert`, return type is `int`
    let onLine = hints |> hintsOnLine 405u

    Assert.IsTrue(
        onLine |> hasHintWithLabel ": int",
        sprintf
            "Expected a \": int\" hint on line 405 (invocation's qualifier doesn't match the\
             return type), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint suppresses a hint for the sole "item" parameter of a call`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 426 (0-indexed): `helper.Add(5);` -- sole parameter is `item`
    let onLine = hints |> hintsOnLine 426u

    Assert.IsFalse(
        onLine |> hasHintWithLabel "item:",
        sprintf "Expected no \"item:\" hint on line 426 (sole argument, method name conveys role), got: %A" onLine
    )

[<Test>]
let ``textDocument/inlayHint keeps an "item" parameter hint when the call has more than one argument`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 431 (0-indexed): `helper.Insert(0, 5);` -- parameters are `index`, `item`
    let onLine = hints |> hintsOnLine 431u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "index:",
        sprintf "Expected an \"index:\" hint on line 431 (call has more than one argument), got: %A" onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "item:",
        sprintf
            "Expected an \"item:\" hint on line 431 (call has more than one argument, so \"item\"\
             still helps disambiguate from \"index\"), got: %A"
            onLine
    )

[<Test>]
let ``textDocument/inlayHint never shows a type hint for an implicit lambda parameter`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // Implicit lambda-parameter type hints (the `(p: SomeType) => ...` style hint) were removed
    // entirely -- see plans/inlay-hint-reduction.md. Real-world evidence showed this category is
    // almost always redundant (spelled out by an immediately-preceding generic invocation,
    // inferable from a property access one hop away, or simply repeated for the same type across
    // multiple lambdas in one short fluent chain), and Visual Studio's own default configuration
    // ships with all inline hints off by default.

    // line 144 (0-indexed): `new FluentQuery().Where(x => x > 0);` -- `x` infers to `int`
    Assert.IsFalse(
        hints |> hintsOnLine 144u |> hasHintWithLabel ": int",
        "Expected no \": int\" implicit lambda-parameter type hint on line 144"
    )

    // line 460 (0-indexed): `repository.Query<Widget>().Where(p => p != null);` -- `p` infers to
    // `Widget`, spelled out one call earlier at `Query<Widget>`
    Assert.IsFalse(
        hints |> hintsOnLine 460u |> hasHintWithLabel ": Widget",
        "Expected no \": Widget\" implicit lambda-parameter type hint on line 460"
    )

    // line 466 (0-indexed): `chained.Where(p => p != null);` -- `p` also infers to `Widget`, but
    // here the receiver isn't itself a generic invocation that spells the type out
    Assert.IsFalse(
        hints |> hintsOnLine 466u |> hasHintWithLabel ": Widget",
        "Expected no \": Widget\" implicit lambda-parameter type hint on line 466"
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint for a string literal initializer`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 474 (0-indexed): `var greeting = "hi";`
    Assert.IsFalse(
        hints |> hintsOnLine 474u |> hasHintWithLabel ": string",
        "Expected no \": string\" hint on line 474 (type is directly implied by the string literal)"
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint for a numeric literal initializer`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 479 (0-indexed): `var count = 42;`
    Assert.IsFalse(
        hints |> hintsOnLine 479u |> hasHintWithLabel ": int",
        "Expected no \": int\" hint on line 479 (type is directly implied by the numeric literal)"
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint for a boolean literal initializer`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 484 (0-indexed): `var flag = true;`
    Assert.IsFalse(
        hints |> hintsOnLine 484u |> hasHintWithLabel ": bool",
        "Expected no \": bool\" hint on line 484 (type is directly implied by the boolean literal)"
    )

[<Test>]
let ``textDocument/inlayHint keeps a var type hint for a unary-negated numeric expression`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 489 (0-indexed): `var negated = -1;` -- a PrefixUnaryExpressionSyntax, not itself a
    // LiteralExpressionSyntax, so intentionally out of scope for the literal-initializer rule
    Assert.IsTrue(
        hints |> hintsOnLine 489u |> hasHintWithLabel ": int",
        "Expected a \": int\" hint on line 489 (initializer is a unary expression, not a bare literal)"
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint for an interpolated string initializer`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 498 (0-indexed): `var greeting = $"Hello, {name}";`
    Assert.IsFalse(
        hints |> hintsOnLine 498u |> hasHintWithLabel ": string",
        "Expected no \": string\" hint on line 498 (interpolated string's natural type is always string)"
    )

[<Test>]
let ``textDocument/inlayHint suppresses a var type hint when an "as" expression's target type matches`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 506 (0-indexed): `var widget = obj as Widget;`
    Assert.IsFalse(
        hints |> hintsOnLine 506u |> hasHintWithLabel ": Widget",
        "Expected no \": Widget\" hint on line 506 (type is already spelled out right after `as`)"
    )
