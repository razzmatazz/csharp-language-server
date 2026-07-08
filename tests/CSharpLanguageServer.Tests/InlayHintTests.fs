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
let ``textDocument/inlayHint suppresses Math.Round's short parameter name but keeps its longer, still-opaque ones``
    ()
    =
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
let ``textDocument/inlayHint keeps hints for string.Substring's genuinely-disambiguating parameter names``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 105 (0-indexed): `"hello".Substring(1, 2);` -- parameters are `startIndex`, `length`
    let onLine = hints |> hintsOnLine 105u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "startIndex:",
        sprintf "Expected a \"startIndex:\" hint on line 105, got: %A" onLine
    )

    Assert.IsTrue(onLine |> hasHintWithLabel "length:", sprintf "Expected a \"length:\" hint on line 105, got: %A" onLine)

[<Test>]
let ``textDocument/inlayHint keeps hints for buffer/offset/count-style parameter names`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 111 (0-indexed): `helper.WriteBuffer(data, 0, data.Length);`
    // -- parameters are `buffer`, `offset`, `count`
    let onLine = hints |> hintsOnLine 111u

    Assert.IsTrue(onLine |> hasHintWithLabel "buffer:", sprintf "Expected a \"buffer:\" hint on line 111, got: %A" onLine)

    Assert.IsTrue(onLine |> hasHintWithLabel "offset:", sprintf "Expected an \"offset:\" hint on line 111, got: %A" onLine)

    Assert.IsTrue(onLine |> hasHintWithLabel "count:", sprintf "Expected a \"count:\" hint on line 111, got: %A" onLine)

[<Test>]
let ``textDocument/inlayHint suppresses a hint for the sole lambda argument of a fluent LINQ-style call``
    ()
    =
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
let ``textDocument/inlayHint suppresses hints for the sole lambda argument of chained fluent ORM-style calls``
    ()
    =
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
let ``textDocument/inlayHint keeps a hint when the sole argument is a method group rather than a lambda``
    ()
    =
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

    Assert.IsTrue(onLine |> hasHintWithLabel "format:", sprintf "Expected a \"format:\" hint on line 177, got: %A" onLine)

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
let ``textDocument/inlayHint keeps hints for multiple lambda arguments even with a trailing CancellationToken``
    ()
    =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/InlayHintTest.cs"

    let hints = getHints client doc

    // line 210 (0-indexed): `new FluentQueryAsync().CombineAsync(x => x, x => x, default);`
    // -- parameters are `first`, `second`, `cancellationToken`
    let onLine = hints |> hintsOnLine 210u

    Assert.IsTrue(
        onLine |> hasHintWithLabel "first:",
        sprintf
            "Expected a \"first:\" hint on line 210 (more than one non-CancellationToken argument), got: %A"
            onLine
    )

    Assert.IsTrue(
        onLine |> hasHintWithLabel "second:",
        sprintf
            "Expected a \"second:\" hint on line 210 (more than one non-CancellationToken argument), got: %A"
            onLine
    )
