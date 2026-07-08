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
