module CSharpLanguageServer.Tests.FoldingRangeTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

let private foldingRangeRequest (doc: LspDocumentHandle) : FoldingRangeParams =
    { TextDocument = { Uri = doc.Uri }
      WorkDoneToken = None
      PartialResultToken = None }

let private getRanges (client: LspTestClient) (doc: LspDocumentHandle) : FoldingRange array =
    let result: FoldingRange array option =
        client.Request("textDocument/foldingRange", foldingRangeRequest doc)
    result |> Option.defaultValue Array.empty

[<Test>]
let ``textDocument/foldingRange returns Some result`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let result: FoldingRange array option =
        client.Request("textDocument/foldingRange", foldingRangeRequest doc)

    Assert.IsTrue(result.IsSome, "Expected Some result from textDocument/foldingRange")

[<Test>]
let ``textDocument/foldingRange includes a range for the namespace`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "namespace Project.FoldingRangeTest { ... }" spans from line 3 to near end
    let hasNamespace =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 3u && r.EndLine > 40u && r.Kind = None)

    Assert.IsTrue(hasNamespace, sprintf "Expected a namespace folding range starting at line 3, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes a range for the class`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "public class FoldingSubject" opens on line 8 (0-indexed) { ... }
    let hasClass =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 8u && r.EndLine > 8u && r.Kind = None)

    Assert.IsTrue(hasClass, sprintf "Expected a class folding range starting at line 8, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes a range for a method`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "public string Greet()" opens on line 29 (0-indexed)
    let hasMethod =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 29u && r.EndLine > 29u && r.Kind = None)

    Assert.IsTrue(hasMethod, sprintf "Expected a method folding range starting at line 29, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes a range for a constructor`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // constructor "public FoldingRangeTestClass(...)" is on line 17
    let hasConstructor =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 17u && r.EndLine > 17u && r.Kind = None)

    Assert.IsTrue(hasConstructor, sprintf "Expected a constructor folding range starting at line 17, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes a range for a property`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "public int Value { get; set; }" is on line 23
    let hasProperty =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 23u && r.EndLine > 23u && r.Kind = None)

    Assert.IsTrue(hasProperty, sprintf "Expected a property folding range starting at line 23, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes imports range for multiple usings`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "using System;" is on line 0, "using System.Collections.Generic;" is on line 1
    let hasImports =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 0u && r.EndLine = 1u && r.Kind = Some FoldingRangeKind.Imports)

    Assert.IsTrue(hasImports, sprintf "Expected an imports folding range from line 0 to 1, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes region range`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "#region Fields" is on line 10 (0-indexed), "#endregion" is on line 15
    let hasRegion =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 10u && r.EndLine = 15u && r.Kind = Some FoldingRangeKind.Region)

    Assert.IsTrue(hasRegion, sprintf "Expected a region folding range from line 10 to 15, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes multi-line comment range`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "/* This is a\n   multi-line comment */" is on lines 31-32 (0-indexed)
    let hasComment =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 31u && r.EndLine = 32u && r.Kind = Some FoldingRangeKind.Comment)

    Assert.IsTrue(hasComment, sprintf "Expected a comment folding range from line 31 to 32, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange includes interface range`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    // "public interface IFoldable" is on line 43
    let hasInterface =
        ranges
        |> Array.exists (fun r ->
            r.StartLine = 43u && r.EndLine > 43u && r.Kind = None)

    Assert.IsTrue(hasInterface, sprintf "Expected an interface folding range starting at line 43, got: %A" ranges)

[<Test>]
let ``textDocument/foldingRange returns sorted ranges`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/FoldingRangeTest.cs"

    let ranges = getRanges client doc

    Assert.IsTrue(ranges.Length > 0, "Expected at least one folding range")

    let isSorted =
        ranges
        |> Array.pairwise
        |> Array.forall (fun (a, b) -> a.StartLine <= b.StartLine)

    Assert.IsTrue(isSorted, "Expected folding ranges to be sorted by StartLine")

[<Test>]
let ``textDocument/foldingRange on simple class file returns method ranges`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/Class.cs"

    let ranges = getRanges client doc

    // Class.cs: class opens on line 2, MethodA on line 4, MethodB on line 10
    let hasClass =
        ranges |> Array.exists (fun r -> r.StartLine = 2u && r.Kind = None)

    let hasMethodA =
        ranges |> Array.exists (fun r -> r.StartLine = 4u && r.Kind = None)

    let hasMethodB =
        ranges |> Array.exists (fun r -> r.StartLine = 10u && r.Kind = None)

    Assert.IsTrue(hasClass, sprintf "Expected class range at line 2, got: %A" ranges)
    Assert.IsTrue(hasMethodA, sprintf "Expected MethodA range at line 4, got: %A" ranges)
    Assert.IsTrue(hasMethodB, sprintf "Expected MethodB range at line 10, got: %A" ranges)
