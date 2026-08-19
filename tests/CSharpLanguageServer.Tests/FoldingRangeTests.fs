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

[<TestFixture>]
type FoldingRangeTests() =
    inherit SharedReadOnlyFixture("genericProject", "Project/FoldingRangeTest.cs")

    [<Test>]
    member this.``textDocument/foldingRange returns Some result``() =
        let client = this.Client
        let doc = this.Doc

        let result: FoldingRange array option =
            client.Request("textDocument/foldingRange", foldingRangeRequest doc)

        Assert.That(result.IsSome, Is.True, "Expected Some result from textDocument/foldingRange")

    [<Test>]
    member this.``textDocument/foldingRange includes a range for the namespace``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "namespace Project.FoldingRangeTest { ... }" spans from line 3 to near end
        let hasNamespace =
            ranges
            |> Array.exists (fun r -> r.StartLine = 3u && r.EndLine > 40u && r.Kind = None)

        Assert.That(
            hasNamespace,
            Is.True,
            sprintf "Expected a namespace folding range starting at line 3, got: %A" ranges
        )

    [<Test>]
    member this.``textDocument/foldingRange includes a range for the class``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "public class FoldingSubject" opens on line 8 (0-indexed) { ... }
        let hasClass =
            ranges
            |> Array.exists (fun r -> r.StartLine = 8u && r.EndLine > 8u && r.Kind = None)

        Assert.That(hasClass, Is.True, sprintf "Expected a class folding range starting at line 8, got: %A" ranges)

    [<Test>]
    member this.``textDocument/foldingRange includes a range for a method``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "public string Greet()" opens on line 29 (0-indexed)
        let hasMethod =
            ranges
            |> Array.exists (fun r -> r.StartLine = 29u && r.EndLine > 29u && r.Kind = None)

        Assert.That(hasMethod, Is.True, sprintf "Expected a method folding range starting at line 29, got: %A" ranges)

    [<Test>]
    member this.``textDocument/foldingRange includes a range for a constructor``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // constructor "public FoldingRangeTestClass(...)" is on line 17
        let hasConstructor =
            ranges
            |> Array.exists (fun r -> r.StartLine = 17u && r.EndLine > 17u && r.Kind = None)

        Assert.That(
            hasConstructor,
            Is.True,
            sprintf "Expected a constructor folding range starting at line 17, got: %A" ranges
        )

    [<Test>]
    member this.``textDocument/foldingRange includes a range for a property``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "public int Value { get; set; }" is on line 23
        let hasProperty =
            ranges
            |> Array.exists (fun r -> r.StartLine = 23u && r.EndLine > 23u && r.Kind = None)

        Assert.That(
            hasProperty,
            Is.True,
            sprintf "Expected a property folding range starting at line 23, got: %A" ranges
        )

    [<Test>]
    member this.``textDocument/foldingRange includes imports range for multiple usings``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "using System;" is on line 0, "using System.Collections.Generic;" is on line 1
        let hasImports =
            ranges
            |> Array.exists (fun r -> r.StartLine = 0u && r.EndLine = 1u && r.Kind = Some FoldingRangeKind.Imports)

        Assert.That(hasImports, Is.True, sprintf "Expected an imports folding range from line 0 to 1, got: %A" ranges)

    [<Test>]
    member this.``textDocument/foldingRange includes region range``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "#region Fields" is on line 10 (0-indexed), "#endregion" is on line 15
        let hasRegion =
            ranges
            |> Array.exists (fun r -> r.StartLine = 10u && r.EndLine = 15u && r.Kind = Some FoldingRangeKind.Region)

        Assert.That(hasRegion, Is.True, sprintf "Expected a region folding range from line 10 to 15, got: %A" ranges)

    [<Test>]
    member this.``textDocument/foldingRange includes multi-line comment range``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "/* This is a\n   multi-line comment */" is on lines 31-32 (0-indexed)
        let hasComment =
            ranges
            |> Array.exists (fun r -> r.StartLine = 31u && r.EndLine = 32u && r.Kind = Some FoldingRangeKind.Comment)

        Assert.That(hasComment, Is.True, sprintf "Expected a comment folding range from line 31 to 32, got: %A" ranges)

    [<Test>]
    member this.``textDocument/foldingRange includes interface range``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        // "public interface IFoldable" is on line 43
        let hasInterface =
            ranges
            |> Array.exists (fun r -> r.StartLine = 43u && r.EndLine > 43u && r.Kind = None)

        Assert.That(
            hasInterface,
            Is.True,
            sprintf "Expected an interface folding range starting at line 43, got: %A" ranges
        )

    [<Test>]
    member this.``textDocument/foldingRange returns sorted ranges``() =
        let client = this.Client
        let doc = this.Doc

        let ranges = getRanges client doc

        Assert.That(ranges.Length > 0, Is.True, "Expected at least one folding range")

        let isSorted =
            ranges
            |> Array.pairwise
            |> Array.forall (fun (a, b) -> a.StartLine <= b.StartLine)

        Assert.That(isSorted, Is.True, "Expected folding ranges to be sorted by StartLine")

[<Test>]
let ``textDocument/foldingRange on simple class file returns method ranges`` () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/Class.cs"

    let ranges = getRanges client doc

    // Class.cs: class opens on line 2, MethodA on line 4, MethodB on line 10
    let hasClass = ranges |> Array.exists (fun r -> r.StartLine = 2u && r.Kind = None)

    let hasMethodA = ranges |> Array.exists (fun r -> r.StartLine = 4u && r.Kind = None)

    let hasMethodB =
        ranges |> Array.exists (fun r -> r.StartLine = 10u && r.Kind = None)

    Assert.That(hasClass, Is.True, sprintf "Expected class range at line 2, got: %A" ranges)
    Assert.That(hasMethodA, Is.True, sprintf "Expected MethodA range at line 4, got: %A" ranges)
    Assert.That(hasMethodB, Is.True, sprintf "Expected MethodB range at line 10, got: %A" ranges)
