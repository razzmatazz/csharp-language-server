module CSharpLanguageServer.Tests.HoverTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testHoverWorks() =
    use client = setupServerClient defaultClientProfile
                                   "TestData/testHoverWorks"
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    //
    // check hover at method name
    //
    let hover0Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
        }

    let hover0: Hover option =
        client.Request("textDocument/hover", hover0Params)

    Assert.IsTrue(hover0.IsSome)

    match hover0 with
    | Some hover ->
        match hover.Contents with
        | U3.C1 c ->
            Assert.AreEqual(MarkupKind.Markdown, c.Kind)
            Assert.AreEqual("```csharp\nvoid Class.Method(string arg)\n```", c.Value)
        | _ -> failwith "C1 was expected"

        Assert.IsTrue(hover.Range.IsNone)

    | _ -> failwith "Some (U3.C1 c) was expected"

    //
    // check hover on `string` value (external System.String type)
    //
    let hover1Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 8u }
          WorkDoneToken = None
        }

    let hover1: Hover option =
        client.Request("textDocument/hover", hover1Params)

    Assert.IsTrue(hover1.IsSome)

    match hover1 with
    | Some hover ->
        match hover.Contents with
        | U3.C1 c ->
            Assert.AreEqual(MarkupKind.Markdown, c.Kind)
            Assert.AreEqual("""```csharp
string
```

Represents text as a sequence of UTF-16 code units.""", c.Value.ReplaceLineEndings())
        | _ -> failwith "C1 was expected"

        Assert.IsTrue(hover.Range.IsNone)

    | _ -> failwith "Some (U3.C1 c) was expected"

    //
    // check hover at beginning of the file (nothing should come up)
    //
    let hover2Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
        }

    let hover2: Hover option =
        client.Request("textDocument/hover", hover2Params)

    Assert.IsTrue(hover2.IsNone)
