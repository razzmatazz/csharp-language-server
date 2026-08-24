module CSharpLanguageServer.Tests.HoverTests

open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let testHoverWorksInCSharpFile () =
    use client = rentFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    //
    // check hover at method name
    //
    let hover0Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

    match hover0 with
    | Some hover ->
        match hover.Contents with
        | U3.C1 c ->
            Assert.That(c.Kind, Is.EqualTo(MarkupKind.Markdown))
            Assert.That(c.Value.ReplaceLineEndings("\n"), Is.EqualTo("```csharp\nvoid Class.MethodA(string arg)\n```"))
        | _ -> failwith "C1 was expected"

        Assert.That(hover.Range.IsNone, Is.True)

    | _ -> failwith "Some (U3.C1 c) was expected"

    //
    // check hover on `string` value (external System.String type)
    //
    let hover1Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 6u; Character = 8u }
          WorkDoneToken = None }

    let hover1: Hover option = client.Request("textDocument/hover", hover1Params)

    match hover1 with
    | Some { Contents = U3.C1 c } ->
        Assert.That(c.Kind, Is.EqualTo(MarkupKind.Markdown))

        Assert.That(
            c.Value.ReplaceLineEndings("\n"),
            Is.EqualTo("```csharp\nstring\n```\n\nRepresents text as a sequence of UTF-16 code units.")
        )
    | _ -> failwith "Some (U3.C1 c) was expected"

    //
    // check hover at beginning of the file (nothing should come up)
    //
    let hover2Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None }

    let hover2: Hover option = client.Request("textDocument/hover", hover2Params)

    Assert.That(hover2.IsNone, Is.True)

[<Test>]
let testHoverWorksInRazorFile () =
    use client = rentFixture "aspnetProject"

    use indexCshtmlFile = client.Open("Project/Views/Test/Index.cshtml")

    Thread.Sleep(250)

    let hover0Params: HoverParams =
        { TextDocument = { Uri = indexCshtmlFile.Uri }
          Position = { Line = 1u; Character = 7u }
          WorkDoneToken = None }

    let hover0: Hover option = client.Request("textDocument/hover", hover0Params)

    Assert.That(hover0.IsSome, Is.True)

    match hover0 with
    | Some { Contents = U3.C1 c } ->
        Assert.That(c.Kind, Is.EqualTo(MarkupKind.Markdown))
        Assert.That(c.Value.ReplaceLineEndings("\n"), Is.EqualTo("```csharp\nstring? IndexViewModel.Output\n```"))

    | _ -> failwith "Some (U3.C1 c) was expected"

[<Test>]
let testHoverWorksWithLowercasedDriveLetterUri () =
    use client = activateFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    // A client may send a lowercase drive letter while the workspace root was
    // announced with an uppercase one; on windows both name the same file.
    // On non-windows paths (no drive letter) the rewrite is a no-op.
    let lowercasedUri =
        System.Text.RegularExpressions.Regex.Replace(
            classFile.Uri,
            "^file:///([A-Z]):",
            fun m -> "file:///" + m.Groups[1].Value.ToLowerInvariant() + ":"
        )

    // a didOpen with the mismatched casing must reach the document too
    let didOpenParams: DidOpenTextDocumentParams =
        { TextDocument =
            { Uri = lowercasedUri
              LanguageId = "csharp"
              Version = 1
              Text = System.IO.File.ReadAllText(System.Uri(classFile.Uri).LocalPath) } }

    client.Notify("textDocument/didOpen", didOpenParams)

    let hoverParams: HoverParams =
        { TextDocument = { Uri = lowercasedUri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let hover: Hover option = client.Request("textDocument/hover", hoverParams)

    match hover with
    | Some { Contents = U3.C1 c } ->
        Assert.That(c.Value.ReplaceLineEndings("\n"), Is.EqualTo("```csharp\nvoid Class.MethodA(string arg)\n```"))
    | _ -> Assert.Fail("hover through a lowercased drive-letter uri should answer contents")
