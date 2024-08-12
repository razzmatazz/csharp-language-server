module CSharpLanguageServer.Tests.HoverTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testHoverWorks() =
    let classCsContents = """class Class
{
    public void Method(string arg)
    {
    }
}
"""
    let projectFiles =
        Map.ofList [
          ("Project/Project.csproj", dotnet8PExeProjectCsproj)
          ("Project/Class.cs", classCsContents)
        ]

    use client = setupServerClient defaultClientProfile projectFiles
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

    let hover0: Hover option = classFile.Request("textDocument/hover", hover0Params)
    Assert.IsTrue(hover0.IsSome)

    match hover0 with
    | Some hover ->
        match hover.Contents with
        | U3.C1 c ->
            Assert.AreEqual(MarkupKind.Markdown, c.Kind)
            Assert.AreEqual("`` void Class.Method(string arg) ``", c.Value)
        | _ -> failwith "C1 was expected"

        Assert.IsTrue(hover.Range.IsNone)

    | _ -> failwith "Some (U3.C1 c) was expected"

    //
    // check hover at beginning of the file (nothing should come up)
    //
    let hover1Params: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
        }

    let hover1: Hover option = classFile.Request("textDocument/hover", hover1Params)
    Assert.IsTrue(hover1.IsNone)
