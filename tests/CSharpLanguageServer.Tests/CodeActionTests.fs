module CSharpLanguageServer.Tests.CodeActionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testCodeActionOnMethodNameWorks() =
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

    let caParams0: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range = { Start = { Line = 2u; Character = 16u }
                    End = { Line = 2u; Character = 16u }
                  }
          Context = { Diagnostics = [| |]
                      Only = None
                      TriggerKind = None
                    }
          WorkDoneToken = None
          PartialResultToken = None
        }

    let caResult0 : TextDocumentCodeActionResult option = classFile.Request("textDocument/codeAction", caParams0)
    Assert.IsTrue(caResult0.IsSome)

    match caResult0 with
    | Some [| U2.C2 x |] ->
        Assert.AreEqual("Extract base class...", x.Title)
        Assert.AreEqual(None, x.Kind)
        Assert.AreEqual(None, x.Diagnostics)
        Assert.AreEqual(None, x.Disabled)
        Assert.IsTrue(x.Edit.IsSome)

    | _ -> failwith "Some [| U2.C1 x |] was expected"
