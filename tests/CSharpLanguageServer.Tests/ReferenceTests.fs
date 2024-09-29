module CSharpLanguageServer.Tests.ReferenceTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testReferenceWorks() =
    let classCsContents = """class Class
{
    public void MethodA(string arg)
    {
    }

    public void MethodB(string arg)
    {
        MethodA(arg);
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
    // try references request at MethodA declaration on line 2
    //
    let reference0Params: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations0: Location[] option = classFile.Request("textDocument/references", reference0Params)

    let expectedLocations: Location array =
        [|
            { Uri = classFile.Uri
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations, locations0.Value)

    //
    // try references request at empty line line 1 -- should return 0 results
    //
    let reference1Params: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations1: Location[] option = classFile.Request("textDocument/references", reference1Params)
    Assert.IsTrue(locations1.IsNone)
