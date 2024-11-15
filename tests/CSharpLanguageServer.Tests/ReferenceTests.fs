module CSharpLanguageServer.Tests.ReferenceTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testReferenceWorks() =
    use client = setupServerClient defaultClientProfile "TestData/testReferenceWorks"
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    //
    // try references request at empty line line 1 -- should return 0 results
    //
    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations0: Location[] option = classFile.Request("textDocument/references", referenceParams0)
    Assert.IsTrue(locations0.IsNone)

    //
    // try references request at MethodA declaration on line 2
    //
    let referenceParams1: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations1: Location[] option = classFile.Request("textDocument/references", referenceParams1)

    let expectedLocations1: Location array =
        [|
            { Uri = classFile.Uri.Substring(7)
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations1, locations1.Value)

    //
    // try references request at MethodA declaration on line 2
    // (with IncludeDeclaration=true)
    //
    let referenceParams2: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = true }
        }

    let locations2: Location[] option = classFile.Request("textDocument/references", referenceParams2)

    let expectedLocations2: Location array =
        [|
            { Uri = classFile.Uri.Substring(7)
              Range = {
                  Start = { Line = 2u; Character = 16u }
                  End = { Line = 2u; Character = 23u }
              }
            }

            { Uri = classFile.Uri.Substring(7)
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations2, locations2.Value)
