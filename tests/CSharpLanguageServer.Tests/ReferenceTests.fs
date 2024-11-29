module CSharpLanguageServer.Tests.ReferenceTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testReferenceWorks() =
    use client = setupServerClient defaultClientProfile
                                   "TestData/testReferenceWorks"
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

    let locations0: Location[] option =
      classFile.Request("textDocument/references", referenceParams0)

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

    let locations1: Location[] option =
      classFile.Request("textDocument/references", referenceParams1)

    let expectedLocations1: Location array =
        [|
            { Uri = classFile.Uri
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

    let locations2: Location[] option =
      classFile.Request("textDocument/references", referenceParams2)

    let expectedLocations2: Location array =
        [|
            { Uri = classFile.Uri
              Range = {
                  Start = { Line = 2u; Character = 16u }
                  End = { Line = 2u; Character = 23u }
              }
            }

            { Uri = classFile.Uri
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations2, locations2.Value)


[<TestCase>]
let testReferenceWorksToAspNetRazorPageReferencedValue() =
    use client = setupServerClient { defaultClientProfile with LoggingEnabled = true }
                                   "TestData/testReferenceWorksToAspNetRazorPageReferencedValue"
    client.StartAndWaitForSolutionLoad()

    use testIndexViewModelCsFile = client.Open("Project/Models/Test/IndexViewModel.cs")

    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = testIndexViewModelCsFile.Uri }
          Position = { Line = 3u; Character = 20u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations0: Location[] option =
      testIndexViewModelCsFile.Request("textDocument/references", referenceParams0)

    Assert.IsTrue(locations0.IsSome)
    Assert.AreEqual(1, locations0.Value.Length)

    use testControllerCsFile = client.Open("Project/Controllers/TestController.cs")

    let expectedLocations0: Location array =
      [|
        { Uri = testControllerCsFile.Uri
          Range = { Start = { Line = 11u; Character = 12u }
                    End = { Line = 11u; Character = 18u } }
        }
       |]

    Assert.AreEqual(expectedLocations0, locations0.Value)

    //
    // do same but with IncludeDeclaration=true
    //
    let referenceParams1: ReferenceParams =
        { TextDocument = { Uri = testIndexViewModelCsFile.Uri }
          Position = { Line = 3u; Character = 20u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = true }
        }

    let locations1: Location[] option =
      testIndexViewModelCsFile.Request("textDocument/references", referenceParams1)

    Assert.IsTrue(locations1.IsSome)
    Assert.AreEqual(4, locations1.Value.Length)

    let expectedLocations1: Location array =
      [|
        { Uri = testIndexViewModelCsFile.Uri
          Range = { Start = { Line = 3u; Character = 19u }
                    End = { Line = 3u; Character = 25u } }
        }

        { Uri = testIndexViewModelCsFile.Uri
          Range = { Start = { Line = 3u; Character = 28u }
                    End = { Line = 3u; Character = 31u } }
        }

        { Uri = testIndexViewModelCsFile.Uri
          Range = { Start = { Line = 3u; Character = 33u }
                    End = { Line = 3u; Character = 36u } }
        }

        { Uri = testControllerCsFile.Uri
          Range = { Start = { Line = 11u; Character = 12u }
                    End = { Line = 11u; Character = 18u } }
        }
       |]

    let sortedLocations1 =
      locations1.Value
      |> Array.sortBy (fun f -> (f.Range.Start.Line, f.Range.Start.Character))

    Assert.AreEqual(expectedLocations1, sortedLocations1)
