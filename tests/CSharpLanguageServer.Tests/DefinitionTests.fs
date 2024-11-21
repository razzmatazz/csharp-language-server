module CSharpLanguageServer.Tests.DefinitionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testDefinitionWorks () =
    use client = setupServerClient { defaultClientProfile with LoggingEnabled = true }
                                   "TestData/testDefinitionWorks"
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    let definitionParams0: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
        }

    let declaration0: Declaration option = classFile.Request("textDocument/definition", definitionParams0)
    Assert.IsTrue(declaration0.IsNone)

    let definitionParams1: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
        }

    let declaration1: Declaration option = classFile.Request("textDocument/definition", definitionParams1)

    match declaration1.Value with
    | U2.C1 _ -> failwith "Location[] was expected"
    | U2.C2 declaration1Locations ->
        let expectedLocations1: Location array =
            [|
              { Uri = classFile.Uri
                Range = { Start = { Line = 2u; Character = 16u }
                          End = { Line = 2u; Character = 23u } }
              }
            |]

        Assert.AreEqual(expectedLocations1, declaration1Locations)

[<TestCase>]
let testDefinitionWorksInAspNetProject () =
    use client = setupServerClient defaultClientProfile
                                   "TestData/testDefinitionWorksInAspNetProject"
    client.StartAndWaitForSolutionLoad()

    use indexCsHtmlCs = client.Open("Project/Pages/Index.cshtml.cs")

    let definitionParams1: DefinitionParams =
        { TextDocument = { Uri = indexCsHtmlCs.Uri }
          Position = { Line = 7u; Character = 8u }
          WorkDoneToken = None
          PartialResultToken = None
        }

    let declaration1: Declaration option = indexCsHtmlCs.Request("textDocument/definition", definitionParams1)

    match declaration1.Value with
    | U2.C1 _ -> failwith "Location[] was expected"
    | U2.C2 declaration1Locations ->
        let expectedLocations1: Location array =
            [|
              { Uri = indexCsHtmlCs.Uri
                Range = { Start = { Line = 4u; Character = 19u }
                          End = { Line = 4u; Character = 24u } }
              }
            |]

        Assert.AreEqual(expectedLocations1, declaration1Locations)
