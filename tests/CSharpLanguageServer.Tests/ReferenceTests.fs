module CSharpLanguageServer.Tests.ReferenceTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testReferenceWorks () =
    use client = activateFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    //
    // try references request at empty line line 1 -- should return 0 results
    //
    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false } }

    let locations0: Location[] option =
        client.Request("textDocument/references", referenceParams0)

    Assert.IsTrue(locations0.IsNone)

    //
    // try references request at MethodA declaration on line 2
    //
    let referenceParams1: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false } }

    let locations1: Location[] option =
        client.Request("textDocument/references", referenceParams1)

    let expectedLocations1: Location array =
        [| { Uri = classFile.Uri
             Range =
               { Start = { Line = 10u; Character = 8u }
                 End = { Line = 10u; Character = 15u } } } |]

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
          Context = { IncludeDeclaration = true } }

    let locations2: Location[] option =
        client.Request("textDocument/references", referenceParams2)

    let expectedLocations2: Location array =
        [| { Uri = classFile.Uri
             Range =
               { Start = { Line = 2u; Character = 16u }
                 End = { Line = 2u; Character = 23u } } }

           { Uri = classFile.Uri
             Range =
               { Start = { Line = 10u; Character = 8u }
                 End = { Line = 10u; Character = 15u } } } |]

    Assert.AreEqual(expectedLocations2, locations2.Value)

[<Test>]
let testReferenceWorksDotnet8 () =
    use client = activateFixture "testReferenceWorksDotnet8"
    use classFile = client.Open("Project/Class.cs")

    //
    // try references request at empty line line 1 -- should return 0 results
    //
    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false } }

    let locations0: Location[] option =
        client.Request("textDocument/references", referenceParams0)

    Assert.IsTrue(locations0.IsNone)

    //
    // try references request at MethodA declaration on line 2
    //
    let referenceParams1: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false } }

    let locations1: Location[] option =
        client.Request("textDocument/references", referenceParams1)

    let expectedLocations1: Location array =
        [| { Uri = classFile.Uri
             Range =
               { Start = { Line = 8u; Character = 8u }
                 End = { Line = 8u; Character = 15u } } } |]

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
          Context = { IncludeDeclaration = true } }

    let locations2: Location[] option =
        client.Request("textDocument/references", referenceParams2)

    let expectedLocations2: Location array =
        [| { Uri = classFile.Uri
             Range =
               { Start = { Line = 2u; Character = 16u }
                 End = { Line = 2u; Character = 23u } } }

           { Uri = classFile.Uri
             Range =
               { Start = { Line = 8u; Character = 8u }
                 End = { Line = 8u; Character = 15u } } } |]

    Assert.AreEqual(expectedLocations2, locations2.Value)

[<Test>]
let testReferenceWorksToRazorPageReferencedValue () =
    use client = activateFixture "aspnetProject"

    use testIndexViewModelCsFile = client.Open "Project/Models/Test/IndexViewModel.cs"
    use testControllerCsFile = client.Open "Project/Controllers/TestController.cs"
    use indexCshtmlFile = client.Open "Project/Views/Test/Index.cshtml"

    use completionTestsCshtmlFile =
        client.Open "Project/Views/Test/CompletionTests.cshtml"

    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = testIndexViewModelCsFile.Uri }
          Position = { Line = 3u; Character = 20u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false } }

    let locations0: Location[] option =
        client.Request("textDocument/references", referenceParams0)

    Assert.IsTrue locations0.IsSome
    Assert.AreEqual(3, locations0.Value.Length)

    let expectedLocations0: Location array =
        [| { Uri = testControllerCsFile.Uri
             Range =
               { Start = { Line = 11u; Character = 12u }
                 End = { Line = 11u; Character = 18u } } }

           { Uri = completionTestsCshtmlFile.Uri
             Range =
               { Start = { Line = 3u; Character = 13u }
                 End = { Line = 3u; Character = 19u } } }

           { Uri = indexCshtmlFile.Uri
             Range =
               { Start = { Line = 1u; Character = 7u }
                 End = { Line = 1u; Character = 13u } } } |]

    let sortedLocations0 =
        locations0.Value
        |> Array.sortBy (fun f -> f.Uri, f.Range.Start.Line, f.Range.Start.Character)

    Assert.AreEqual(expectedLocations0, sortedLocations0)

    //
    // do same but with IncludeDeclaration=true
    //
    let referenceParams1: ReferenceParams =
        { TextDocument = { Uri = testIndexViewModelCsFile.Uri }
          Position = { Line = 3u; Character = 20u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = true } }

    let locations1: Location[] option =
        client.Request("textDocument/references", referenceParams1)

    Assert.IsTrue(locations1.IsSome)
    Assert.AreEqual(6, locations1.Value.Length)

    let expectedLocations1: Location array =
        [| { Uri = testControllerCsFile.Uri
             Range =
               { Start = { Line = 11u; Character = 12u }
                 End = { Line = 11u; Character = 18u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 19u }
                 End = { Line = 3u; Character = 25u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 28u }
                 End = { Line = 3u; Character = 31u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 33u }
                 End = { Line = 3u; Character = 36u } } }

           { Uri = completionTestsCshtmlFile.Uri
             Range =
               { Start = { Line = 3u; Character = 13u }
                 End = { Line = 3u; Character = 19u } } }

           { Uri = indexCshtmlFile.Uri
             Range =
               { Start = { Line = 1u; Character = 7u }
                 End = { Line = 1u; Character = 13u } } } |]

    let sortedLocations1 =
        locations1.Value
        |> Array.sortBy (fun f -> f.Uri, f.Range.Start.Line, f.Range.Start.Character)

    Assert.AreEqual(expectedLocations1, sortedLocations1)

[<Test>]
let testReferenceWorksFromRazorPageReferencedValue () =
    use client = activateFixture "aspnetProject"

    use testIndexViewModelCsFile = client.Open "Project/Models/Test/IndexViewModel.cs"
    use testControllerCsFile = client.Open "Project/Controllers/TestController.cs"
    use indexCshtmlFile = client.Open "Project/Views/Test/Index.cshtml"

    use completionTestsCshtmlFile =
        client.Open "Project/Views/Test/CompletionTests.cshtml"

    let referenceParams0: ReferenceParams =
        { TextDocument = { Uri = indexCshtmlFile.Uri }
          Position = { Line = 1u; Character = 7u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = true } }

    let locations0: Location[] option =
        client.Request("textDocument/references", referenceParams0)

    Assert.IsTrue(locations0.IsSome)
    Assert.AreEqual(6, locations0.Value.Length)

    let expectedLocations0: Location array =
        [| { Uri = testControllerCsFile.Uri
             Range =
               { Start = { Line = 11u; Character = 12u }
                 End = { Line = 11u; Character = 18u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 19u }
                 End = { Line = 3u; Character = 25u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 28u }
                 End = { Line = 3u; Character = 31u } } }

           { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 33u }
                 End = { Line = 3u; Character = 36u } } }

           { Uri = completionTestsCshtmlFile.Uri
             Range =
               { Start = { Line = 3u; Character = 13u }
                 End = { Line = 3u; Character = 19u } } }

           { Uri = indexCshtmlFile.Uri
             Range =
               { Start = { Line = 1u; Character = 7u }
                 End = { Line = 1u; Character = 13u } } } |]

    let sortedLocations0 =
        locations0.Value
        |> Array.sortBy (fun f -> f.Uri, f.Range.Start.Line, f.Range.Start.Character)

    Assert.AreEqual(expectedLocations0, sortedLocations0)
