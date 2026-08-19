module CSharpLanguageServer.Tests.ReferenceTests

open System
open System.Threading

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestFixture>]
type ReferenceTests() =
    inherit SharedReadOnlyFixture("genericProject", "Project/Class.cs")

    [<Test>]
    member this.testReferenceWorks() =
        let client = this.Client
        let classFile = this.Doc

        //
        // try references request at `using` token in the file on line 0 -- should return 0 results
        //
        let referenceParams0: ReferenceParams =
            { TextDocument = { Uri = classFile.Uri }
              Position = { Line = 0u; Character = 0u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = false } }

        let locations0: Location[] option =
            client.Request("textDocument/references", referenceParams0)

        ClassicAssert.IsTrue(locations0.IsNone)

        //
        // try references request at MethodA declaration on line 2
        //
        let referenceParams1: ReferenceParams =
            { TextDocument = { Uri = classFile.Uri }
              Position = { Line = 4u; Character = 16u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = false } }

        let locations1: Location[] option =
            client.Request("textDocument/references", referenceParams1)

        let expectedLocations1: Location array =
            [| { Uri = classFile.Uri
                 Range =
                   { Start = { Line = 12u; Character = 8u }
                     End = { Line = 12u; Character = 15u } } } |]

        ClassicAssert.AreEqual(expectedLocations1, locations1.Value)

        //
        // try references request at MethodA declaration on line 2
        // (with IncludeDeclaration=true)
        //
        let referenceParams2: ReferenceParams =
            { TextDocument = { Uri = classFile.Uri }
              Position = { Line = 4u; Character = 16u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = true } }

        let locations2: Location[] option =
            client.Request("textDocument/references", referenceParams2)

        let expectedLocations2: Location array =
            [| { Uri = classFile.Uri
                 Range =
                   { Start = { Line = 4u; Character = 16u }
                     End = { Line = 4u; Character = 23u } } }

               { Uri = classFile.Uri
                 Range =
                   { Start = { Line = 12u; Character = 8u }
                     End = { Line = 12u; Character = 15u } } } |]

        ClassicAssert.AreEqual(expectedLocations2, locations2.Value)

    [<Test>]
    member this.testReferenceWithIncludeDeclarationDecompilesForBclSymbol() =
        // Regression test: textDocument/references with IncludeDeclaration=true on a BCL
        // method (Console.WriteLine) should include the decompiled definition location as a
        // csharp: URI when useMetadataUris=true, just like textDocument/definition does.
        let client = this.Client
        let classFile = this.Doc

        // Class.cs line 7 (0-indexed): Console.WriteLine(str);
        //                                       ^^^^^^^^^
        //                                       char 16-25
        let referenceParams: ReferenceParams =
            { TextDocument = { Uri = classFile.Uri }
              Position = { Line = 7u; Character = 16u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = true } }

        let locations: Location[] option =
            client.Request("textDocument/references", referenceParams)

        let expectedDefUri =
            client.SolutionDir
            |> Uri
            |> string
            |> _.Substring("file:///".Length)
            |> sprintf "csharp:/%s/Project/Project.csproj/decompiled/System.Console.cs"

        ClassicAssert.IsTrue(locations.IsSome, "Expected Some locations")

        let defLocations =
            locations.Value |> Array.filter (fun l -> l.Uri.StartsWith "csharp:")

        ClassicAssert.IsTrue(defLocations.Length > 0, "Expected at least one decompiled definition location")

        for loc in defLocations do
            ClassicAssert.AreEqual(expectedDefUri, loc.Uri)

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

    ClassicAssert.IsTrue(locations0.IsNone)

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

    ClassicAssert.AreEqual(expectedLocations1, locations1.Value)

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

    ClassicAssert.AreEqual(expectedLocations2, locations2.Value)

/// Shares one `aspnetProject` client/solution load and one open of each of the 4 Razor-adjacent
/// documents across both tests below, instead of paying for two separate activations (each
/// followed by the same `Thread.Sleep(250)` startup-race workaround).
[<TestFixture>]
type RazorReferenceTests() =
    inherit
        SharedReadOnlyFixture(
            "aspnetProject",
            [ "Project/Models/Test/IndexViewModel.cs"
              "Project/Controllers/TestController.cs"
              "Project/Views/Test/Index.cshtml"
              "Project/Views/Test/CompletionTests.cshtml" ]
        )

    override __.OnDocsOpened() = Thread.Sleep(250) // TODO: work around race for Razor support

    [<Test>]
    [<Retry(3)>]
    member this.testReferenceWorksToRazorPageReferencedValue() =
        let client = this.Client
        let testIndexViewModelCsFile = this.Docs.["Project/Models/Test/IndexViewModel.cs"]
        let testControllerCsFile = this.Docs.["Project/Controllers/TestController.cs"]
        let indexCshtmlFile = this.Docs.["Project/Views/Test/Index.cshtml"]

        let completionTestsCshtmlFile =
            this.Docs.["Project/Views/Test/CompletionTests.cshtml"]

        let referenceParams0: ReferenceParams =
            { TextDocument = { Uri = testIndexViewModelCsFile.Uri }
              Position = { Line = 3u; Character = 20u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = false } }

        let locations0: Location[] option =
            client.Request("textDocument/references", referenceParams0)

        ClassicAssert.IsTrue locations0.IsSome
        ClassicAssert.AreEqual(3, locations0.Value.Length)

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

        ClassicAssert.AreEqual(expectedLocations0, sortedLocations0)

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

        ClassicAssert.IsTrue(locations1.IsSome)
        ClassicAssert.AreEqual(6, locations1.Value.Length)

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

        ClassicAssert.AreEqual(expectedLocations1, sortedLocations1)

    [<Test>]
    [<Retry(3)>]
    member this.testReferenceWorksFromRazorPageReferencedValue() =
        let client = this.Client
        let testIndexViewModelCsFile = this.Docs.["Project/Models/Test/IndexViewModel.cs"]
        let testControllerCsFile = this.Docs.["Project/Controllers/TestController.cs"]
        let indexCshtmlFile = this.Docs.["Project/Views/Test/Index.cshtml"]

        let completionTestsCshtmlFile =
            this.Docs.["Project/Views/Test/CompletionTests.cshtml"]

        let referenceParams0: ReferenceParams =
            { TextDocument = { Uri = indexCshtmlFile.Uri }
              Position = { Line = 1u; Character = 7u }
              WorkDoneToken = None
              PartialResultToken = None
              Context = { IncludeDeclaration = true } }

        let locations0: Location[] option =
            client.Request("textDocument/references", referenceParams0)

        ClassicAssert.IsTrue(locations0.IsSome)
        ClassicAssert.AreEqual(6, locations0.Value.Length)

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

        ClassicAssert.AreEqual(expectedLocations0, sortedLocations0)
