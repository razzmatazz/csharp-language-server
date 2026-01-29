module CSharpLanguageServer.Tests.DefinitionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testDefinitionWorks () =
    use client = activateFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    let definitionParams0: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None }

    let declaration0: Declaration option =
        client.Request("textDocument/definition", definitionParams0)

    Assert.IsTrue(declaration0.IsNone)

    let definitionParams1: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None }

    let declaration1: Declaration option =
        client.Request("textDocument/definition", definitionParams1)

    match declaration1 with
    | Some(U2.C2 declaration1Locations) ->
        let expectedLocations1: Location array =
            [| { Uri = classFile.Uri
                 Range =
                   { Start = { Line = 4u; Character = 16u }
                     End = { Line = 4u; Character = 23u } } } |]

        Assert.AreEqual(expectedLocations1, declaration1Locations)
    | _ -> failwith "U2.C2 Location[] was expected"

[<Test>]
let testDefinitionWorksInAspNetProject () =
    use client = activateFixture "aspnetProject"

    use testIndexViewModelCsFile = client.Open("Project/Models/Test/IndexViewModel.cs")
    use testControllerCsFile = client.Open("Project/Controllers/TestController.cs")

    let definitionParams0: DefinitionParams =
        { TextDocument = { Uri = testControllerCsFile.Uri }
          Position = { Line = 11u; Character = 12u }
          WorkDoneToken = None
          PartialResultToken = None }

    let definition0: Declaration option =
        client.Request("textDocument/definition", definitionParams0)

    let expectedLocations0: Location array =
        [| { Uri = testIndexViewModelCsFile.Uri
             Range =
               { Start = { Line = 3u; Character = 19u }
                 End = { Line = 3u; Character = 25u } } } |]

    match definition0 with
    | Some(U2.C2 definition0Locations) -> Assert.AreEqual(expectedLocations0, definition0Locations)
    | _ -> failwithf "Some Location[] was expected but %s received" (string definition0)
