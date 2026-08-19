module CSharpLanguageServer.Tests.WorkspaceFolderRegressionTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``loose document attaches to nearest containing project`` () =
    use client = activateFixture "nestedProjects"
    use looseDocument = client.Open("App/Tests/Scratch.cs")

    let definitionParams: DefinitionParams =
        { TextDocument = { Uri = looseDocument.Uri }
          Position = { Line = 0u; Character = 17u }
          WorkDoneToken = None
          PartialResultToken = None }

    let definition: Declaration option =
        client.Request("textDocument/definition", definitionParams)

    match definition with
    | Some(U2.C2 [| location |]) -> StringAssert.EndsWith("/App/Tests/Marker.cs", location.Uri)
    | _ ->
        ClassicAssert.Fail(
            sprintf "definition in the nearest containing project was expected but received %A" definition
        )
