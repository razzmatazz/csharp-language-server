module CSharpLanguageServer.Tests.TypeDefinitionTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test textDocument/typeDefinition works`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let typeDefinitionParams0: TypeDefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 9u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None }

    let typeDefinition0: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/typeDefinition", typeDefinitionParams0)

    let csharpUriForSystemString =
        client.SolutionDir
        |> Uri
        |> string
        |> _.Substring("file:///".Length)
        |> sprintf "csharp:/%s/$metadata$/projects/Project/assemblies/System.Runtime/symbols/System.String.cs"

    match typeDefinition0 with
    | Some(U2.C1(U2.C2 ls)) ->
        Assert.AreEqual(1, ls.Length)

        let expectedTypeDefLocationsForStringArg =
            [| { Uri = csharpUriForSystemString
                 Range =
                   { Start = { Line = 12u; Character = 20u }
                     End = { Line = 12u; Character = 26u } } } |]

        Assert.AreEqual(expectedTypeDefLocationsForStringArg, ls)

    | _ -> failwith "Some U2.C1 (U2.C2) was expected"

    ()
