module CSharpLanguageServer.Tests.TypeDefinitionTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let ``test textDocument/typeDefinition works`` () =
    use client = rentFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let typeDefinitionParams0: TypeDefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 12u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None }

    let typeDefinition0: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/typeDefinition", typeDefinitionParams0)

    let csharpUriForSystemString =
        client.SolutionDir
        |> Uri
        |> string
        |> _.Substring("file:///".Length)
        |> sprintf "csharp:/%s/Project/Project.csproj/decompiled/System.String.cs"

    match typeDefinition0 with
    | Some(U2.C1(U2.C2 ls)) ->
        Assert.That(ls.Length, Is.EqualTo(1))

        let expectedTypeDefLocationsForStringArg =
            [| { Uri = csharpUriForSystemString
                 Range =
                   { Start = { Line = 12u; Character = 20u }
                     End = { Line = 12u; Character = 26u } } } |]

        Assert.That(ls, Is.EqualTo(box expectedTypeDefLocationsForStringArg))

    | _ -> failwith "Some U2.C1 (U2.C2) was expected"
