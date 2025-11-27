module CSharpLanguageServer.Tests.CSharpMetadataTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test csharp/metadata works`` () =
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
        Assert.AreEqual(csharpUriForSystemString, ls[0].Uri)

    | _ -> failwith "Some U2.C1 (U2.C2) was expected"

    let metadataParams0: CSharpMetadataParams =
        { TextDocument = { Uri = csharpUriForSystemString } }

    let metadata0: CSharpMetadataResponse option =
        client.Request("csharp/metadata", metadataParams0)

    match metadata0 with
    | Some metadata0 ->
        Assert.AreEqual("System.Runtime", metadata0.AssemblyName)
        Assert.AreEqual("Project", metadata0.ProjectName)
        Assert.AreEqual("System.String", metadata0.SymbolName)
        Assert.IsTrue(metadata0.Source.StartsWith("using System"))

    | _ -> failwith "Some CSharpMetadataResponse was expected"
