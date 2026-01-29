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
        Assert.AreEqual(1, ls.Length)
        Assert.AreEqual(csharpUriForSystemString, ls[0].Uri)

    | _ -> failwith "Some U2.C1 (U2.C2) was expected"

    let metadataParams0: CSharpMetadataParams =
        { TextDocument = { Uri = csharpUriForSystemString } }

    let metadata0: CSharpMetadataResponse =
        match client.Request("csharp/metadata", metadataParams0) with
        | Some response -> response
        | None -> failwith "no response from csharp/metadata"

    Assert.AreEqual("System.Runtime", metadata0.AssemblyName)
    Assert.AreEqual("Project", metadata0.ProjectName)
    Assert.AreEqual("System.String", metadata0.SymbolName)
    Assert.IsTrue(metadata0.Source.StartsWith "using System")

[<Test>]
let ``test csharp/metadata works with no prior LSP request`` () =
    use client = activateFixture "genericProject"

    let csharpUriForSystemString =
        client.SolutionDir
        |> Uri
        |> string
        |> _.Substring("file:///".Length)
        |> sprintf "csharp:/%s/Project/Project.csproj/decompiled/System.String.cs"

    let metadataParams0: CSharpMetadataParams =
        { TextDocument = { Uri = csharpUriForSystemString } }

    let metadata0: CSharpMetadataResponse =
        match client.Request("csharp/metadata", metadataParams0) with
        | Some response -> response
        | None -> failwithf "no response from csharp/metadata for Uri=%s" csharpUriForSystemString

    Assert.AreEqual("System.Runtime", metadata0.AssemblyName)
    Assert.AreEqual("Project", metadata0.ProjectName)
    Assert.AreEqual("System.String", metadata0.SymbolName)
    Assert.IsTrue(metadata0.Source.StartsWith "using System")
