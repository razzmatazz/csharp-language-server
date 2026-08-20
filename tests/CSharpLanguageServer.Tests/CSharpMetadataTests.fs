module CSharpLanguageServer.Tests.CSharpMetadataTests

open System

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.FixturePool

[<Test>]
let ``test csharp/metadata works`` () =
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
        ClassicAssert.AreEqual(1, ls.Length)
        ClassicAssert.AreEqual(csharpUriForSystemString, ls[0].Uri)

    | _ -> failwith "Some U2.C1 (U2.C2) was expected"

    let metadataParams0: CSharpMetadataParams =
        { TextDocument = { Uri = csharpUriForSystemString } }

    let metadata0: CSharpMetadataResponse =
        match client.Request("csharp/metadata", metadataParams0) with
        | Some response -> response
        | None -> failwith "no response from csharp/metadata"

    ClassicAssert.AreEqual("System.Runtime", metadata0.AssemblyName)
    ClassicAssert.AreEqual("Project", metadata0.ProjectName)
    ClassicAssert.AreEqual("System.String", metadata0.SymbolName)
    ClassicAssert.IsTrue(metadata0.Source.StartsWith "using System")

[<Test>]
let ``test csharp/metadata works with no prior LSP request`` () =
    use client = rentFixture "genericProject"

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

    ClassicAssert.AreEqual("System.Runtime", metadata0.AssemblyName)
    ClassicAssert.AreEqual("Project", metadata0.ProjectName)
    ClassicAssert.AreEqual("System.String", metadata0.SymbolName)
    ClassicAssert.IsTrue(metadata0.Source.StartsWith "using System")

[<Test>]
let ``csharp metadata preserves type names ending in suffix characters`` () =
    use client = rentFixture "genericProject"

    let processMetadataUri =
        client.SolutionDir
        |> Uri
        |> string
        |> _.Substring("file:///".Length)
        |> sprintf "csharp:/%s/Project/Project.csproj/decompiled/System.Diagnostics.Process.cs"

    let metadataParams: CSharpMetadataParams =
        { TextDocument = { Uri = processMetadataUri } }

    let metadata: CSharpMetadataResponse =
        match client.Request("csharp/metadata", metadataParams) with
        | Some response -> response
        | None -> failwithf "no response from csharp/metadata for Uri=%s" processMetadataUri

    ClassicAssert.AreEqual("System.Diagnostics.Process", metadata.SymbolName)
    StringAssert.Contains("class Process", metadata.Source)

[<Test>]
let ``definition resolves members of nested metadata types`` () =
    use client = rentFixture "genericProject"

    use classFile =
        client.OpenWithText(
            "Project/Class.cs",
            """using System.Collections.Generic;
class Class
{
    bool M(Dictionary<int, string>.Enumerator e) => e.MoveNext();
}
"""
        )

    let definitionParams: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 3u; Character = 58u }
          WorkDoneToken = None
          PartialResultToken = None }

    let definition: Declaration option =
        client.Request("textDocument/definition", definitionParams)

    let locations =
        match definition with
        | Some(U2.C2 locations) when locations.Length > 0 -> locations
        | _ -> failwithf "metadata locations were expected but received %A" definition

    let location = locations[0]

    StringAssert.EndsWith("/decompiled/System.Collections.Generic.Dictionary%602%2BEnumerator.cs", location.Uri)
    Assert.That(locations, Has.All.Property("Uri").EqualTo(location.Uri))
