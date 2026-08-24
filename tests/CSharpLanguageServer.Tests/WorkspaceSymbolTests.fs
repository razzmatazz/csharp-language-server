module CSharpLanguageServer.Tests.WorkspaceSymbolTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let testWorkspaceSymbolWorks () =
    use client = rentFixture "genericProject"

    let serverCaps = client.ServerCapabilities.Value
    Assert.That(serverCaps.WorkspaceSymbolProvider, Is.EqualTo(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some))

    use classFile = client.Open("Project/Class.cs")

    let completionParams0: WorkspaceSymbolParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Query = "Class" }

    let symbols0: U2<SymbolInformation[], WorkspaceSymbol[]> option =
        client.Request("workspace/symbol", completionParams0)

    match symbols0 with
    | Some(U2.C1 sis) ->
        Assert.That(sis.Length, Is.EqualTo(4))

        let sym0 = sis[0]
        Assert.That(sym0.Name, Is.EqualTo("Class"))
        Assert.That(sym0.Kind, Is.EqualTo(SymbolKind.Class))
        Assert.That(sym0.Tags.IsSome, Is.False)
        Assert.That(sym0.ContainerName.IsSome, Is.False)
        Assert.That(sym0.Location.Uri, Is.EqualTo(classFile.Uri))
        ()

    | _ -> failwith "Some U2.C1 was expected"
