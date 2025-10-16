module CSharpLanguageServer.Tests.WorkspaceSymbolTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testWorkspaceSymbolWorks () =
    let client = Fixtures.getShared "testWorkspaceSymbolWorks"

    let serverCaps = client.GetState().ServerCapabilities.Value
    Assert.AreEqual(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some, serverCaps.WorkspaceSymbolProvider)

    use classFile = client.Open("Project/Class.cs")

    let completionParams0: WorkspaceSymbolParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Query = "Class" }

    let symbols0: U2<SymbolInformation[], WorkspaceSymbol[]> option =
        client.Request("workspace/symbol", completionParams0)

    match symbols0 with
    | Some(U2.C1 sis) ->
        Assert.AreEqual(1, sis.Length)

        let sym0 = sis[0]
        Assert.AreEqual("Class", sym0.Name)
        Assert.AreEqual(SymbolKind.Class, sym0.Kind)
        Assert.IsFalse(sym0.Tags.IsSome)
        Assert.IsFalse(sym0.ContainerName.IsSome)
        Assert.AreEqual(classFile.Uri, sym0.Location.Uri)
        ()

    | _ -> failwith "Some U2.C1 was expected"
