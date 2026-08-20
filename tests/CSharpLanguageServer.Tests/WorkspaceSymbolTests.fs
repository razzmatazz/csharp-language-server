module CSharpLanguageServer.Tests.WorkspaceSymbolTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.FixturePool

[<Test>]
let testWorkspaceSymbolWorks () =
    use client = rentFixture "genericProject"

    let serverCaps = client.ServerCapabilities.Value
    ClassicAssert.AreEqual(true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some, serverCaps.WorkspaceSymbolProvider)

    use classFile = client.Open("Project/Class.cs")

    let completionParams0: WorkspaceSymbolParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Query = "Class" }

    let symbols0: U2<SymbolInformation[], WorkspaceSymbol[]> option =
        client.Request("workspace/symbol", completionParams0)

    match symbols0 with
    | Some(U2.C1 sis) ->
        ClassicAssert.AreEqual(4, sis.Length)

        let sym0 = sis[0]
        ClassicAssert.AreEqual("Class", sym0.Name)
        ClassicAssert.AreEqual(SymbolKind.Class, sym0.Kind)
        ClassicAssert.IsFalse(sym0.Tags.IsSome)
        ClassicAssert.IsFalse(sym0.ContainerName.IsSome)
        ClassicAssert.AreEqual(classFile.Uri, sym0.Location.Uri)
        ()

    | _ -> failwith "Some U2.C1 was expected"
