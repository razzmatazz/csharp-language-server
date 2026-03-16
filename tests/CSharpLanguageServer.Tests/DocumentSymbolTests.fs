module CSharpLanguageServer.Tests.DocumentSymbolTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test textDocument/documentSymbol root has file range covering entire document`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let docSymbolParams: DocumentSymbolParams =
        { TextDocument = { Uri = classFile.Uri }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<SymbolInformation[], DocumentSymbol[]> option =
        client.Request("textDocument/documentSymbol", docSymbolParams)

    Assert.IsTrue(result.IsSome, "Expected Some result from textDocument/documentSymbol")

    let symbols =
        match result.Value with
        | U2.C2 docSymbols -> docSymbols
        | U2.C1 _ -> failwith "Expected DocumentSymbol[] (C2), got SymbolInformation[] (C1)"

    Assert.IsTrue(symbols.Length > 0, "Expected at least one root symbol")

    let root = symbols.[0]
    Assert.AreEqual(SymbolKind.File, root.Kind)
    Assert.AreEqual("Class.cs", root.Name)

    // The root range should start at the beginning of the file
    Assert.AreEqual(0u, root.Range.Start.Line, "Root range should start at line 0")
    Assert.AreEqual(0u, root.Range.Start.Character, "Root range should start at character 0")

    // The root range should extend to the end of the file (not be an empty 0,0-0,0 range)
    Assert.IsTrue(
        root.Range.End.Line > 0u,
        "Root range end line should be beyond line 0 (file range should cover the whole document)"
    )

    // Class.cs has 16 lines (0-indexed: 0–15, with line 15 being empty after trailing newline)
    Assert.AreEqual(15u, root.Range.End.Line, "Root range end line should be the last line of the file")
    Assert.AreEqual(0u, root.Range.End.Character, "Root range end character for trailing-newline file")

    // SelectionRange should match Range for the root file symbol
    Assert.AreEqual(root.Range, root.SelectionRange, "SelectionRange should equal Range for root file symbol")

[<Test>]
let ``test textDocument/documentSymbol root has children`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    let docSymbolParams: DocumentSymbolParams =
        { TextDocument = { Uri = classFile.Uri }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<SymbolInformation[], DocumentSymbol[]> option =
        client.Request("textDocument/documentSymbol", docSymbolParams)

    let symbols =
        match result.Value with
        | U2.C2 docSymbols -> docSymbols
        | U2.C1 _ -> failwith "Expected DocumentSymbol[] (C2), got SymbolInformation[] (C1)"

    let root = symbols.[0]
    Assert.AreEqual(SymbolKind.File, root.Kind)

    Assert.IsTrue(root.Children.IsSome, "Root symbol should have children")
    Assert.IsTrue(root.Children.Value.Length > 0, "Root symbol should have at least one child")

    // The namespace symbol should be among the children
    let nsSymbol =
        root.Children.Value |> Array.tryFind (fun s -> s.Kind = SymbolKind.Namespace)

    Assert.IsTrue(nsSymbol.IsSome, "Expected a child symbol with kind Namespace")

[<Test>]
let ``test textDocument/documentSymbol root range covers file with namespace`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    let docSymbolParams: DocumentSymbolParams =
        { TextDocument = { Uri = classFile.Uri }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<SymbolInformation[], DocumentSymbol[]> option =
        client.Request("textDocument/documentSymbol", docSymbolParams)

    let symbols =
        match result.Value with
        | U2.C2 docSymbols -> docSymbols
        | U2.C1 _ -> failwith "Expected DocumentSymbol[] (C2), got SymbolInformation[] (C1)"

    let root = symbols.[0]
    Assert.AreEqual(SymbolKind.File, root.Kind)

    // Root range should start at beginning of file
    Assert.AreEqual(0u, root.Range.Start.Line)
    Assert.AreEqual(0u, root.Range.Start.Character)

    // Root range should extend to end of file, not be empty
    Assert.IsTrue(root.Range.End.Line > 0u, "Root range should cover the full file, not be an empty range at 0,0")
