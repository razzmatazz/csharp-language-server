module CSharpLanguageServer.Tests.DocumentSymbolTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let ``test textDocument/documentSymbol root has file range covering entire document`` () =
    use client = rentFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let docSymbolParams: DocumentSymbolParams =
        { TextDocument = { Uri = classFile.Uri }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<SymbolInformation[], DocumentSymbol[]> option =
        client.Request("textDocument/documentSymbol", docSymbolParams)

    Assert.That(result.IsSome, Is.True, "Expected Some result from textDocument/documentSymbol")

    let symbols =
        match result.Value with
        | U2.C2 docSymbols -> docSymbols
        | U2.C1 _ -> failwith "Expected DocumentSymbol[] (C2), got SymbolInformation[] (C1)"

    Assert.That(symbols.Length > 0, Is.True, "Expected at least one root symbol")

    let root = symbols.[0]
    Assert.That(root.Kind, Is.EqualTo(SymbolKind.File))
    Assert.That(root.Name, Is.EqualTo("Class.cs"))

    // The root range should start at the beginning of the file
    Assert.That(root.Range.Start.Line, Is.EqualTo(0u), "Root range should start at line 0")
    Assert.That(root.Range.Start.Character, Is.EqualTo(0u), "Root range should start at character 0")

    // The root range should extend to the end of the file (not be an empty 0,0-0,0 range)
    Assert.That(
        root.Range.End.Line > 0u,
        Is.True,
        "Root range end line should be beyond line 0 (file range should cover the whole document)"
    )

    // Class.cs has 16 lines (0-indexed: 0–15, with line 15 being empty after trailing newline)
    Assert.That(root.Range.End.Line, Is.EqualTo(15u), "Root range end line should be the last line of the file")
    Assert.That(root.Range.End.Character, Is.EqualTo(0u), "Root range end character for trailing-newline file")

    // SelectionRange should match Range for the root file symbol
    Assert.That(root.SelectionRange, Is.EqualTo(root.Range), "SelectionRange should equal Range for root file symbol")

[<Test>]
let ``test textDocument/documentSymbol root has children`` () =
    use client = rentFixture "genericProject"
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
    Assert.That(root.Kind, Is.EqualTo(SymbolKind.File))

    Assert.That(root.Children.IsSome, Is.True, "Root symbol should have children")
    Assert.That(root.Children.Value.Length > 0, Is.True, "Root symbol should have at least one child")

    // The namespace symbol should be among the children
    let nsSymbol =
        root.Children.Value |> Array.tryFind (fun s -> s.Kind = SymbolKind.Namespace)

    Assert.That(nsSymbol.IsSome, Is.True, "Expected a child symbol with kind Namespace")

[<Test>]
let ``test textDocument/documentSymbol root range covers file with namespace`` () =
    use client = rentFixture "genericProject"
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
    Assert.That(root.Kind, Is.EqualTo(SymbolKind.File))

    // Root range should start at beginning of file
    Assert.That(root.Range.Start.Line, Is.EqualTo(0u))
    Assert.That(root.Range.Start.Character, Is.EqualTo(0u))

    // Root range should extend to end of file, not be empty
    Assert.That(
        root.Range.End.Line > 0u,
        Is.True,
        "Root range should cover the full file, not be an empty range at 0,0"
    )
