module CSharpLanguageServer.Tests.CallHierarchyTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testCallHierarchyIncomingCallsWorks () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    // Step 1: Prepare call hierarchy for MethodA (line 4, char 16 is where "MethodA" is)
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for MethodA")
    | Some items ->
        Assert.AreEqual(1, items.Length)

        let methodAItem = items[0]
        Assert.AreEqual("MethodA(string)", methodAItem.Name)
        Assert.AreEqual(SymbolKind.Method, methodAItem.Kind)

        // Step 2: Get incoming calls for MethodA - should find MethodB as caller
        let incomingCallsParams: CallHierarchyIncomingCallsParams =
            { Item = methodAItem
              WorkDoneToken = None
              PartialResultToken = None }

        let incomingCallsResult: CallHierarchyIncomingCall[] option =
            client.Request("callHierarchy/incomingCalls", incomingCallsParams)

        match incomingCallsResult with
        | None -> Assert.Fail("incomingCalls should return a result")
        | Some incomingCalls ->
            Assert.AreEqual(1, incomingCalls.Length)

            let incomingCall = incomingCalls[0]
            Assert.AreEqual("MethodB(string)", incomingCall.From.Name)
            Assert.AreEqual(SymbolKind.Method, incomingCall.From.Kind)

            // FromRanges should point to the location where MethodA is called in MethodB (line 12)
            Assert.AreEqual(1, incomingCall.FromRanges.Length, "Should have one call site")
            Assert.AreEqual(12u, incomingCall.FromRanges[0].Start.Line, "Call site should be on line 12")

[<Test>]
let testCallHierarchyPrepareReturnsNoneForNonCallableSymbol () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    // Position on "using" keyword (line 0, char 0) - not a callable symbol
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | Some _ -> Assert.Fail("prepareCallHierarchy should return None for non-callable symbols")
    | None -> ()
