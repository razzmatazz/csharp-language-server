module CSharpLanguageServer.Tests.CallHierarchyTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let testCallHierarchyIncomingCallsWorks () =
    use client = rentFixture "genericProject"
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
        Assert.That(items.Length, Is.EqualTo(1))

        let methodAItem = items[0]
        Assert.That(methodAItem.Name, Is.EqualTo("MethodA(string)"))
        Assert.That(methodAItem.Kind, Is.EqualTo(SymbolKind.Method))

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
            Assert.That(incomingCalls.Length, Is.EqualTo(1))

            let incomingCall = incomingCalls[0]
            Assert.That(incomingCall.From.Name, Is.EqualTo("MethodB(string)"))
            Assert.That(incomingCall.From.Kind, Is.EqualTo(SymbolKind.Method))

            // FromRanges should point to the location where MethodA is called in MethodB (line 12)
            Assert.That(incomingCall.FromRanges.Length, Is.EqualTo(1), "Should have one call site")
            Assert.That(incomingCall.FromRanges[0].Start.Line, Is.EqualTo(12u), "Call site should be on line 12")

[<Test>]
let testCallHierarchyOutgoingCallsWorks () =
    use client = rentFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    // Step 1: Prepare call hierarchy for MethodB (line 10, char 16 is where "MethodB" is)
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 10u; Character = 16u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for MethodB")
    | Some items ->
        Assert.That(items.Length, Is.EqualTo(1))

        let methodBItem = items[0]
        Assert.That(methodBItem.Name, Is.EqualTo("MethodB(string)"))

        // Step 2: Get outgoing calls for MethodB - should find the call to MethodA
        let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
            { Item = methodBItem
              WorkDoneToken = None
              PartialResultToken = None }

        let outgoingCallsResult: CallHierarchyOutgoingCall[] option =
            client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

        match outgoingCallsResult with
        | None -> Assert.Fail("outgoingCalls should return a result")
        | Some outgoingCalls ->
            Assert.That(outgoingCalls.Length, Is.EqualTo(1))

            let outgoingCall = outgoingCalls[0]
            Assert.That(outgoingCall.To.Name, Is.EqualTo("MethodA(string)"))
            Assert.That(outgoingCall.To.Kind, Is.EqualTo(SymbolKind.Method))

            // FromRanges should point to the call site of MethodA inside MethodB (line 12)
            Assert.That(outgoingCall.FromRanges.Length, Is.EqualTo(1), "Should have one call site")
            Assert.That(outgoingCall.FromRanges[0].Start.Line, Is.EqualTo(12u), "Call site should be on line 12")

[<Test>]
let testCallHierarchyOutgoingCallsReturnsEmptyNotNullWhenNoVisibleTargets () =
    use client = rentFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    // MethodA (line 4, char 16) only calls Console.WriteLine, which lives in
    // metadata and has no source location, so the result should be an EMPTY
    // array (an honest "nothing to show"), not null ("unsupported").
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for MethodA")
    | Some items ->
        let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
            { Item = items[0]
              WorkDoneToken = None
              PartialResultToken = None }

        let outgoingCallsResult: CallHierarchyOutgoingCall[] option =
            client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

        match outgoingCallsResult with
        | None -> Assert.Fail("outgoingCalls should return [] for a callable symbol, not null")
        | Some outgoingCalls -> Assert.That(outgoingCalls.Length, Is.EqualTo(0))

[<Test>]
let testCallHierarchyOutgoingCallsCoverComplexCallSites () =
    use client = rentFixture "genericProject"
    use testFile = client.Open "Project/OutgoingCallsTest.cs"

    // Prepare on Orchestrator (line 4, char 16), whose body mixes call shapes:
    // a constructor call, a call inside a lambda, a local function invocation,
    // a call inside the local function's body, a delegate Invoke (metadata,
    // dropped) and a plain instance call.
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = testFile.Uri }
          Position = { Line = 4u; Character = 16u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for Orchestrator")
    | Some items ->
        let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
            { Item = items[0]
              WorkDoneToken = None
              PartialResultToken = None }

        let outgoingCallsResult: CallHierarchyOutgoingCall[] option =
            client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

        match outgoingCallsResult with
        | None -> Assert.Fail("outgoingCalls should return a result")
        | Some outgoingCalls ->
            let byName = outgoingCalls |> Array.map (fun c -> c.To.Name, c) |> Array.sortBy fst

            Assert.That(
                byName |> Array.map fst,
                Is.EqualTo(box [| "Helper(int)"; "LocalHelper()"; "Render()"; "Widget()" |]),
                "Expected the ctor, the lambda/local-function targets and the instance call; delegate Invoke has no source and is dropped"
            )

            let call name =
                byName |> Array.find (fun (n, _) -> n = name) |> snd

            // Helper is called twice: from the lambda (line 7) and from inside
            // the local function's body (line 14) - both attribute to Orchestrator.
            let helperLines =
                (call "Helper(int)").FromRanges |> Array.map _.Start.Line |> Array.sort

            Assert.That(helperLines, Is.EqualTo(box [| 7u; 14u |]))

            Assert.That((call "Widget()").FromRanges[0].Start.Line, Is.EqualTo(6u), "ctor call site")
            Assert.That((call "LocalHelper()").FromRanges[0].Start.Line, Is.EqualTo(8u), "local function call site")
            Assert.That((call "Render()").FromRanges[0].Start.Line, Is.EqualTo(10u), "instance call site")

[<Test>]
let testCallHierarchyOutgoingCallsGroupConstructedGenericsAndFindExtensionMethods () =
    use client = rentFixture "genericProject"
    use testFile = client.Open "Project/OutgoingCallsTest.cs"

    // CallsBoth (line 34, char 16) invokes Echo<T> twice with different type
    // arguments plus a user-defined extension method in instance form.
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = testFile.Uri }
          Position = { Line = 34u; Character = 16u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for CallsBoth")
    | Some items ->
        let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
            { Item = items[0]
              WorkDoneToken = None
              PartialResultToken = None }

        let outgoingCallsResult: CallHierarchyOutgoingCall[] option =
            client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

        match outgoingCallsResult with
        | None -> Assert.Fail("outgoingCalls should return a result")
        | Some outgoingCalls ->
            let byName = outgoingCalls |> Array.map (fun c -> c.To.Name, c) |> Array.sortBy fst

            // Echo<int> and Echo<string> are the SAME method: constructed
            // generics must group to one target under the original definition.
            Assert.That(
                byName |> Array.map fst,
                Is.EqualTo(box [| "Echo<T>(T)"; "Shout()" |]),
                "Constructed generic instantiations should group to one target; the extension method should be found"
            )

            let call name =
                byName |> Array.find (fun (n, _) -> n = name) |> snd

            let echoLines =
                (call "Echo<T>(T)").FromRanges |> Array.map _.Start.Line |> Array.sort

            Assert.That(echoLines, Is.EqualTo(box [| 36u; 37u |]), "Both instantiations' call sites should be present")
            Assert.That((call "Shout()").FromRanges[0].Start.Line, Is.EqualTo(38u), "extension method call site")

[<Test>]
let testCallHierarchyOutgoingCallsIncludeConstructorInitializers () =
    use client = rentFixture "genericProject"
    use testFile = client.Open "Project/OutgoingCallsTest.cs"

    let outgoingFor (line: uint32) (character: uint32) =
        let prepareParams: CallHierarchyPrepareParams =
            { TextDocument = { Uri = testFile.Uri }
              Position = { Line = line; Character = character }
              WorkDoneToken = None }

        let prepareResult: CallHierarchyItem[] option =
            client.Request("textDocument/prepareCallHierarchy", prepareParams)

        match prepareResult with
        | None -> failwithf "prepareCallHierarchy should return a result at %d:%d" line character
        | Some items ->
            let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
                { Item = items[0]
                  WorkDoneToken = None
                  PartialResultToken = None }

            let result: CallHierarchyOutgoingCall[] option =
                client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

            match result with
            | None -> failwithf "outgoingCalls should return a result at %d:%d" line character
            | Some calls -> calls

    // ChainDerived() : this(5) - the chained ctor is an outgoing call (line 58)
    let fromParameterless = outgoingFor 58u 11u
    Assert.That(fromParameterless.Length, Is.EqualTo(1))
    Assert.That(fromParameterless[0].To.Name, Is.EqualTo("ChainDerived(int)"))
    Assert.That(fromParameterless[0].FromRanges[0].Start.Line, Is.EqualTo(58u), "this(5) call site")

    // ChainDerived(int) : base(size) - the base ctor is an outgoing call (line 62)
    let fromSized = outgoingFor 62u 11u
    Assert.That(fromSized.Length, Is.EqualTo(1))
    Assert.That(fromSized[0].To.Name, Is.EqualTo("ChainBase(int)"))
    Assert.That(fromSized[0].FromRanges[0].Start.Line, Is.EqualTo(62u), "base(size) call site")

[<Test>]
let testCallHierarchyOutgoingCallsAnchorAtTheCalleeNameToken () =
    use client = rentFixture "genericProject"
    use testFile = client.Open "Project/OutgoingCallsTest.cs"

    // Start (line 69, char 23) is a multi-line fluent chain:
    //     return this
    //         .Step()      <- line 72
    //         .Step();     <- line 73
    // Each call site must anchor at ITS callee name token, not at the start
    // of the invocation expression (which is the chain head on line 71 for
    // both).
    let prepareParams: CallHierarchyPrepareParams =
        { TextDocument = { Uri = testFile.Uri }
          Position = { Line = 69u; Character = 23u }
          WorkDoneToken = None }

    let prepareResult: CallHierarchyItem[] option =
        client.Request("textDocument/prepareCallHierarchy", prepareParams)

    match prepareResult with
    | None -> Assert.Fail("prepareCallHierarchy should return a result for Start")
    | Some items ->
        let outgoingCallsParams: CallHierarchyOutgoingCallsParams =
            { Item = items[0]
              WorkDoneToken = None
              PartialResultToken = None }

        let outgoingCallsResult: CallHierarchyOutgoingCall[] option =
            client.Request("callHierarchy/outgoingCalls", outgoingCallsParams)

        match outgoingCallsResult with
        | None -> Assert.Fail("outgoingCalls should return a result")
        | Some outgoingCalls ->
            Assert.That(outgoingCalls.Length, Is.EqualTo(1), "Both chain links call the same method")

            let stepLines = outgoingCalls[0].FromRanges |> Array.map _.Start.Line |> Array.sort

            Assert.That(
                stepLines,
                Is.EqualTo(box [| 72u; 73u |]),
                "Each chained call should anchor on its own callee name line"
            )

[<Test>]
let testCallHierarchyPrepareReturnsNoneForNonCallableSymbol () =
    use client = rentFixture "genericProject"
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
