module CSharpLanguageServer.Tests.CompletionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``completion works in a .cs file`` () =
    use client = activateFixture "genericProject"

    // resolve provider is necessary for lsp client to resolve
    // detail and documentation props for a completion item
    let haveResolveProvider =
        client.GetState().ServerCapabilities
        |> Option.bind _.CompletionProvider
        |> Option.bind _.ResolveProvider
        |> Option.defaultValue false

    Assert.IsTrue(haveResolveProvider)

    use classFile = client.Open("Project/ClassForCompletionTests.cs")

    let completionParams0: CompletionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 13u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = None }

    let completion0: U2<CompletionItem array, CompletionList> option =
        client.Request("textDocument/completion", completionParams0)

    match completion0 with
    | Some(U2.C2 cl) ->
        Assert.IsTrue(cl.IsIncomplete)
        Assert.IsFalse(cl.ItemDefaults.IsSome)
        Assert.AreEqual(6, cl.Items.Length)

        let methodAItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodA")

        match methodAItem with
        | None -> failwith "an item with Label 'MethodA' was expected for completion at this position"
        | Some item ->
            Assert.AreEqual(item.Label, "MethodA")
            Assert.IsFalse(item.Detail.IsSome)
            Assert.IsFalse(item.Documentation.IsSome)
            Assert.IsFalse(item.Tags.IsSome)
            Assert.AreEqual(item.InsertText, Some "MethodA")
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            Assert.AreEqual(Some "MethodA", item.SortText)
            Assert.AreEqual(Some "MethodA", item.FilterText)
            Assert.AreEqual(None, item.InsertTextFormat)
            Assert.IsFalse(item.CommitCharacters.IsSome)
            Assert.IsFalse(item.TextEdit.IsSome)
            Assert.IsTrue(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.AreEqual(itemResolved.Detail, Some "void ClassForCompletion.MethodA(string arg)")
            Assert.IsFalse(itemResolved.Documentation.IsSome)

        let getHashCodeItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "GetHashCode")

        match getHashCodeItem with
        | None -> failwith "an item with Label 'GetHashCode' was expected for completion at this position"
        | Some item ->
            Assert.AreEqual(item.Label, "GetHashCode")
            Assert.IsFalse(item.Detail.IsSome)
            Assert.IsFalse(item.Documentation.IsSome)
            Assert.IsFalse(item.Tags.IsSome)
            Assert.AreEqual(item.InsertText, Some "GetHashCode")
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            Assert.AreEqual(Some "GetHashCode", item.SortText)
            Assert.AreEqual(Some "GetHashCode", item.FilterText)
            Assert.AreEqual(None, item.InsertTextFormat)
            Assert.IsFalse(item.CommitCharacters.IsSome)
            Assert.IsFalse(item.TextEdit.IsSome)
            Assert.IsTrue(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.AreEqual(itemResolved.Detail, Some "int object.GetHashCode()")
            Assert.IsTrue(itemResolved.Documentation.IsSome)

            match itemResolved.Documentation with
            | Some(U2.C2 markup) ->
                Assert.AreEqual(MarkupKind.PlainText, markup.Kind)
                Assert.AreEqual("Serves as the default hash function.", markup.Value)
            | _ -> failwith "Documentation w/ Kind=Markdown was expected for GetHashCode"

            ()

    | _ -> failwith "Some U2.C1 was expected"

    ()

[<Test>]
let ``completion works for extension methods`` () =
    use client = activateFixture "genericProject"

    use classFile =
        client.Open("Project/ClassForCompletionTestsWithExtensionMethods.cs")

    let completionParams0: CompletionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 13u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = None }

    let completion0: U2<CompletionItem array, CompletionList> option =
        client.Request("textDocument/completion", completionParams0)

    match completion0 with
    | Some(U2.C2 cl) ->
        Assert.AreEqual(7, cl.Items.Length)

        let methodBItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodB")

        match methodBItem with
        | None -> failwith "an item with Label 'MethodB' was expected for completion at this position"
        | Some item ->
            Assert.AreEqual("MethodB", item.Label)
            Assert.IsFalse(item.Detail.IsSome)
            Assert.IsFalse(item.Documentation.IsSome)
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.AreEqual(
                itemResolved.Detail,
                Some "(extension) string ClassForCompletionWithExtensionMethods.MethodB()"
            )

            Assert.IsFalse(itemResolved.Documentation.IsSome)

    | _ -> failwith "Some U2.C1 was expected"

[<Test>]
let ``completion works in cshtml files`` () =
    use client = activateFixture "aspnetProject"

    use cshtmlFile = client.Open "Project/Views/Test/CompletionTests.cshtml"

    let testCompletionResultContainsItem
        line
        character
        expectedLabel
        expectedCompletionItemKind
        expectedDetail
        documentationTestFn
        =
        let completionParams0: CompletionParams =
            { TextDocument = { Uri = cshtmlFile.Uri }
              Position = { Line = line; Character = character }
              WorkDoneToken = None
              PartialResultToken = None
              Context = None }

        let completion: U2<CompletionItem array, CompletionList> option =
            client.Request("textDocument/completion", completionParams0)

        match completion with
        | Some(U2.C2 cl) ->
            let expectedItem = cl.Items |> Seq.tryFind (fun i -> i.Label = expectedLabel)

            match expectedItem with
            | None -> failwithf "an item with Label '%s' was expected for completion at this position" expectedLabel
            | Some item ->
                Assert.AreEqual(expectedLabel, item.Label)
                Assert.IsFalse(item.Detail.IsSome)
                Assert.IsFalse(item.Documentation.IsSome)
                Assert.AreEqual(Some expectedCompletionItemKind, item.Kind)

                let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

                Assert.AreEqual(Some expectedDetail, itemResolved.Detail)
                Assert.IsTrue(documentationTestFn itemResolved.Documentation)

        | _ -> failwith "Some U2.C1 was expected"

    //
    // 1st completion test: (@Model.|)
    //
    testCompletionResultContainsItem
        1u
        14u
        "Output"
        CompletionItemKind.Property
        "string? Project.Models.Test.IndexViewModel.Output { get; set; }"
        _.IsNone

    //
    // 2nd completion test: @Model.|
    //
    testCompletionResultContainsItem
        2u
        13u
        "Output"
        CompletionItemKind.Property
        "string? Project.Models.Test.IndexViewModel.Output { get; set; }"
        _.IsNone

    //
    // 3nd completion test: @Model.Output.|
    //
    testCompletionResultContainsItem 3u 13u "ToString" CompletionItemKind.Method "string? object.ToString()" _.IsSome

    //
    // 4nd completion test: x.
    //
    testCompletionResultContainsItem
        6u
        6u
        "TryFormat"
        CompletionItemKind.Method
        "bool int.TryFormat(Span<byte> utf8Destination, out int bytesWritten, [ReadOnlySpan<char> format = default], [IFormatProvider? provider = null])"
        _.IsSome
