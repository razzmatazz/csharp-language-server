module CSharpLanguageServer.Tests.CompletionTests

open System.Threading
open System.Text.Json.Nodes

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.FixturePool

[<Test>]
let ``completion works in a .cs file`` () =
    use client = rentFixture "genericProject"

    // resolve provider is necessary for lsp client to resolve
    // detail and documentation props for a completion item
    let haveResolveProvider =
        client.ServerCapabilities
        |> Option.bind _.CompletionProvider
        |> Option.bind _.ResolveProvider
        |> Option.defaultValue false

    ClassicAssert.IsTrue(haveResolveProvider)

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
        ClassicAssert.IsTrue(cl.IsIncomplete)
        ClassicAssert.IsFalse(cl.ItemDefaults.IsSome)
        ClassicAssert.AreEqual(6, cl.Items.Length)

        let methodAItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodA")

        match methodAItem with
        | None -> failwith "an item with Label 'MethodA' was expected for completion at this position"
        | Some item ->
            ClassicAssert.AreEqual(item.Label, "MethodA")
            ClassicAssert.IsFalse(item.Detail.IsSome)
            ClassicAssert.IsFalse(item.Documentation.IsSome)
            ClassicAssert.IsFalse(item.Tags.IsSome)
            ClassicAssert.AreEqual(item.InsertText, Some "MethodA")
            ClassicAssert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            ClassicAssert.AreEqual(Some "MethodA", item.SortText)
            ClassicAssert.AreEqual(Some "MethodA", item.FilterText)
            ClassicAssert.AreEqual(None, item.InsertTextFormat)
            ClassicAssert.IsFalse(item.CommitCharacters.IsSome)
            ClassicAssert.IsFalse(item.TextEdit.IsSome)
            ClassicAssert.IsTrue(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            ClassicAssert.AreEqual(itemResolved.Detail, Some "void ClassForCompletion.MethodA(string arg)")
            ClassicAssert.IsFalse(itemResolved.Documentation.IsSome)

        let getHashCodeItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "GetHashCode")

        match getHashCodeItem with
        | None -> failwith "an item with Label 'GetHashCode' was expected for completion at this position"
        | Some item ->
            ClassicAssert.AreEqual(item.Label, "GetHashCode")
            ClassicAssert.IsFalse(item.Detail.IsSome)
            ClassicAssert.IsFalse(item.Documentation.IsSome)
            ClassicAssert.IsFalse(item.Tags.IsSome)
            ClassicAssert.AreEqual(item.InsertText, Some "GetHashCode")
            ClassicAssert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            ClassicAssert.AreEqual(Some "GetHashCode", item.SortText)
            ClassicAssert.AreEqual(Some "GetHashCode", item.FilterText)
            ClassicAssert.AreEqual(None, item.InsertTextFormat)
            ClassicAssert.IsFalse(item.CommitCharacters.IsSome)
            ClassicAssert.IsFalse(item.TextEdit.IsSome)
            ClassicAssert.IsTrue(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            ClassicAssert.AreEqual(itemResolved.Detail, Some "int object.GetHashCode()")
            ClassicAssert.IsTrue(itemResolved.Documentation.IsSome)

            match itemResolved.Documentation with
            | Some(U2.C2 markup) ->
                ClassicAssert.AreEqual(MarkupKind.PlainText, markup.Kind)
                ClassicAssert.AreEqual("Serves as the default hash function.", markup.Value)
            | _ -> failwith "Documentation w/ Kind=Markdown was expected for GetHashCode"

            ()

    | _ -> failwith "Some U2.C1 was expected"

    ()

[<Test>]
let ``completion works for extension methods`` () =
    use client = rentFixture "genericProject"

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
        ClassicAssert.AreEqual(7, cl.Items.Length)

        let methodBItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodB")

        match methodBItem with
        | None -> failwith "an item with Label 'MethodB' was expected for completion at this position"
        | Some item ->
            ClassicAssert.AreEqual("MethodB", item.Label)
            ClassicAssert.IsFalse(item.Detail.IsSome)
            ClassicAssert.IsFalse(item.Documentation.IsSome)
            ClassicAssert.AreEqual(Some CompletionItemKind.Method, item.Kind)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            ClassicAssert.AreEqual(
                itemResolved.Detail,
                Some "(extension) string ClassForCompletionWithExtensionMethods.MethodB()"
            )

            ClassicAssert.IsFalse(itemResolved.Documentation.IsSome)

    | _ -> failwith "Some U2.C1 was expected"

[<Test>]
[<Retry(5)>]
let ``completion works in cshtml files`` () =
    use client = rentFixture "aspnetProject"

    use cshtmlFile = client.Open "Project/Views/Test/CompletionTests.cshtml"

    Thread.Sleep(250) // TODO: work around race for Razor support

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
                ClassicAssert.AreEqual(expectedLabel, item.Label)
                ClassicAssert.IsFalse(item.Detail.IsSome)
                ClassicAssert.IsFalse(item.Documentation.IsSome)
                ClassicAssert.AreEqual(Some expectedCompletionItemKind, item.Kind)

                let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

                ClassicAssert.AreEqual(Some expectedDetail, itemResolved.Detail)
                ClassicAssert.IsTrue(documentationTestFn itemResolved.Documentation)

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

[<Test>]
let ``completionItem/resolve handles sentinel -1 positions in textEdit`` () =
    // Some editors (e.g. Neovim's built-in LSP client) echo back a completionItem
    // with "line": -1, "character": -1 as sentinel values when no real range is
    // known. Position is typed as uint32 in the LSP library, so these values
    // overflow during deserialization and the server crashes with -32603.
    // This test constructs the offending payload as a raw JObject (bypassing the
    // F# type system, just as the editor does) and asserts the server responds
    // successfully rather than erroring.
    use client = rentFixture "genericProject"
    use classFile = client.Open("Project/ClassForCompletionTests.cs")

    // First get a real completion item so we have a valid Data/cache key.
    let completionParams: CompletionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 13u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = None }

    let completion: U2<CompletionItem array, CompletionList> option =
        client.Request("textDocument/completion", completionParams)

    let item =
        match completion with
        | Some(U2.C2 cl) -> cl.Items |> Seq.find (fun i -> i.Label = "MethodA")
        | _ -> failwith "expected a CompletionList"

    // Serialize the item to a mutable JsonObject, then inject a textEdit with -1
    // sentinel positions — exactly what a misbehaving editor would send back.
    let itemJson =
        match JsonObject.Create((serialize item).JsonElement) with
        | null -> failwith "expected completion item to serialize as a JSON object"
        | obj -> obj

    let makeSentinelPosition () =
        let pos = JsonObject()
        pos["line"] <- JsonValue.Create(-1)
        pos["character"] <- JsonValue.Create(-1)
        pos

    let sentinelRange = JsonObject()
    sentinelRange["start"] <- makeSentinelPosition ()
    sentinelRange["end"] <- makeSentinelPosition ()

    let sentinelTextEdit = JsonObject()
    sentinelTextEdit["newText"] <- JsonValue.Create("")
    sentinelTextEdit["range"] <- sentinelRange

    itemJson["textEdit"] <- sentinelTextEdit

    // This must not throw (i.e. the server must not return a -32603 error).
    let resolved: CompletionItem = client.Request("completionItem/resolve", itemJson)

    ClassicAssert.AreEqual(Some "void ClassForCompletion.MethodA(string arg)", resolved.Detail)
