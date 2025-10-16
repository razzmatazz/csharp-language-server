module CSharpLanguageServer.Tests.CompletionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``completion works in a .cs file`` () =
    let client = Fixtures.getShared "testCompletionWorks"

    // resolve provider is necessary for lsp client to resolve
    // detail and documentation props for a completion item
    let haveResolveProvider =
        client.GetState().ServerCapabilities
        |> Option.bind _.CompletionProvider
        |> Option.bind _.ResolveProvider
        |> Option.defaultValue false

    Assert.IsTrue(haveResolveProvider)

    use classFile = client.Open("Project/Class.cs")

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

            Assert.AreEqual(itemResolved.Detail, Some "void Class.MethodA(string arg)")
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
    let client = Fixtures.getShared "testCompletionWorks"
    use classFile = client.Open("Project/ClassWithExtensionMethods.cs")

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

            Assert.AreEqual(itemResolved.Detail, Some "(extension) string ClassWithExtensionMethods.MethodB()")
            Assert.IsFalse(itemResolved.Documentation.IsSome)

    | _ -> failwith "Some U2.C1 was expected"
