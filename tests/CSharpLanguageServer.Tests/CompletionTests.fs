module CSharpLanguageServer.Tests.CompletionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testCompletionWorks () =
    use client = setupServerClient defaultClientProfile
                                   "TestData/testCompletionWorks"
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    let completionParams0: CompletionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 13u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = None
        }

    let completion0: U2<CompletionItem array, CompletionList> option =
        client.Request("textDocument/completion", completionParams0)

    let mutable completionItemForMethodA: option<CompletionItem> = None

    match completion0 with
    | Some (U2.C2 cl) ->
        Assert.IsTrue(cl.IsIncomplete)
        Assert.IsFalse(cl.ItemDefaults.IsSome)
        Assert.AreEqual(6, cl.Items.Length)

        let methodAItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodA")
        match methodAItem with
        | None -> failwith "an item with Label 'MethodA' was expected for completion at this position"
        | Some item ->
            completionItemForMethodA <- Some item
            Assert.AreEqual(item.Label, "MethodA")
            Assert.AreEqual(item.Detail, Some "void Class.MethodA(string arg)")
            Assert.IsFalse(item.Documentation.IsSome)
            Assert.IsFalse(item.Tags.IsSome)
            Assert.AreEqual(item.InsertText, Some "MethodA")
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            Assert.AreEqual(Some "MethodA", item.SortText)
            Assert.AreEqual(Some "MethodA", item.FilterText)
            Assert.AreEqual(None, item.InsertTextFormat)
            Assert.IsFalse(item.CommitCharacters.IsSome)
            Assert.IsFalse(item.TextEdit.IsSome)
            Assert.IsFalse(item.Data.IsSome)
            ()

        let getHashCodeItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "GetHashCode")
        match getHashCodeItem with
        | None -> failwith "an item with Label 'GetHashCode' was expected for completion at this position"
        | Some item ->
            completionItemForMethodA <- Some item
            Assert.AreEqual(item.Label, "GetHashCode")
            Assert.AreEqual(item.Detail, Some "int object.GetHashCode()")

            match item.Documentation with
            | Some (U2.C2 markup) ->
                Assert.AreEqual(MarkupKind.Markdown, markup.Kind)
                Assert.AreEqual("Serves as the default hash function.", markup.Value)
            | _ -> failwith "Documentation w/ Kind=Markdown was expected for GetHashCode"

            Assert.IsFalse(item.Tags.IsSome)
            Assert.AreEqual(item.InsertText, Some "GetHashCode")
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            Assert.AreEqual(Some "GetHashCode", item.SortText)
            Assert.AreEqual(Some "GetHashCode", item.FilterText)
            Assert.AreEqual(None, item.InsertTextFormat)
            Assert.IsFalse(item.CommitCharacters.IsSome)
            Assert.IsFalse(item.TextEdit.IsSome)
            Assert.IsFalse(item.Data.IsSome)
            ()

    | _ -> failwith "Some U2.C1 was expected"
    ()

[<TestCase>]
let testCompletionWorksForExtensionMethods () =
    use client = setupServerClient defaultClientProfile
                                   "TestData/testCompletionWorksForExtensionMethods"
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    let completionParams0: CompletionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 13u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = None
        }

    let completion0: U2<CompletionItem array, CompletionList> option =
        client.Request("textDocument/completion", completionParams0)

    match completion0 with
    | Some (U2.C2 cl) ->
        Assert.AreEqual(7, cl.Items.Length)

        let methodBItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodB")
        match methodBItem with
        | Some item ->
            Assert.AreEqual("MethodB", item.Label)
            Assert.AreEqual(Some "(extension) string Class.MethodB()", item.Detail)
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            ()

        | _ -> failwith "an item with Label 'MethodB' was expected for completion at this position"

    | _ -> failwith "Some U2.C1 was expected"
