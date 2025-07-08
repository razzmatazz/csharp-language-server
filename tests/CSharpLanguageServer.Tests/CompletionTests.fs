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
        | Some item ->
            completionItemForMethodA <- Some item
            Assert.IsFalse(item.LabelDetails.IsSome)
            Assert.IsFalse(item.Documentation.IsSome)
            Assert.IsFalse(item.Tags.IsSome)
            Assert.IsFalse(item.Detail.IsSome)
            Assert.AreEqual(Some CompletionItemKind.Method, item.Kind)
            Assert.AreEqual(Some "MethodA", item.SortText)
            Assert.AreEqual(Some "MethodA", item.FilterText)
            Assert.AreEqual(Some InsertTextFormat.PlainText, item.InsertTextFormat)
            Assert.IsFalse(item.CommitCharacters.IsSome)
            Assert.IsFalse(item.TextEdit.IsSome)
            Assert.IsTrue(item.Data.IsSome)
            ()

        | _ -> failwith "an item with Label 'MethodA' was expected for completion at this position"

    | _ -> failwith "Some U2.C1 was expected"

    // now check we can resolve completion item data
    let resolvedItem : CompletionItem =
        client.Request("completionItem/resolve", completionItemForMethodA.Value)

    Assert.IsTrue(resolvedItem.Documentation.IsSome)
    ()
