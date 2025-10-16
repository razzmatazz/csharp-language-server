module CSharpLanguageServer.Tests.CompletionTests

open Xunit
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Fact>]
let ``completion works in a .cs file``(fixture: CompletionServerClientFixture) =
    let client = fixture.Client

    // resolve provider is necessary for lsp client to resolve
    // detail and documentation props for a completion item
    let haveResolveProvider =
        client.GetState().ServerCapabilities
        |> Option.bind _.CompletionProvider
        |> Option.bind _.ResolveProvider
        |> Option.defaultValue false

    Assert.True(haveResolveProvider)

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
        Assert.True(cl.IsIncomplete)
        Assert.False(cl.ItemDefaults.IsSome)
        Assert.Equal(6, cl.Items.Length)

        let methodAItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodA")

        match methodAItem with
        | None -> failwith "an item with Label 'MethodA' was expected for completion at this position"
        | Some item ->
            Assert.Equal(item.Label, "MethodA")
            Assert.False(item.Detail.IsSome)
            Assert.False(item.Documentation.IsSome)
            Assert.False(item.Tags.IsSome)
            Assert.Equal(item.InsertText, Some "MethodA")
            Assert.Equal(Some CompletionItemKind.Method, item.Kind)
            Assert.Equal(Some "MethodA", item.SortText)
            Assert.Equal(Some "MethodA", item.FilterText)
            Assert.Equal(None, item.InsertTextFormat)
            Assert.False(item.CommitCharacters.IsSome)
            Assert.False(item.TextEdit.IsSome)
            Assert.True(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.Equal(itemResolved.Detail, Some "void Class.MethodA(string arg)")
            Assert.False(itemResolved.Documentation.IsSome)

        let getHashCodeItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "GetHashCode")

        match getHashCodeItem with
        | None -> failwith "an item with Label 'GetHashCode' was expected for completion at this position"
        | Some item ->
            Assert.Equal(item.Label, "GetHashCode")
            Assert.False(item.Detail.IsSome)
            Assert.False(item.Documentation.IsSome)
            Assert.False(item.Tags.IsSome)
            Assert.Equal(item.InsertText, Some "GetHashCode")
            Assert.Equal(Some CompletionItemKind.Method, item.Kind)
            Assert.Equal(Some "GetHashCode", item.SortText)
            Assert.Equal(Some "GetHashCode", item.FilterText)
            Assert.Equal(None, item.InsertTextFormat)
            Assert.False(item.CommitCharacters.IsSome)
            Assert.False(item.TextEdit.IsSome)
            Assert.True(item.Data.IsSome)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.Equal(itemResolved.Detail, Some "int object.GetHashCode()")
            Assert.True(itemResolved.Documentation.IsSome)

            match itemResolved.Documentation with
            | Some(U2.C2 markup) ->
                Assert.Equal(MarkupKind.PlainText, markup.Kind)
                Assert.Equal("Serves as the default hash function.", markup.Value)
            | _ -> failwith "Documentation w/ Kind=Markdown was expected for GetHashCode"

            ()

    | _ -> failwith "Some U2.C1 was expected"

    ()

[<Fact>]
let ``completion works for extension methods``(fixture: CompletionServerClientFixture) =
    let client = fixture.Client

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
        Assert.Equal(7, cl.Items.Length)

        let methodBItem = cl.Items |> Seq.tryFind (fun i -> i.Label = "MethodB")

        match methodBItem with
        | None -> failwith "an item with Label 'MethodB' was expected for completion at this position"
        | Some item ->
            Assert.Equal("MethodB", item.Label)
            Assert.False(item.Detail.IsSome)
            Assert.False(item.Documentation.IsSome)
            Assert.Equal(Some CompletionItemKind.Method, item.Kind)

            let itemResolved: CompletionItem = client.Request("completionItem/resolve", item)

            Assert.Equal(itemResolved.Detail, Some "(extension) string ClassWithExtensionMethods.MethodB()")
            Assert.False(itemResolved.Documentation.IsSome)

    | _ -> failwith "Some U2.C1 was expected"
