namespace CSharpLanguageServer.Tests

open System
open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Tooling.SemanticTokens

[<TestFixture>]
type SemanticTokenTests() =
    static let client: ClientController =
        setupServerClient defaultClientProfile "TestData/testSemanticTokens"

    [<OneTimeSetUp>]
    member _.Setup() = client.StartAndWaitForSolutionLoad()

    [<Test>]
    member _.``textDocument/semanticTokens/full work``() =
        let semanticTokensOptions =
            client.GetState().ServerCapabilities
            |> Option.bind _.SemanticTokensProvider
            |> Option.bind (fun s ->
                match s with
                | U2.C1 c1 -> Some c1
                | _ -> None)

        Assert.IsTrue semanticTokensOptions.IsSome

        let legend = semanticTokensOptions.Value.Legend
        Assert.AreEqual([| "static" |], legend.TokenModifiers)
        Assert.AreEqual(18, legend.TokenTypes.Length)

        // Make sure the server exposes the capability.
        let haveFullSemanticTokenCapability =
            semanticTokensOptions
            |> Option.bind _.Full
            |> Option.bind (fun f ->
                match f with
                | U2.C1 full -> Some full
                | _ -> None)
            |> Option.defaultValue false

        Assert.IsTrue haveFullSemanticTokenCapability

        use file = client.Open "Project/Program.cs"

        let semanticTokenParams: SemanticTokensParams =
            { PartialResultToken = None
              WorkDoneToken = None
              TextDocument = { Uri = file.Uri } }

        let semanticToken: SemanticTokens =
            client.Request("textDocument/semanticTokens/full", semanticTokenParams)

        Assert.IsTrue(semanticToken.ResultId.IsNone)

        let tokens = semanticToken |> (decodeSemanticToken legend)
        Assert.AreEqual(123, tokens.Length)

        Assert.AreEqual(
            [| { Line = 0u
                 StartChar = 0u
                 Length = 5u
                 TokenType = "keyword"
                 TokenModifiers = [||] }
               { Line = 0u
                 StartChar = 6u
                 Length = 6u
                 TokenType = "namespace"
                 TokenModifiers = [||] }
               { Line = 2u
                 StartChar = 0u
                 Length = 9u
                 TokenType = "keyword"
                 TokenModifiers = [||] }
               { Line = 2u
                 StartChar = 10u
                 Length = 7u
                 TokenType = "namespace"
                 TokenModifiers = [||] }
               { Line = 4u
                 StartChar = 4u
                 Length = 9u
                 TokenType = "keyword"
                 TokenModifiers = [||] }
               { Line = 4u
                 StartChar = 14u
                 Length = 10u
                 TokenType = "interface"
                 TokenModifiers = [||] }
               { Line = 6u
                 StartChar = 8u
                 Length = 6u
                 TokenType = "keyword"
                 TokenModifiers = [||] }
               { Line = 6u
                 StartChar = 15u
                 Length = 11u
                 TokenType = "method"
                 TokenModifiers = [||] } |],
            tokens |> Array.take 8
        )

    [<Test>]
    member _.``textDocument/semanticTokens/full work with multi-line literals``() =
        let semanticTokensOptions =
            client.GetState().ServerCapabilities
            |> Option.bind _.SemanticTokensProvider
            |> Option.bind (fun s ->
                match s with
                | U2.C1 c1 -> Some c1
                | _ -> None)

        let legend = semanticTokensOptions.Value.Legend

        use file = client.Open "Project/MultiLineToken.cs"
        let len = file.GetFileContents().Length

        let semanticTokenParams: SemanticTokensParams =
            { PartialResultToken = None
              WorkDoneToken = None
              TextDocument = { Uri = file.Uri } }

        let semanticToken: SemanticTokens =
            client.Request("textDocument/semanticTokens/full", semanticTokenParams)

        // Test if anything is out-of-bound (that might indicate an underflow)
        Assert.IsFalse(
            semanticToken.Data
            |> Array.chunkBySize 5
            |> Array.fold (fun st -> fun datum -> st || datum[2] > uint32 len) false
        )
