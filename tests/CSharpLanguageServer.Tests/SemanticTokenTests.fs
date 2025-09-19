module CSharpLanguageServer.Tests.SemanticTokenTests

open System
open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

type DecodedToken =
    { Line: uint
      StartChar: uint
      Length: uint
      TokenType: string
      TokenModifiers: string[] }

let decodeSemanticToken legend (semanticToken: SemanticTokens) : DecodedToken[] =
    let decodeTokenWithLegend (legend: SemanticTokensLegend) (token: uint[]) : DecodedToken =
        if token.Length <> 5 then
            failwith "invalid size of `token`, must be 5!"

        let bitIndexes (n: int) =
            seq {
                let mutable x = n
                let mutable i = 0

                while x <> 0 do
                    if (x &&& 1) <> 0 then
                        yield i

                    x <- x >>> 1
                    i <- i + 1
            }
            |> Seq.toArray

        let tokenModifiers =
            token[4] |> int |> bitIndexes |> Array.map (fun i -> legend.TokenModifiers[i])

        { Line = token[0]
          StartChar = token[1]
          Length = token[2]
          TokenType = legend.TokenTypes[int token[3]]
          TokenModifiers = tokenModifiers }

    let offsetMappingFn (prevTok: option<DecodedToken>) (tok: DecodedToken) =
        let mappedTok =
            match prevTok with
            | None -> tok
            | Some prevTok ->
                match tok.Line with
                | 0u ->
                    { tok with
                        Line = prevTok.Line
                        StartChar = prevTok.StartChar + tok.StartChar }
                | _ ->
                    { tok with
                        Line = prevTok.Line + tok.Line }

        mappedTok, Some mappedTok

    let tokens, _ =
        semanticToken.Data
        |> Seq.chunkBySize 5
        |> Seq.map (decodeTokenWithLegend legend)
        |> Seq.mapFold offsetMappingFn None

    tokens |> Array.ofSeq


[<TestCase>]
let testSemanticTokens () =
    use client = setupServerClient defaultClientProfile "TestData/testSemanticTokens"
    client.StartAndWaitForSolutionLoad()

    let semanticTokensOptions =
        client.GetState().ServerCapabilities
        |> Option.bind (fun c -> c.SemanticTokensProvider)
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
        |> Option.bind (fun c1 -> c1.Full)
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


[<TestCase>]
let testSemanticTokensWithMultiLineLiteral () =
    use client =
        setupServerClient defaultClientProfile "TestData/testSemanticTokensWithMultiLineLiteral"

    client.StartAndWaitForSolutionLoad()

    let semanticTokensOptions =
        client.GetState().ServerCapabilities
        |> Option.bind (fun c -> c.SemanticTokensProvider)
        |> Option.bind (fun s ->
            match s with
            | U2.C1 c1 -> Some c1
            | _ -> None)

    let legend = semanticTokensOptions.Value.Legend

    use file = client.Open "Project/Program.cs"
    let len = file.GetFileContents().Length

    let semanticTokenParams: SemanticTokensParams =
        { PartialResultToken = None
          WorkDoneToken = None
          TextDocument = { Uri = file.Uri } }

    let semanticToken: SemanticTokens =
        client.Request("textDocument/semanticTokens/full", semanticTokenParams)

    // Test if anything is out-of-bound (that might indicates an underflow)
    Assert.IsFalse(
        semanticToken.Data
        |> Array.chunkBySize 5
        |> Array.fold (fun st -> fun datum -> st || datum[2] > uint32 len) false
    )

    let extraLF = uint (Environment.NewLine.Length - 1)

    let expectedTokens =
        [| { Line = 0u
             StartChar = 0u
             Length = 3u
             TokenType = "keyword"
             TokenModifiers = [||] }
           { Line = 0u
             StartChar = 4u
             Length = 1u
             TokenType = "variable"
             TokenModifiers = [||] }
           { Line = 0u
             StartChar = 6u
             Length = 1u
             TokenType = "operator"
             TokenModifiers = [||] }
           { Line = 0u
             StartChar = 8u
             Length = 5u + extraLF
             TokenType = "string"
             TokenModifiers = [||] }
           { Line = 1u
             StartChar = 0u
             Length = 24u + extraLF
             TokenType = "string"
             TokenModifiers = [||] }
           { Line = 2u
             StartChar = 15u
             Length = 4u + extraLF
             TokenType = "string"
             TokenModifiers = [||] } |]

    let tokens = semanticToken |> (decodeSemanticToken legend)
    Assert.AreEqual(expectedTokens, tokens)
