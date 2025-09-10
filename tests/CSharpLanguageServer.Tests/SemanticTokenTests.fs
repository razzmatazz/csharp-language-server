module CSharpLanguageServer.Tests.SemanticTokenTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testSemanticTokenWorks () =
    use client = setupServerClient defaultClientProfile "TestData/testSemanticToken"
    client.StartAndWaitForSolutionLoad()
    let file = client.Open "Project/Program.cs"
    let len = (file.GetFileContentsWithTextEditsApplied Array.empty).Length
    let semanticTokenParams0: SemanticTokensOptionsFullC2 = { Delta = None }
    let semanticToken: SemanticTokens = client.Request("textDocument/semanticTokens/full", semanticTokenParams0)
    Assert.IsFalse(semanticToken.Data
      |> Array.chunkBySize 5
      |> Array.fold (fun st -> fun datum -> st && datum[2] > uint32 len) false)

