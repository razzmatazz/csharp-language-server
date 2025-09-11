module CSharpLanguageServer.Tests.SemanticTokenTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open System
open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testSemanticTokenWorks () =
    use client = setupServerClient defaultClientProfile "TestData/testSemanticToken"
    client.StartAndWaitForSolutionLoad()

    // Make sure the server have right capability.
    let haveFullSemanticTokenCapability =
        client.GetState().ServerCapabilities
        |> Option.bind (fun c -> c.SemanticTokensProvider)
        |> Option.bind (fun s ->
            match s with
            | U2.C1 c1 -> Some c1
            | _ -> None)
        |> Option.bind (fun c1 -> c1.Full)
        |> Option.bind (fun f ->
            match f with
            | U2.C1 full -> Some full
            | _ -> None)
        |> Option.defaultValue false

    Assert.IsTrue haveFullSemanticTokenCapability

    use file = client.Open "Project/Program.cs"
    let len = (file.GetFileContentsWithTextEditsApplied Array.empty).Length

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

    ()
