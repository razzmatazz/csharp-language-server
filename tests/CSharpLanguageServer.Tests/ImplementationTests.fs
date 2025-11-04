module CSharpLanguageServer.Tests.ImplementationTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test textDocument/implementation works`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let implementationParams0: ImplementationParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 10u; Character = 8u }
          WorkDoneToken = None
          PartialResultToken = None }

    let implementation0: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/implementation", implementationParams0)

    // TODO: fix this test
    ()
