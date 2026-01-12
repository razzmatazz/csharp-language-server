module CSharpLanguageServer.Tests.DocumentHighlightTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test textDocument/documentHighlight works in .cs file`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let highlightParams: DocumentHighlightParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 9u; Character = 8u }
          WorkDoneToken = None
          PartialResultToken = None }

    let highlights: DocumentHighlight[] option =
        client.Request("textDocument/documentHighlight", highlightParams)

    let expectedHighlights: DocumentHighlight list =
        [ { Range =
              { Start = { Line = 2u; Character = 16u }
                End = { Line = 2u; Character = 23u } }
            Kind = Some DocumentHighlightKind.Read }

          { Range =
              { Start = { Line = 9u; Character = 8u }
                End = { Line = 9u; Character = 15u } }
            Kind = Some DocumentHighlightKind.Read } ]

    Assert.AreEqual(Some expectedHighlights, highlights |> Option.map List.ofArray)

[<Test>]
let ``test textDocument/documentHighlight works in .cshtml file`` () =
    use client = activateFixture "aspnetProject"
    use indexCshtmlFile = client.Open "Project/Views/Test/Index.cshtml"

    let highlightParams: DocumentHighlightParams =
        { TextDocument = { Uri = indexCshtmlFile.Uri }
          Position = { Line = 1u; Character = 1u }
          WorkDoneToken = None
          PartialResultToken = None }

    let highlights: DocumentHighlight[] option =
        client.Request("textDocument/documentHighlight", highlightParams)

    let expectedHighlights: DocumentHighlight list =
        [ { Range =
              { Start = { Line = 1u; Character = 1u }
                End = { Line = 1u; Character = 6u } }
            Kind = Some DocumentHighlightKind.Read } ]

    Assert.AreEqual(Some expectedHighlights, highlights |> Option.map List.ofArray)
