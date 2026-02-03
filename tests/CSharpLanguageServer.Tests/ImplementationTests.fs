module CSharpLanguageServer.Tests.ImplementationTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let testTextDocumentImplementationWorks () =
    use client = activateFixture "genericProject"
    use semanticTokenFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    // Test finding implementations of IGreetable.GetGreeting() interface method
    // Line 6 (0-indexed), Character 15 is on "GetGreeting" in "string GetGreeting();"
    let implementationParams0: ImplementationParams =
        { TextDocument = { Uri = semanticTokenFile.Uri }
          Position = { Line = 6u; Character = 15u }
          WorkDoneToken = None
          PartialResultToken = None }

    let implementation0: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/implementation", implementationParams0)

    // Should find implementations in Person (line 18) and Student (line 33)
    match implementation0 with
    | Some(U2.C1(U2.C2 locations)) ->
        Assert.AreEqual(1, locations.Length)
        // Verify first implementation (Person.GetGreeting)
        Assert.AreEqual(semanticTokenFile.Uri, locations[0].Uri)
        Assert.AreEqual(18u, locations[0].Range.Start.Line)

    | _ -> failwithf "Some Location[] was expected but %s received" (string implementation0)
