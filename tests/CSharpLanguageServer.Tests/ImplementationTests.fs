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

[<Test>]
let testImplementationOnConcreteClassWorks () =
    // PR #360: FindDerivedClassesAsync is now called for class symbols.
    // Cursor on `Person` (concrete class) should return Student as a derived class.
    use client = activateFixture "genericProject"
    use hierarchyFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    // Line 9 (0-indexed), Character 6 is on "Person" in "class Person : IGreetable"
    let implParams: ImplementationParams =
        { TextDocument = { Uri = hierarchyFile.Uri }
          Position = { Line = 9u; Character = 6u }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/implementation", implParams)

    // Should find Student (line 24) as the class derived from Person
    match result with
    | Some(U2.C1(U2.C2 locations)) ->
        Assert.AreEqual(1, locations.Length)
        Assert.AreEqual(hierarchyFile.Uri, locations[0].Uri)
        Assert.AreEqual(24u, locations[0].Range.Start.Line)

    | _ -> failwithf "Some Location[] was expected but %s received" (string result)

[<Test>]
let testImplementationOnAbstractClassWorks () =
    // PR #360: Cursor on an abstract class symbol should return all derived classes.
    // `Animal` (abstract, line 39) has one direct subclass: Dog (line 44).
    use client = activateFixture "genericProject"
    use hierarchyFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    // Line 39 (0-indexed), Character 15 is on "Animal" in "abstract class Animal"
    let implParams: ImplementationParams =
        { TextDocument = { Uri = hierarchyFile.Uri }
          Position = { Line = 39u; Character = 15u }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/implementation", implParams)

    // Should find Dog (line 44) as the class derived from Animal
    match result with
    | Some(U2.C1(U2.C2 locations)) ->
        Assert.AreEqual(1, locations.Length)
        Assert.AreEqual(hierarchyFile.Uri, locations[0].Uri)
        Assert.AreEqual(44u, locations[0].Range.Start.Line)

    | _ -> failwithf "Some Location[] was expected but %s received" (string result)

[<Test>]
let testImplementationOnInterfaceTypeWorks () =
    // Regression check for PR #360: cursor on an interface type (not a method) should
    // still return the implementing class (Person), not break due to the new class-path.
    use client = activateFixture "genericProject"
    use hierarchyFile = client.Open "Project/ClassAndInterfaceHierarchy.cs"

    // Line 4 (0-indexed), Character 10 is on "IGreetable" in "interface IGreetable"
    let implParams: ImplementationParams =
        { TextDocument = { Uri = hierarchyFile.Uri }
          Position = { Line = 4u; Character = 10u }
          WorkDoneToken = None
          PartialResultToken = None }

    let result: U2<Definition, DefinitionLink array> option =
        client.Request("textDocument/implementation", implParams)

    // FindImplementationsAsync on an interface type returns all classes in the
    // hierarchy that carry the implementation: Person (direct implementor, line 9)
    // and Student (inherits the implementation transitively, line 24).
    match result with
    | Some(U2.C1(U2.C2 locations)) ->
        let lines = locations |> Array.map (fun l -> l.Range.Start.Line) |> Array.sort
        Assert.AreEqual(2, locations.Length)
        Assert.AreEqual([| 9u; 24u |], lines)

    | _ -> failwithf "Some Location[] was expected but %s received" (string result)
