module CSharpLanguageServer.Tests.InternalTests

open System
open NUnit.Framework
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open CSharpLanguageServer.Roslyn.Solution

[<TestCase("1.csproj:net8.0", "net8.0")>]
[<TestCase("1.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0,netstandard2.0", "net8.0")>]
[<TestCase("1.csproj:netstandard1.0,netstandard2.0", "netstandard2.0")>]
[<TestCase("1.csproj:net40,net462,net6.0,net8.0,netcoreapp3.1,netstandard2.0", "net8.0")>]
[<TestCase("1.csproj:net40,net462,net6.0,net8.0,net8.0-windows,netcoreapp3.1,netstandard2.0", "net8.0-windows")>]
[<TestCase("1.csproj:net40,net462", "net462")>]
[<TestCase("1.csproj:net8.0 2.csproj:net8.0", "net8.0")>]
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:netstandard2.0,net462", "net10.0")>]
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0 2.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0 2.csproj:net9.0-windows", "net9.0-windows")>]
[<TestCase("1.csproj:net9.0 2.csproj:net9.0-windows", "net9.0-windows")>]
let testApplyWorkspaceTargetFrameworkProp (tfmList: string, expectedTfm: string | null) =

    let parseTfmList (projectEntry: string) : string * list<string> =
        let parts = projectEntry.Split(':')

        if parts.Length <> 2 then
            failwithf "Invalid project entry format: '%s'. Expected 'ProjectName:tfm1,tfm2,...'" projectEntry

        let projectName = parts.[0]
        let tfmStrings = parts.[1].Split(',') |> List.ofSeq
        (projectName, tfmStrings)

    let tfmsPerProject: Map<string, list<string>> =
        tfmList
        |> _.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseTfmList
        |> Map.ofArray

    Assert.AreEqual(expectedTfm |> Option.ofObj, workspaceTargetFramework tfmsPerProject)

[<Test>]
let testSymbolGetMetadataNameForGlobalNamespaceType () =
    // Regression test: symbolGetMetadataName' had a loop with `doContinue = true`
    // initialised unconditionally, so it always executed at least one iteration.
    // For a type in the global namespace the first iteration is correct, but it
    // also pushes the empty global-namespace name, producing a spurious leading dot
    // (e.g. ".GlobalType" instead of "GlobalType").
    //
    // The same off-by-one means that if ContainingNamespace is null (which happens
    // for error/synthetic symbols in Razor-generated compilations) the very first
    // iteration crashes with NullReferenceException on ns.Name.
    let source = "class GlobalType {}"
    let tree = CSharpSyntaxTree.ParseText(source)

    let compilation =
        CSharpCompilation.Create(
            "TestAssembly",
            syntaxTrees = [| tree |],
            references = [| MetadataReference.CreateFromFile(typeof<obj>.Assembly.Location) |]
        )

    let globalType = compilation.GetTypeByMetadataName("GlobalType")

    match globalType |> Option.ofObj with
    | None -> Assert.Fail("GlobalType should be resolvable")
    | Some globalType ->
        let result = CSharpLanguageServer.Roslyn.Symbol.symbolGetMetadataName globalType
        // Before fix: returns ".GlobalType" (spurious leading dot from the empty global ns name)
        // After fix:  returns "GlobalType"
        Assert.AreEqual("GlobalType", result)
