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
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:netstandard2.0,net462", null)>]
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0 2.csproj:net8.0,net10.0", "net8.0")>]
// A TFM that only some projects declare must never become a workspace-global MSBuild
// property: it would override the other projects' own TargetFramework and invalidate
// their restore output.  See https://github.com/razzmatazz/csharp-language-server/issues/405
[<TestCase("1.csproj:net8.0 2.csproj:net9.0-windows", null)>]
[<TestCase("1.csproj:net9.0 2.csproj:net9.0-windows", null)>]
[<TestCase("1.csproj:net10.0 2.csproj:net10.0-windows", null)>]
[<TestCase("1.csproj:net10.0,net10.0-windows 2.csproj:net10.0", "net10.0")>]
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

    Assert.That(workspaceTargetFramework tfmsPerProject, Is.EqualTo(expectedTfm |> Option.ofObj))

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
        Assert.That(result, Is.EqualTo("GlobalType"))

[<Test>]
let ``describeSolutionLoadFailure names the missing SDK and global.json`` () =
    // Shape of the error Roslyn's build host reports when global.json pins an SDK
    // that is not installed: the hostfxr text, wrapped in an AggregateException.
    let hostfxrText =
        "An exception of type System.InvalidOperationException was thrown: Error while calling hostfxr function hostfxr_resolve_sdk2. Error code: -2147450725 Detailed error: A compatible .NET SDK was not found.\n\n"
        + "Requested SDK version: 10.0.402\n"
        + "global.json file: /src/app/global.json\n\n"
        + "Installed SDKs:\n\n"
        + "Install the [10.0.402] .NET SDK or update [/src/app/global.json] to match an installed SDK.\n"

    let ex = AggregateException(InvalidOperationException(hostfxrText))

    let message = describeSolutionLoadFailure "/src/app/app.sln" ex

    Assert.That(message, Does.Contain(".NET SDK 10.0.402"))
    Assert.That(message, Does.Contain("\"/src/app/global.json\""))
    Assert.That(message, Does.Not.Contain("hostfxr"))
    Assert.That(message, Does.Not.Contain("\n"))

[<Test>]
let ``describeSolutionLoadFailure reports the innermost error's first line`` () =
    let ex =
        AggregateException(InvalidOperationException("outer", ArgumentException("the real problem\nmore detail")))

    let message = describeSolutionLoadFailure "/src/app/app.sln" ex

    Assert.That(message, Does.Contain("the real problem"))
    Assert.That(message, Does.Not.Contain("more detail"))
    Assert.That(message, Does.Not.Contain("outer"))
