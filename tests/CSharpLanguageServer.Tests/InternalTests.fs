module CSharpLanguageServer.Tests.InternalTests

open System
open NUnit.Framework

open CSharpLanguageServer.RoslynHelpers

[<TestCase("1.csproj:net8.0", "net8.0")>]
[<TestCase("1.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0,netstandard2.0", "net8.0")>]
[<TestCase("1.csproj:netstandard1.0,netstandard2.0", "netstandard2.0")>]
[<TestCase("1.csproj:net40,net462,net6.0,net8.0,netcoreapp3.1,netstandard2.0", "net8.0")>]
[<TestCase("1.csproj:net40,net462", "net462")>]
[<TestCase("1.csproj:net8.0 2.csproj:net8.0", "net8.0")>]
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:netstandard2.0,net462", null)>]
[<TestCase("1.csproj:net8.0,net10.0 2.csproj:net8.0,net10.0", "net10.0")>]
[<TestCase("1.csproj:net8.0 2.csproj:net8.0,net10.0", "net8.0")>]
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

    let props = Map.empty |> applyWorkspaceTargetFrameworkProp tfmsPerProject

    Assert.AreEqual(expectedTfm |> Option.ofObj, props |> Map.tryFind "TargetFramework")

[<TestCase>]
let testApplyWorkspaceTargetFrameworkPropWithEmptyMap () =
    let props = Map.empty |> applyWorkspaceTargetFrameworkProp Map.empty

    Assert.AreEqual(None, props |> Map.tryFind "TargetFramework")
