module CSharpLanguageServer.Tests.InternalTests

open NUnit.Framework

open CSharpLanguageServer.RoslynHelpers

[<TestCase("net8.0", "net8.0")>]
[<TestCase("net8.0;net10.0", "net10.0")>]
[<TestCase("net8.0;netstandard2.0", "net8.0")>]
[<TestCase("netstandard1.0;netstandard2.0", "netstandard2.0")>]
[<TestCase("net40;net462;net6.0;net8.0;netcoreapp3.1;netstandard2.0", "net8.0")>]
[<TestCase("net40;net462", "net462")>]
let testTheLatestTfmIsSelected(tfmList: string, expectedTfm: string) =
    let selectedTfm = tfmList.Split(";") |> selectLatestTfm
    Assert.AreEqual(expectedTfm |> Option.ofObj, selectedTfm)
