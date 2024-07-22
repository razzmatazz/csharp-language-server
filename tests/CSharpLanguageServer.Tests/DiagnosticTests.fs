module CSharpLanguageServer.Tests.Diagnostics

open System.Threading

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testPushDiagnosticsWork () =
    let projectFiles =
        Map.ofList [
          ("Project/Project.csproj",
           """<Project Sdk="Microsoft.NET.Sdk">
                <PropertyGroup>
                  <OutputType>Exe</OutputType>
                  <TargetFramework>net8.0</TargetFramework>
                </PropertyGroup>
              </Project>
           """);
          ("Project/Class.cs",
           """XXX"""
          )
        ]

    use client = setupServerClient
                     { defaultClientProfile with LoggingEnabled = false }
                     projectFiles

    client.StartAndWaitForSolutionLoad()

    let classFile = client.Open("Project/Class.cs")

    Thread.Sleep(5000)

    let state = client.GetState()
    let version, diagnosticList = state.PushDiagnostics |> Map.find classFile.Uri

    Assert.AreEqual(None, version)

    Assert.AreEqual(3, diagnosticList.Length)

    let diagnostic0 = diagnosticList.[0]
    Assert.AreEqual("Identifier expected", diagnostic0.Message)
    Assert.AreEqual(Some DiagnosticSeverity.Error, diagnostic0.Severity)
    Assert.AreEqual(0, diagnostic0.Range.Start.Line)
    Assert.AreEqual(3, diagnostic0.Range.Start.Character)

    let diagnostic1 = diagnosticList.[1]
    Assert.AreEqual("; expected", diagnostic1.Message)

    let diagnostic2 = diagnosticList.[2]
    Assert.AreEqual(
      "The type or namespace name 'XXX' could not be found (are you missing a using directive or an assembly reference?)",
      diagnostic2.Message)
