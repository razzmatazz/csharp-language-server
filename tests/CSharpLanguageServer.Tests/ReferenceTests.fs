module CSharpLanguageServer.Tests.ReferenceTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testReferenceWorks() =
    let classCsContents = """class Class
{
    public void MethodA(string arg)
    {
    }

    public void MethodB(string arg)
    {
        MethodA(arg);
    }
}
"""
    let projectFiles =
        Map.ofList [
          ("Project/Project.csproj", dotnet8PExeProjectCsproj)
          ("Project/Class.cs", classCsContents)
        ]

    use client = setupServerClient defaultClientProfile projectFiles
    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    //
    // try references request at MethodA declaration on line 2
    //
    let reference0Params: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 2u; Character = 16u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations0: Location[] option = classFile.Request("textDocument/references", reference0Params)

    let expectedLocations: Location array =
        [|
            { Uri = classFile.Uri
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations, locations0.Value)

    //
    // try references request at empty line line 1 -- should return 0 results
    //
    let reference1Params: ReferenceParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 0u; Character = 0u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations1: Location[] option = classFile.Request("textDocument/references", reference1Params)
    Assert.IsTrue(locations1.IsNone)


let dotnet8WebProjectCsproj = """<Project Sdk="Microsoft.NET.Sdk.Web">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <RootNamespace>test_csharp_web</RootNamespace>
  </PropertyGroup>
</Project>
"""

[<TestCase>]
let testReferenceWorksToAspNetRazorPageReferencedValue() =

    let programCsContents = """var builder = WebApplication.CreateBuilder(args);
builder.Services.AddRazorPages();

var app = builder.Build();
app.UseRouting();
app.MapRazorPages();
app.Run();
"""

    let indexCshtmlContents = """@page
@model IndexModel
<div>@Model.Value</div>
"""

    let indexCshtmlCsContents = """using Microsoft.AspNetCore.Mvc.RazorPages;
namespace test_csharp_web.Pages;
public class IndexModel : PageModel
{
    public string? Value { get; set; }
    public void OnGet()
    {
        Value = "test";
    }
}
"""

    let viewImportsCshtmlContents= """@using test_csharp_web
@namespace test_csharp_web.Pages
@addTagHelper *, Microsoft.AspNetCore.Mvc.TagHelpers
"""
    let projectFiles =
        Map.ofList [
          ("Project/Project.csproj", dotnet8WebProjectCsproj)
          ("Project/Program.cs", programCsContents)
          ("Project/Pages/Index.cshtml", indexCshtmlContents)
          ("Project/Pages/Index.cshtml.cs", indexCshtmlCsContents)
          ("Project/Pages/_ViewImports.cshtml", viewImportsCshtmlContents)
        ]

    use client = setupServerClient defaultClientProfile projectFiles
    client.StartAndWaitForSolutionLoad()

    use indexCshtmlCsFile = client.Open("Project/Pages/Index.cshtml.cs")

    //
    // try references request at MethodA declaration on line 2
    //
    let reference0Params: ReferenceParams =
        { TextDocument = { Uri = indexCshtmlCsFile.Uri }
          Position = { Line = 4u; Character = 19u }
          WorkDoneToken = None
          PartialResultToken = None
          Context = { IncludeDeclaration = false }
        }

    let locations0: Location[] option = indexCshtmlCsFile.Request("textDocument/references", reference0Params)

    System.Console.WriteLine("{0}", locations0)

    let expectedLocations: Location array =
        [|
            { Uri = indexCshtmlCsContents.Uri
              Range = {
                  Start = { Line = 8u; Character = 8u }
                  End = { Line = 8u; Character = 15u }
              }
            }
         |]

    Assert.AreEqual(expectedLocations, locations0.Value)
