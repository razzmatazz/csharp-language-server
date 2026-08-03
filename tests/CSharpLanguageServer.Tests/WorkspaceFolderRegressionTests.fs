module CSharpLanguageServer.Tests.WorkspaceFolderRegressionTests

open System.IO

open Microsoft.CodeAnalysis
open NUnit.Framework

open CSharpLanguageServer.Lsp.WorkspaceFolder

[<Test>]
let ``loose document attaches to nearest containing project`` () =
    use workspace = new AdhocWorkspace()

    let appDirectory = Path.Combine(Path.GetTempPath(), "App")
    let testsDirectory = Path.Combine(appDirectory, "Tests")
    let parentProjectPath = Path.Combine(appDirectory, "App.csproj")
    let nearestProjectPath = Path.Combine(testsDirectory, "Tests.csproj")
    let looseDocumentPath = Path.Combine(testsDirectory, "Scratch.cs")

    let projectInfo name filePath =
        ProjectInfo.Create(
            ProjectId.CreateNewId(),
            VersionStamp.Create(),
            name,
            name,
            LanguageNames.CSharp,
            filePath = filePath
        )

    let solution =
        workspace.CurrentSolution
            .AddProject(projectInfo "App" parentProjectPath)
            .AddProject(projectInfo "Tests" nearestProjectPath)

    let workspaceFolder =
        { LspWorkspaceFolder.Empty with
            Solution = Loaded(workspace, solution) }

    let document, _ =
        workspaceFolderDocumentAdd looseDocumentPath "class Scratch {}" workspaceFolder

    match document with
    | Some document -> Assert.AreEqual(nearestProjectPath, document.Project.FilePath)
    | None -> Assert.Fail("the loose document should be attached to a containing project")
