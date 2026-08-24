module CSharpLanguageServer.Tests.WorkspaceFolderTests

open System
open System.IO
open System.Text.RegularExpressions

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open NUnit.Framework

open CSharpLanguageServer.Lsp.WorkspaceFolder

let private testFilePath name =
    Path.Combine(Path.GetTempPath(), "CsLsWorkspaceFolderTests", name)

let private fileUri (path: string) = Uri(path).AbsoluteUri

/// Encode the drive colon the way VS Code does (file:///c%3A/...). On
/// non-Windows paths (no drive letter) this is a no-op.
let private vsCodeStyleUri (path: string) =
    Regex.Replace(fileUri path, "^file:///([A-Za-z]):", "file:///$1%3A")

let private addSourceDocument (solution: Solution) (projectId: ProjectId) name filePath =
    let docInfo =
        DocumentInfo.Create(
            DocumentId.CreateNewId(projectId),
            name,
            filePath = filePath,
            loader =
                TextLoader.From(TextAndVersion.Create(SourceText.From("class Something {}"), VersionStamp.Create()))
        )

    solution.AddDocument(docInfo)

let private makeWorkspaceFolder (solution: Solution) =
    { LspWorkspaceFolder.Empty with
        Solution = Loaded(solution.Workspace, solution) }

let private makeSolutionWithProject () =
    let ws = new AdhocWorkspace()
    let project = ws.AddProject("Project", LanguageNames.CSharp)
    ws.CurrentSolution, project.Id

[<Test>]
let ``document resolves by its file uri`` () =
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "Plain.cs"
    let solution = addSourceDocument solution projectId "Plain.cs" path
    let wf = makeWorkspaceFolder solution

    match workspaceFolderDocument AnyDocument (fileUri path) wf with
    | Some doc -> Assert.That(doc.FilePath, Is.EqualTo(path))
    | None -> Assert.Fail("document should resolve by its own uri")

[<Test>]
let ``document resolves by a vscode style uri with an encoded drive colon`` () =
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "Encoded.cs"
    let solution = addSourceDocument solution projectId "Encoded.cs" path
    let wf = makeWorkspaceFolder solution

    match workspaceFolderDocument AnyDocument (vsCodeStyleUri path) wf with
    | Some doc -> Assert.That(doc.FilePath, Is.EqualTo(path))
    | None -> Assert.Fail("document should resolve from a %3A-encoded uri")

[<Test>]
let ``document resolves from a differently cased uri`` () =
    // Roslyn's file-path index compares with StringComparer.OrdinalIgnoreCase
    // on every platform, so a client that cases the path differently still
    // resolves. (System.Uri equality is also case-insensitive, so this
    // matches the previous scan.)
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "Cased.cs"
    let solution = addSourceDocument solution projectId "Cased.cs" path
    let wf = makeWorkspaceFolder solution

    match workspaceFolderDocument AnyDocument (fileUri (path.ToUpperInvariant())) wf with
    | Some doc -> Assert.That(doc.FilePath, Is.EqualTo(path))
    | None -> Assert.Fail("document should resolve regardless of path casing")

[<Test>]
let ``a path shared by two projects resolves to none`` () =
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "Linked.cs"
    let solution = addSourceDocument solution projectId "Linked.cs" path

    let otherProject =
        ProjectInfo.Create(ProjectId.CreateNewId(), VersionStamp.Create(), "Other", "Other", LanguageNames.CSharp)

    let solution = solution.AddProject(otherProject)
    let solution = addSourceDocument solution otherProject.Id "Linked.cs" path
    let wf = makeWorkspaceFolder solution

    Assert.That(
        (workspaceFolderDocument AnyDocument (fileUri path) wf).IsNone,
        Is.True,
        "an ambiguous path (linked into two projects) should resolve to None"
    )

[<Test>]
let ``a path that is also an additional document still resolves to the source document`` () =
    // GetDocumentIdsWithFilePath returns ids for ALL document kinds, so the
    // lookup must not treat a source document + additional document pair as
    // ambiguous: only regular source documents count.
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "Dual.cs"
    let solution = addSourceDocument solution projectId "Dual.cs" path

    let solution =
        solution.AddAdditionalDocument(
            DocumentId.CreateNewId(projectId),
            "Dual.cs",
            SourceText.From("not source"),
            filePath = path
        )

    let wf = makeWorkspaceFolder solution

    match workspaceFolderDocument AnyDocument (fileUri path) wf with
    | Some doc -> Assert.That(doc.FilePath, Is.EqualTo(path))
    | None -> Assert.Fail("the source document should win over a same-path additional document")

[<Test>]
let ``an additional-document-only path resolves to none`` () =
    let solution, projectId = makeSolutionWithProject ()
    let path = testFilePath "OnlyAdditional.txt"

    let solution =
        solution.AddAdditionalDocument(
            DocumentId.CreateNewId(projectId),
            "OnlyAdditional.txt",
            SourceText.From("data"),
            filePath = path
        )

    let wf = makeWorkspaceFolder solution

    Assert.That(
        (workspaceFolderDocument AnyDocument (fileUri path) wf).IsNone,
        Is.True,
        "a path known only as an additional document is not a source document"
    )

[<Test>]
let ``workspace folder routing matches a differently cased uri on windows`` () =
    let workspace =
        { CSharpLanguageServer.Lsp.Workspace.LspWorkspace.Empty with
            Folders =
                [ { LspWorkspaceFolder.Empty with
                      Uri = "file:///C:/Temp/CasingRoot" } ] }

    let resolved =
        workspace
        |> CSharpLanguageServer.Lsp.Workspace.workspaceFolder "file:///c:/temp/casingroot/File.cs"

    match Environment.OSVersion.Platform with
    | PlatformID.Win32NT ->
        Assert.That(
            resolved.IsSome,
            Is.True,
            "windows filesystems are case-insensitive, so routing should match a differently cased uri"
        )
    | _ ->
        Assert.That(
            resolved.IsNone,
            Is.True,
            "path casing is significant on non-windows filesystems, so routing should not match"
        )
