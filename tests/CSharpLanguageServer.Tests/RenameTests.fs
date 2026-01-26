module CSharpLanguageServer.Tests.RenameTests

open System
open System.IO

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``rename can be applied to a variable`` () =
    use client = activateFixture "genericProject"

    use classFile = client.Open "Project/Class.cs"

    let prepareParams: PrepareRenameParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 15u }
          WorkDoneToken = None }

    let prepareResult: option<PrepareRenameResult> =
        client.Request("textDocument/prepareRename", prepareParams)

    let expectedPrepareResult: PrepareRenameResult =
        { Range =
            { Start = { Line = 4u; Character = 15u }
              End = { Line = 4u; Character = 18u } }
          Placeholder = "str" }
        |> U3.C2

    Assert.AreEqual(Some expectedPrepareResult, prepareResult)

    let renameParams: RenameParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 15u }
          WorkDoneToken = None
          NewName = "xxx" }

    let renameResult: option<WorkspaceEdit> =
        client.Request("textDocument/rename", renameParams)

    match renameResult with
    | None -> failwith "Some WorkspaceEdit was expected"

    | Some workspaceEdit ->
        let textEdits = workspaceEdit.Changes.Value |> Map.find classFile.Uri

        let expectedClassContents =
            File
                .ReadAllText(Path.Combine(client.SolutionDir, "Project", "Class.cs.str-renamed-to-xxx.txt"))
                .ReplaceLineEndings("\n")

        let actualClassContents =
            classFile.GetFileContentsWithTextEditsApplied(textEdits).ReplaceLineEndings("\n")

        Assert.AreEqual(expectedClassContents, actualClassContents)

[<Test>]
let ``rename can be applied to a variable in .cshtml file`` () =
    use client = activateFixture "aspnetProject"

    use indexCshtmlFile = client.Open "Project/Views/Test/Index.cshtml"

    let prepareParams: PrepareRenameParams =
        { TextDocument = { Uri = indexCshtmlFile.Uri }
          Position = { Line = 3u; Character = 11u }
          WorkDoneToken = None }

    let prepareResult: option<PrepareRenameResult> =
        client.Request("textDocument/prepareRename", prepareParams)

    let expectedPrepareResult: PrepareRenameResult =
        { Range =
            { Start = { Line = 3u; Character = 11u }
              End = { Line = 3u; Character = 11u } }
          Placeholder = "x" }
        |> U3.C2

    Assert.AreEqual(Some expectedPrepareResult, prepareResult)

(*
    let renameParams: RenameParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 4u; Character = 15u }
          WorkDoneToken = None
          NewName = "xxx" }

    let renameResult: option<WorkspaceEdit> =
        client.Request("textDocument/rename", renameParams)

    match renameResult with
    | None -> failwith "Some WorkspaceEdit was expected"

    | Some workspaceEdit ->
        let textEdits = workspaceEdit.Changes.Value |> Map.find classFile.Uri

        let expectedClassContents =
            File
                .ReadAllText(Path.Combine(client.SolutionDir, "Project", "Class.cs.str-renamed-to-xxx.txt"))
                .ReplaceLineEndings("\n")

        let actualClassContents =
            classFile.GetFileContentsWithTextEditsApplied(textEdits).ReplaceLineEndings("\n")

        Assert.AreEqual(expectedClassContents, actualClassContents)
*)
