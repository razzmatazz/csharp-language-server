module CSharpLanguageServer.Tests.DocumentFormattingTests

open System.IO

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase>]
let testEditorConfigFormatting () =
    use client =
        setupServerClient defaultClientProfile "TestData/testEditorConfigFormatting"

    client.StartAndWaitForSolutionLoad()

    use classFile = client.Open("Project/Class.cs")

    let docFormattingParams0: DocumentFormattingParams =
        { TextDocument = { Uri = classFile.Uri }
          WorkDoneToken = None
          Options =
            { TabSize = 8u
              InsertSpaces = false
              TrimTrailingWhitespace = Some true
              InsertFinalNewline = Some true
              TrimFinalNewlines = Some true } }

    let textEdits: TextEdit[] option =
        client.Request("textDocument/formatting", docFormattingParams0)

    match textEdits with
    | Some tes ->
        let expectedClassContents =
            File.ReadAllText(Path.Combine(client.ProjectDir, "Project", "ExpectedFormatting.cs.txt"))

        Assert.AreEqual(expectedClassContents, classFile.GetFileContentsWithTextEditsApplied(tes))
    | None -> failwith "Some TextEdit's were expected"
