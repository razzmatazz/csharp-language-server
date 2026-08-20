module CSharpLanguageServer.Tests.DocumentFormattingTests

open System.IO

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.FixturePool

let private formattingOptionsProfile =
    { defaultClientProfile with
        ServerConfig =
            { defaultClientProfile.ServerConfig with
                applyFormattingOptions = Some true } }

[<Test>]
let testEditorConfigFormatting () =
    use client = rentFixture "projectWithEditorConfig"
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
            File
                .ReadAllText(Path.Combine(client.SolutionDir, "Project", "Class.cs.formatted.txt"))
                .ReplaceLineEndings("\n")

        let actualClassContents =
            classFile.GetFileContentsWithTextEditsApplied(tes).ReplaceLineEndings("\n")

        ClassicAssert.AreEqual(expectedClassContents, actualClassContents)
    | None -> failwith "Some TextEdit's were expected"

[<Test>]
let testEofFormattingOptionsNormalizeNewlinesWithoutChangingFinallyPlacement () =
    use client =
        activateFixtureExt "genericProject" formattingOptionsProfile emptyFixturePatch id

    let sourceWithoutFinalNewline =
        """class C
{
    void M()
    {
        try { }
        finally { }
    }
}
"""
        |> fun source -> source.TrimEnd().ReplaceLineEndings("\r\n")

    let sourceWithFinalNewline = sourceWithoutFinalNewline + "\r\n"
    let sourceWithSurplusFinalNewlines = sourceWithoutFinalNewline + "\r\n\r\n\r\n"
    use classFile = client.OpenWithText("Project/Class.cs", sourceWithoutFinalNewline)

    let assertFormatting caseName source insertFinalNewline trimFinalNewlines expected =
        classFile.Change(source)

        let formattingParams: DocumentFormattingParams =
            { TextDocument = { Uri = classFile.Uri }
              WorkDoneToken = None
              Options =
                { TabSize = 4u
                  InsertSpaces = true
                  TrimTrailingWhitespace = None
                  InsertFinalNewline = insertFinalNewline
                  TrimFinalNewlines = trimFinalNewlines } }

        let textEdits: TextEdit[] option =
            client.Request("textDocument/formatting", formattingParams)

        match textEdits with
        | Some textEdits ->
            let actual = classFile.GetFileContentsWithTextEditsApplied(textEdits)
            ClassicAssert.AreEqual(expected, actual, caseName)
        | None -> failwith "Some TextEdit's were expected"

    assertFormatting "insert true" sourceWithoutFinalNewline (Some true) None sourceWithFinalNewline

    assertFormatting
        "insert true with existing newlines"
        sourceWithSurplusFinalNewlines
        (Some true)
        None
        sourceWithSurplusFinalNewlines

    assertFormatting "insert false" sourceWithSurplusFinalNewlines (Some false) None sourceWithSurplusFinalNewlines

    assertFormatting "trim true" sourceWithSurplusFinalNewlines None (Some true) sourceWithFinalNewline

    assertFormatting "trim false" sourceWithoutFinalNewline None (Some false) sourceWithoutFinalNewline

    assertFormatting
        "insert and trim true"
        sourceWithSurplusFinalNewlines
        (Some true)
        (Some true)
        sourceWithFinalNewline

    assertFormatting "options absent" sourceWithSurplusFinalNewlines None None sourceWithSurplusFinalNewlines
