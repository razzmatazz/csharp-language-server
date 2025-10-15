module CSharpLanguageServer.Tests.CodeActionTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let ``code action menu appears on request`` () =
    let client = testCodeActionsFixture
    use classFile = client.Open("Project/Class.cs")

    let caParams: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 1u; Character = 0u }
              End = { Line = 1u; Character = 0u } }
          Context =
            { Diagnostics = [||]
              Only = None
              TriggerKind = None }
          WorkDoneToken = None
          PartialResultToken = None }

    let caResult: TextDocumentCodeActionResult option =
        client.Request("textDocument/codeAction", caParams)

    let assertCodeActionHasTitle (ca: CodeAction, title: string) =
        Assert.AreEqual(title, ca.Title)
        Assert.AreEqual(None, ca.Kind)
        Assert.AreEqual(None, ca.Diagnostics)
        Assert.AreEqual(None, ca.Disabled)
        Assert.IsTrue(ca.Edit.IsSome)

    match caResult with
    | Some [| U2.C2 generateOverrides
              U2.C2 extractInterface
              U2.C2 generateConstructor
              U2.C2 extractBaseClass
              U2.C2 addDebuggerDisplay |] ->
        assertCodeActionHasTitle (generateOverrides, "Generate overrides...")
        assertCodeActionHasTitle (extractInterface, "Extract interface...")
        assertCodeActionHasTitle (generateConstructor, "Generate constructor 'Class()'")
        assertCodeActionHasTitle (extractBaseClass, "Extract base class...")
        assertCodeActionHasTitle (addDebuggerDisplay, "Add 'DebuggerDisplay' attribute")

    | _ -> failwith "Not all code actions were matched as expected"


[<Test>]
let ``extract base class request extracts base class`` () =
    let client = testCodeActionsFixture
    use classFile = client.Open("Project/Class.cs")

    let caParams0: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 2u; Character = 16u }
              End = { Line = 2u; Character = 16u } }
          Context =
            { Diagnostics = [||]
              Only = None
              TriggerKind = None }
          WorkDoneToken = None
          PartialResultToken = None }

    let caResult: TextDocumentCodeActionResult option =
        client.Request("textDocument/codeAction", caParams0)

    match caResult with
    | Some [| U2.C2 x |] -> Assert.AreEqual("Extract base class...", x.Title)
    // TODO: match extract base class edit structure

    | _ -> failwith "Some [| U2.C2 x |] was expected"


[<Test>]
let ``extract interface code action should extract an interface`` () =
    let client = testCodeActionsFixture
    use classFile = client.Open("Project/Class.cs")

    let caArgs: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 0u; Character = 0u }
              End = { Line = 0u; Character = 0u } }
          Context =
            { Diagnostics = [||]
              Only = Some [| "refactor.extract.interface" |]
              TriggerKind = Some CodeActionTriggerKind.Invoked }
          WorkDoneToken = None
          PartialResultToken = None }

    let caOptions: TextDocumentCodeActionResult option =
        match client.Request("textDocument/codeAction", caArgs) with
        | Some opts -> opts
        | None -> failwith "Expected code actions"

    let codeAction =
        match caOptions |> Option.bind (Array.tryItem 1) with
        | Some(U2.C2 ca) ->
            Assert.AreEqual("Extract interface...", ca.Title)
            ca
        | _ -> failwith "Extract interface action not found"

    let expectedImplementInterfaceEdits =
        { Range =
            { Start = { Line = 0u; Character = 11u }
              End = { Line = 0u; Character = 11u } }
          NewText = " : IClass" }

    let expectedCreateInterfaceEdits =
        { Range =
            { Start = { Line = 0u; Character = 0u }
              End = { Line = 0u; Character = 0u } }
          NewText = "internal interface IClass\n{\n    void Method(string arg);\n}" }

    match codeAction.Edit with
    | Some { DocumentChanges = Some [| U4.C1 create; U4.C1 implement |] } ->
        match create.Edits, implement.Edits with
        | [| U2.C1 createEdits |], [| U2.C1 implementEdits |] ->
            Assert.AreEqual(expectedCreateInterfaceEdits, createEdits |> TextEdit.normalizeNewText)

            Assert.AreEqual(expectedImplementInterfaceEdits, implementEdits |> TextEdit.normalizeNewText)

        | _ -> failwith "Expected exactly one U2.C1 edit in both create/implement"

    | _ -> failwith "Unexpected edit structure"
