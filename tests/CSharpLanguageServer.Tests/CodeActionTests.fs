module CSharpLanguageServer.Tests.CodeActionTests

open System

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<TestCase("net10.0")>]
[<TestCase("net8.0")>]
let ``code action menu appears on request`` (tfm: string) =
    let patchFixture = patchFixtureWithTfm tfm

    use client =
        activateFixtureExt "genericProject" defaultClientProfile patchFixture id

    use classFile = client.Open "Project/Class.cs"

    let caParams: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 3u; Character = 0u }
              End = { Line = 3u; Character = 0u } }
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

    match tfm, Environment.OSVersion.Platform with
    | "net8.0", PlatformID.Win32NT -> () // this particular variant fails consistently as of Roslyn 5.0.0
    | "net8.0", PlatformID.Unix -> () // this particular variant fails consistently as of Roslyn 5.0.0

    | _, _ ->
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
    use client = activateFixture "genericProject"
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
    use client = activateFixture "genericProject"
    use classFile = client.Open("Project/Class.cs")

    let caArgs: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 2u; Character = 0u }
              End = { Line = 2u; Character = 0u } }
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
            { Start = { Line = 2u; Character = 11u }
              End = { Line = 2u; Character = 11u } }
          NewText = " : IClass" }

    let expectedCreateInterfaceEdits =
        { Range =
            { Start = { Line = 0u; Character = 0u }
              End = { Line = 0u; Character = 0u } }
          NewText = "internal interface IClass\n{\n    void MethodA(string arg);\n    void MethodB(string arg);\n}" }

    match codeAction.Edit with
    | Some { DocumentChanges = Some [| U4.C1 create; U4.C1 implement |] } ->
        match create.Edits, implement.Edits with
        | [| U2.C1 createEdits |], [| U2.C1 implementEdits |] ->
            Assert.AreEqual(expectedCreateInterfaceEdits, createEdits |> TextEdit.normalizeNewText)

            Assert.AreEqual(expectedImplementInterfaceEdits, implementEdits |> TextEdit.normalizeNewText)

        | _ -> failwith "Expected exactly one U2.C1 edit in both create/implement"

    | _ -> failwith "Unexpected edit structure"

[<Test>]
let ``code actions are listed when activated on a string literal`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let caParams: CodeActionParams =
        { TextDocument = { Uri = classFile.Uri }
          Range =
            { Start = { Line = 6u; Character = 20u }
              End = { Line = 6u; Character = 20u } }
          Context =
            { Diagnostics = [||]
              Only = None
              TriggerKind = None }
          WorkDoneToken = None
          PartialResultToken = None }

    let caResult: TextDocumentCodeActionResult =
        match client.Request("textDocument/codeAction", caParams) with
        | Some caResult -> caResult
        | None -> failwith "Some TextDocumentCodeActionResult was expected"

    Assert.AreEqual(10, caResult.Length)

    match caResult with
    | [| U2.C2 introduceConstant
         U2.C2 introduceConstantForAllOccurences
         U2.C2 introduceLocalConstant
         U2.C2 introduceLocalConstantForAllOccurences
         U2.C2 introduceParameterAndUpdateCallSitesDirectly
         U2.C2 _
         U2.C2 _
         U2.C2 _
         U2.C2 _
         U2.C2 _ |] ->
        let assertCAHasTitle (ca: CodeAction) title = Assert.AreEqual(title, ca.Title)

        assertCAHasTitle introduceConstant "Introduce constant - Introduce constant for '\"\"'"

        assertCAHasTitle
            introduceConstantForAllOccurences
            "Introduce constant - Introduce constant for all occurrences of '\"\"'"

        assertCAHasTitle introduceLocalConstant "Introduce constant - Introduce local constant for '\"\"'"

        assertCAHasTitle
            introduceLocalConstantForAllOccurences
            "Introduce constant - Introduce local constant for all occurrences of '\"\"'"

        assertCAHasTitle
            introduceParameterAndUpdateCallSitesDirectly
            "Introduce parameter for '\"\"' - and update call sites directly"

    | _ -> failwith "Not all code actions were matched as expected"

    ()
