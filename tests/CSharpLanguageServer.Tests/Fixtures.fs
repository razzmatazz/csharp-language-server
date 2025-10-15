module CSharpLanguageServer.Tests.Fixtures

open CSharpLanguageServer.Tests.Tooling

let fixture (projectDir: string) : ClientController =
    let client: ClientController = setupServerClient defaultClientProfile projectDir
    client.StartAndWaitForSolutionLoad()
    client

let testCompletionWorksFixture = fixture ("TestData/testCompletionWorks")
let testCodeActionsFixture = fixture ("TestData/testCodeActions")

let testEditorConfigFormattingFixture =
    fixture ("TestData/testEditorConfigFormatting")

let testHoverWorksFixture = fixture ("TestData/testHoverWorks")
let testSemanticTokensFixture = fixture ("TestData/testSemanticTokens")
let testDiagnosticsWorkFixture = fixture ("TestData/testDiagnosticsWork")
let testDefinitionWorksFixture = fixture ("TestData/testDefinitionWorks")
