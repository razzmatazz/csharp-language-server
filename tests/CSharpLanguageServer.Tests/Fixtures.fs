module CSharpLanguageServer.Tests.Fixtures

open CSharpLanguageServer.Tests.Tooling


type CodeActionServerClientFixture() =
    let client: ClientController =
        setupServerClient defaultClientProfile "TestData/testCodeActions"

    do client.StartAndWaitForSolutionLoad()

    member _.Client = client


type CompletionServerClientFixture() =
    let client: ClientController =
        setupServerClient defaultClientProfile "TestData/testCompletionWorks"

    do client.StartAndWaitForSolutionLoad()

    member _.Client = client
