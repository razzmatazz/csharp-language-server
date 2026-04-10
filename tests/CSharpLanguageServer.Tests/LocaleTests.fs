module CSharpLanguageServer.Tests.LocaleTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Tests.Tooling

/// Pull diagnostics for the open file and return the message of the third diagnostic,
/// which is the "type or namespace not found" error from the testDiagnosticsWork fixture.
let private getMissingNamespaceDiagnosticMessage (client: LspTestClient) (classFile: LspDocumentHandle) =
    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report with
    | Some(U2.C1 report) when report.Items.Length = 3 -> report.Items.[2].Message
    | Some(U2.C1 report) -> failwithf "Expected 3 diagnostics but got %d" report.Items.Length
    | _ -> failwith "Expected a full DocumentDiagnosticReport"

/// The "type or namespace not found" message as emitted by Roslyn in German.
/// The Roslyn `de` satellite assemblies ship inside the NuGet package and are
/// present in the build output on every platform, so this assertion is reliable
/// regardless of the OS locale (including en-US CI runners).
let private germanMissingNsPrefix = "Der Typ- oder Namespacename"

/// The English variant for comparison in the priority test.
let private englishMissingNsPrefix = "The type or namespace"

[<Test>]
let testLocaleEnvVar () =
    // Set DOTNET_CLI_UI_LANGUAGE=de in the server process environment.
    let profile =
        { defaultClientProfile with
            ExtraEnv = Map.ofList [ "DOTNET_CLI_UI_LANGUAGE", "de" ] }

    use client = activateFixtureExt "testDiagnosticsWork" profile emptyFixturePatch id
    use classFile = client.Open("Project/Class.cs")

    let msg = getMissingNamespaceDiagnosticMessage client classFile

    Assert.IsTrue(
        msg.StartsWith(germanMissingNsPrefix),
        sprintf "Expected German diagnostic (starting with '%s') but got: %s" germanMissingNsPrefix msg
    )

[<Test>]
let testLocaleCliArg () =
    // Pass --locale de as an extra CLI argument to the server.
    let profile =
        { defaultClientProfile with
            ExtraArgs = [ "--locale"; "de" ] }

    use client = activateFixtureExt "testDiagnosticsWork" profile emptyFixturePatch id
    use classFile = client.Open("Project/Class.cs")

    let msg = getMissingNamespaceDiagnosticMessage client classFile

    Assert.IsTrue(
        msg.StartsWith(germanMissingNsPrefix),
        sprintf "Expected German diagnostic (starting with '%s') but got: %s" germanMissingNsPrefix msg
    )

[<Test>]
let testLocaleCliArgTakesPriority () =
    // DOTNET_CLI_UI_LANGUAGE says German, but --locale en-US should win.
    let profile =
        { defaultClientProfile with
            ExtraEnv = Map.ofList [ "DOTNET_CLI_UI_LANGUAGE", "de" ]
            ExtraArgs = [ "--locale"; "en-US" ] }

    use client = activateFixtureExt "testDiagnosticsWork" profile emptyFixturePatch id
    use classFile = client.Open("Project/Class.cs")

    let msg = getMissingNamespaceDiagnosticMessage client classFile

    Assert.IsTrue(
        msg.StartsWith(englishMissingNsPrefix),
        sprintf "Expected English diagnostic (starting with '%s') but got: %s" englishMissingNsPrefix msg
    )

[<Test>]
let testLocaleSetting () =
    // Start the server with no locale override, then push csharp.locale=de via
    // workspace/didChangeConfiguration and verify subsequent diagnostics are German.
    use client = activateFixture "testDiagnosticsWork"
    use classFile = client.Open("Project/Class.cs")

    // Confirm English (or system default) before the change.
    let msgBefore = getMissingNamespaceDiagnosticMessage client classFile

    // Send workspace/didChangeConfiguration with locale=de.
    let configParams: DidChangeConfigurationParams =
        { Settings =
            {| csharp = {| locale = "de" |} |}
            |> Ionide.LanguageServerProtocol.Server.serialize }

    client.Notify("workspace/didChangeConfiguration", configParams)

    // Re-open the file to force fresh diagnostics after the locale change.
    classFile.Change(classFile.GetFileContents())

    let msgAfter = getMissingNamespaceDiagnosticMessage client classFile

    Assert.IsTrue(
        msgAfter.StartsWith(germanMissingNsPrefix),
        sprintf
            "Expected German diagnostic after locale change (starting with '%s') but got: %s (was: %s)"
            germanMissingNsPrefix
            msgAfter
            msgBefore
    )
