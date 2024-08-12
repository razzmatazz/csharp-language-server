module CSharpLanguageServer.Tests.InitializationTests

open NUnit.Framework

open CSharpLanguageServer.Tests.Tooling
open Ionide.LanguageServerProtocol.Types

[<TestCase>]
let testServerRegistersCapabilitiesWithTheClient () =
    let projectFiles =
        Map.ofList [
          ("Project/Project.csproj", dotnet8PExeProjectCsproj)
          ("Project/Class.cs", """class Class {}""")
        ]

    use client = setupServerClient defaultClientProfile projectFiles
    client.StartAndWaitForSolutionLoad()

    let serverInfo = client.GetState().ServerInfo.Value
    Assert.AreEqual("csharp-ls", serverInfo.Name)

    let serverCaps = client.GetState().ServerCapabilities.Value

    Assert.AreEqual(
        { Change            = Some TextDocumentSyncKind.Incremental
          OpenClose         = Some true
          Save              = Some (U2.C2 { IncludeText = Some true })
          WillSave          = None
          WillSaveWaitUntil = None
        }
        |> U2<TextDocumentSyncOptions,TextDocumentSyncKind>.C1 |> Some,
        serverCaps.TextDocumentSync)

    Assert.AreEqual(
        null,
        serverCaps.Workspace)

    Assert.AreEqual(
        true |> U2<bool, HoverOptions>.C1 |> Some,
        serverCaps.HoverProvider)

    Assert.AreEqual(
        true |> U3<bool, ImplementationOptions, ImplementationRegistrationOptions>.C1 |> Some,
        serverCaps.ImplementationProvider)

    Assert.AreEqual(
        true |> U2<bool, DocumentSymbolOptions>.C1 |> Some,
        serverCaps.DocumentSymbolProvider)

    Assert.AreEqual(
        true |> U2<bool, DefinitionOptions>.C1 |> Some,
        serverCaps.DefinitionProvider)

    Assert.AreEqual(
        null,
        serverCaps.InlineValueProvider)

    Assert.AreEqual(
        { DocumentSelector      = Some [|U2.C1 { Language = None
                                                 Scheme   = Some "file"
                                                 Pattern  = Some "**/*.cs" }|]
          WorkDoneProgress      = None
          Identifier            = None
          InterFileDependencies = false
          WorkspaceDiagnostics  = false
          Id                    = None
        }
        |> U2<DiagnosticOptions, DiagnosticRegistrationOptions>.C2 |> Some,
        serverCaps.DiagnosticProvider)

    Assert.AreEqual(
        true |> U2<bool, DocumentHighlightOptions>.C1 |> Some,
        serverCaps.DocumentHighlightProvider)

    Assert.AreEqual(
        { WorkDoneProgress    = None
          TriggerCharacters   = Some [|"."; "'"|]
          AllCommitCharacters = None
          ResolveProvider     = None
          CompletionItem      = None }
        |> Some,
        serverCaps.CompletionProvider)

    Assert.AreEqual(
        { WorkDoneProgress = None
          CodeActionKinds  = None
          ResolveProvider  = Some true }
        |> U2<bool, CodeActionOptions>.C2 |> Some,
        serverCaps.CodeActionProvider)

    Assert.AreEqual(
        true |> U2<bool, RenameOptions>.C1 |> Some,
        serverCaps.RenameProvider)

    Assert.AreEqual(
        null,
        serverCaps.DeclarationProvider)

    Assert.AreEqual(
        true |> U2<bool, DocumentFormattingOptions>.C1 |> Some,
        serverCaps.DocumentFormattingProvider)

    Assert.AreEqual(
        true |> U2<bool, ReferenceOptions>.C1 |> Some,
        serverCaps.ReferencesProvider)

    Assert.AreEqual(
        true |> U2<bool, WorkspaceSymbolOptions>.C1 |> Some,
        serverCaps.WorkspaceSymbolProvider)

    Assert.AreEqual(
        { WorkDoneProgress    = None
          TriggerCharacters   = Some [|"("; ","; "<"; "{"; "["|]
          RetriggerCharacters = None }
        |> Some,
        serverCaps.SignatureHelpProvider)

    Assert.AreEqual(
        null,
        serverCaps.MonikerProvider)

    Assert.IsTrue(client.ServerDidRespondTo("initialize"))
    Assert.IsTrue(client.ServerDidRespondTo("initialized"))
