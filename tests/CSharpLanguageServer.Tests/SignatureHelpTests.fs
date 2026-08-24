module CSharpLanguageServer.Tests.SignatureHelpTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.Fixtures

[<Test>]
let ``test textDocument/signatureHelp works`` () =
    use client = rentFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let signatureHelpParams0: SignatureHelpParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 12u; Character = 16u }
          WorkDoneToken = None
          Context = None }

    let signatureHelp0: SignatureHelp option =
        client.Request("textDocument/signatureHelp", signatureHelpParams0)

    match signatureHelp0 with
    | None -> failwith "Some SignatureHelp was expected"
    | Some sh ->
        Assert.That(sh.Signatures.Length, Is.EqualTo(1))

        let expectedSignature0 =
            { Label = "void Class.MethodA(string arg)"
              Documentation =
                Some(
                    U2.C2
                        { Kind = MarkupKind.Markdown
                          Value = "" }
                )
              Parameters =
                Some
                    [| { Label = U2.C1 "string arg"
                         Documentation = None } |]
              ActiveParameter = None }

        Assert.That(sh.Signatures[0], Is.EqualTo(expectedSignature0))

        Assert.That(sh.ActiveSignature, Is.EqualTo(Some 0u))
        Assert.That(sh.ActiveParameter, Is.EqualTo(None))
