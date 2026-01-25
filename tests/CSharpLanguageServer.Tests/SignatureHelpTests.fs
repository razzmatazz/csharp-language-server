module CSharpLanguageServer.Tests.SignatureHelpTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

[<Test>]
let ``test textDocument/signatureHelp works`` () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let signatureHelpParams0: SignatureHelpParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 10u; Character = 16u }
          WorkDoneToken = None
          Context = None }

    let signatureHelp0: SignatureHelp option =
        client.Request("textDocument/signatureHelp", signatureHelpParams0)

    match signatureHelp0 with
    | None -> failwith "Some SignatureHelp was expected"
    | Some sh ->
        Assert.AreEqual(1, sh.Signatures.Length)

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

        Assert.AreEqual(expectedSignature0, sh.Signatures[0])

        Assert.AreEqual(Some 0u, sh.ActiveSignature)
        Assert.AreEqual(None, sh.ActiveParameter)
