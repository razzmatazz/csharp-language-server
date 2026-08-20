module CSharpLanguageServer.Tests.SignatureHelpTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling
open CSharpLanguageServer.Tests.FixturePool

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
        ClassicAssert.AreEqual(1, sh.Signatures.Length)

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

        ClassicAssert.AreEqual(expectedSignature0, sh.Signatures[0])

        ClassicAssert.AreEqual(Some 0u, sh.ActiveSignature)
        ClassicAssert.AreEqual(None, sh.ActiveParameter)
