open NUnit.Framework
open CSharpLanguageServer.DocumentationUtil

[<TestCase(
    "",
    "", "",
    "")>]
[<TestCase(
    "<summary>doc string</summary>",
    "", "",
    "doc string")>]
[<TestCase(
    "<summary>\r\ndoc string\r\n\r\n</summary>",
    "", "",
    "doc string")>]
let testFormatDocXml (xml, typeName, typeAssemblyName, expectedMD) =
    Assert.AreEqual(
        expectedMD,
        formatDocXml xml typeName typeAssemblyName)
