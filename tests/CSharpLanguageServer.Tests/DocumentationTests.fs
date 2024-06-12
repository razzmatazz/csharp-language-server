module CSharpLanguageServer.Tests.DocumentationTests

open System

open NUnit.Framework

open CSharpLanguageServer.DocumentationUtil

[<TestCase(
    "",
    "")>]
[<TestCase(
    "<summary>doc string</summary>",
    "doc string")>]
[<TestCase(
    "<summary>\ndoc string\n\n</summary>",
    "doc string")>]
[<TestCase(
    "<summary>doc string</summary>\n <param name=\"x\">y</param>",
    """doc string

Parameters:
- `` x ``: y""")>]
[<TestCase(
    "\n\
      <summary>Gets the standard error output stream.</summary>\n\
      <returns>A <see cref=\"T:System.IO.TextWriter\" /> that represents the standard error output stream.</returns>\n\
",
    """Gets the standard error output stream.

Returns: A `` System.IO.TextWriter `` that represents the standard error output stream."""
)>]
[<TestCase(
    "\n\
            <summary>\n\
            Asserts that a condition is true. If the condition is false the method throws\n\
            an <see cref=\"T:NUnit.Framework.AssertionException\" />.\n\
            </summary>\n\
            <param name=\"condition\">The evaluated condition</param>\n\
 \n\
    ",
    """Asserts that a condition is true. If the condition is false the method throws an `` NUnit.Framework.AssertionException ``.

Parameters:
- `` condition ``: The evaluated condition"""
)>]
[<TestCase(
    "\n\
      <summary>Writes a string to the text stream, followed by a line terminator.</summary>\n\
      <param name=\"value\">The string to write. If <paramref name=\"value\" /> is <see langword=\"null\" />, only the line terminator is written.</param>\n\
      <exception cref=\"T:System.ObjectDisposedException\">The <see cref=\"T:System.IO.TextWriter\" /> is closed.</exception>\n\
      <exception cref=\"T:System.IO.IOException\">An I/O error occurs.</exception>\n\
    ",
    """Writes a string to the text stream, followed by a line terminator.

Parameters:
- `` value ``: The string to write. If `` value `` is `` null ``, only the line terminator is written.

Exceptions:
- `` System.ObjectDisposedException ``: The `` System.IO.TextWriter `` is closed.
- `` System.IO.IOException ``: An I/O error occurs."""
)>]
[<TestCase("""
<member name="M:csharp_test.Test.TestSomething2">
    <summary>
      Test method.
      Does another thing.
    </summary>
</member>
""",
    "Test method. Does another thing.")>]
[<TestCase(
    "<summary>test <c>xx</c></summary>",
    "test `` xx ``")>]
[<TestCase(
    "<summary>test <unknown-inline-tag>contents-of-unknown-tag</unknown-inline-tag></summary>",
    "test contents-of-unknown-tag")>]
[<TestCase(
    "<summary>test <unknown-inline-tag>contents-of-unknown-inline-tag</unknown-inline-tag></summary>",
    "test contents-of-unknown-inline-tag")>]
[<TestCase(
    "<summary>summary</summary>\n
    <unknown-top-level-tag>contents-of-unknown-top-level-tag</unknown-top-level-tag>",
    "summary\n\
<unknown-top-level-tag>contents-of-unknown-top-level-tag</unknown-top-level-tag>")>]
[<TestCase(
    "<summary>summary</summary><remarks>remarks</remarks>",
    "summary\n\nRemarks: remarks")>]
[<TestCase(
    "<summary>A</summary><returns></returns>",
    "A")>]
[<TestCase(
    "<param name=\"x\">y</param><param name=\"a\">b</param>",
    """
Parameters:
- `` x ``: y
- `` a ``: b""")>]
[<TestCase(
    "<summary>desc</summary><typeparam name=\"x\">y</typeparam>",
    """desc

Types:
- `` x ``: y""")>]
let testFormatDocXml (xml, expectedMD: string) =
    Assert.AreEqual(expectedMD.Replace("\r\n", "\n"), String.Join("\n", formatDocXml xml))
