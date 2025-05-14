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
[<TestCase("""
<member name="M:Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)">
            <summary>
            <para>Adds a child <paramref name="node" />. Nodes can have any number of children, but every child must have a unique name. Child nodes are automatically deleted when the parent node is deleted, so an entire scene can be removed by deleting its topmost node.</para>
            <para>If <paramref name="forceReadableName" /> is <see langword="true" />, improves the readability of the added <paramref name="node" />. If not named, the <paramref name="node" /> is renamed to its type, and if it shares <see cref="P:Godot.Node.Name" /> with a sibling, a number is suffixed more appropriately. This operation is very slow. As such, it is recommended leaving this to <see langword="false" />, which assigns a dummy name featuring <c>@</c> in both situations.</para>
            <para>If <paramref name="internal" /> is different than <see cref="F:Godot.Node.InternalMode.Disabled" />, the child will be added as internal node. These nodes are ignored by methods like <see cref="M:Godot.Node.GetChildren(System.Boolean)" />, unless their parameter <c>include_internal</c> is <see langword="true" />. The intended usage is to hide the internal nodes from the user, so the user won't accidentally delete or modify them. Used by some GUI nodes, e.g. <see cref="T:Godot.ColorPicker" />. See <see cref="T:Godot.Node.InternalMode" /> for available modes.</para>
            <para><b>Note:</b> If <paramref name="node" /> already has a parent, this method will fail. Use <see cref="M:Godot.Node.RemoveChild(Godot.Node)" /> first to remove <paramref name="node" /> from its current parent. For example:</para>
            <para><code>
            Node childNode = GetChild(0);
            if (childNode.GetParent() != null)
            {
                childNode.GetParent().RemoveChild(childNode);
            }
            AddChild(childNode);
            </code></para>
            <para>If you need the child node to be added below a specific node in the list of children, use <see cref="M:Godot.Node.AddSibling(Godot.Node,System.Boolean)" /> instead of this method.</para>
            <para><b>Note:</b> If you want a child to be persisted to a <see cref="T:Godot.PackedScene" />, you must set <see cref="P:Godot.Node.Owner" /> in addition to calling <see cref="M:Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)" />. This is typically relevant for <a href="$DOCS_URL/tutorials/plugins/running_code_in_the_editor.html">tool scripts</a> and <a href="$DOCS_URL/tutorials/plugins/editor/index.html">editor plugins</a>. If <see cref="M:Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)" /> is called without setting <see cref="P:Godot.Node.Owner" />, the newly added <see cref="T:Godot.Node" /> will not be visible in the scene tree, though it will be visible in the 2D/3D view.</para>
            </summary>
        </member>
    """,
    """Adds a child . Nodes can have any number of children, but every child must have a unique name. Child nodes are automatically deleted when the parent node is deleted, so an entire scene can be removed by deleting its topmost node.If  is , improves the readability of the added . If not named, the  is renamed to its type, and if it shares  with a sibling, a number is suffixed more appropriately. This operation is very slow. As such, it is recommended leaving this to , which assigns a dummy name featuring @ in both situations.If  is different than , the child will be added as internal node. These nodes are ignored by methods like , unless their parameter include_internal is . The intended usage is to hide the internal nodes from the user, so the user won't accidentally delete or modify them. Used by some GUI nodes, e.g. . See  for available modes.Note: If  already has a parent, this method will fail. Use  first to remove  from its current parent. For example:
            Node childNode = GetChild(0);
            if (childNode.GetParent() != null)
            {
                childNode.GetParent().RemoveChild(childNode);
            }
            AddChild(childNode);
            If you need the child node to be added below a specific node in the list of children, use  instead of this method.Note: If you want a child to be persisted to a , you must set  in addition to calling . This is typically relevant for tool scripts and editor plugins. If  is called without setting , the newly added  will not be visible in the scene tree, though it will be visible in the 2D/3D view."""
)>]
let testFormatDocXml (inputXml, expectedMD: string) =
    let resultMd = String.Join("\n", formatDocXml inputXml)
    Assert.AreEqual(expectedMD.Replace("\r\n", "\n"), resultMd)
