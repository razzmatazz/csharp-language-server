module CSharpLanguageServer.Tests.DocumentationTests

open System

open Xunit

open CSharpLanguageServer.DocumentationUtil

[<Theory>]
[<InlineData("", "")>]
[<InlineData("<summary>doc string</summary>", "doc string")>]
[<InlineData("<summary>\ndoc string\n\n</summary>", "doc string")>]
[<InlineData("<summary>doc string</summary>\n <param name=\"x\">y</param>",
           """doc string

Parameters:
- ``x``: y""")>]
[<InlineData("\n\
      <summary>Gets the standard error output stream.</summary>\n\
      <returns>A <see cref=\"T:System.IO.TextWriter\" /> that represents the standard error output stream.</returns>\n\
",
           """Gets the standard error output stream.

Returns: A ``System.IO.TextWriter`` that represents the standard error output stream.""")>]
[<InlineData("\n\
            <summary>\n\
            Asserts that a condition is true. If the condition is false the method throws\n\
            an <see cref=\"T:NUnit.Framework.AssertionException\" />.\n\
            </summary>\n\
            <param name=\"condition\">The evaluated condition</param>\n\
 \n\
    ",
           """Asserts that a condition is true. If the condition is false the method throws an ``NUnit.Framework.AssertionException``.

Parameters:
- ``condition``: The evaluated condition""")>]
[<InlineData("\n\
      <summary>Writes a string to the text stream, followed by a line terminator.</summary>\n\
      <param name=\"value\">The string to write. If <paramref name=\"value\" /> is <see langword=\"null\" />, only the line terminator is written.</param>\n\
      <exception cref=\"T:System.ObjectDisposedException\">The <see cref=\"T:System.IO.TextWriter\" /> is closed.</exception>\n\
      <exception cref=\"T:System.IO.IOException\">An I/O error occurs.</exception>\n\
    ",
           """Writes a string to the text stream, followed by a line terminator.

Parameters:
- ``value``: The string to write. If ``value`` is ``null``, only the line terminator is written.

Exceptions:
- ``System.ObjectDisposedException``: The ``System.IO.TextWriter`` is closed.
- ``System.IO.IOException``: An I/O error occurs.""")>]
[<InlineData("""
<member name="M:csharp_test.Test.TestSomething2">
    <summary>
      Test method.
      Does another thing.
    </summary>
</member>
""",
           "Test method. Does another thing.")>]
[<InlineData("<summary>test <c>xx</c></summary>", "test ``xx``")>]
[<InlineData("<summary>test <unknown-inline-tag>contents-of-unknown-tag</unknown-inline-tag></summary>",
           "test contents-of-unknown-tag")>]
[<InlineData("<summary>test <unknown-inline-tag>contents-of-unknown-inline-tag</unknown-inline-tag></summary>",
           "test contents-of-unknown-inline-tag")>]
[<InlineData("<summary>summary</summary>\n
    <unknown-top-level-tag>contents-of-unknown-top-level-tag</unknown-top-level-tag>",
           "summary\n\
<unknown-top-level-tag>contents-of-unknown-top-level-tag</unknown-top-level-tag>")>]
[<InlineData("<summary>summary</summary><remarks>remarks</remarks>", "summary\n\nRemarks: remarks")>]
[<InlineData("<summary>A</summary><returns></returns>", "A")>]
[<InlineData("<param name=\"x\">y</param><param name=\"a\">b</param>",
           """
Parameters:
- ``x``: y
- ``a``: b""")>]
[<InlineData("<summary>desc</summary><typeparam name=\"x\">y</typeparam>",
           """desc

Types:
- ``x``: y""")>]
[<InlineData("""
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
            AddChild(childNode);</code></para>
            <para>If you need the child node to be added below a specific node in the list of children, use <see cref="M:Godot.Node.AddSibling(Godot.Node,System.Boolean)" /> instead of this method.</para>
            <para><b>Note:</b> If you want a child to be persisted to a <see cref="T:Godot.PackedScene" />, you must set <see cref="P:Godot.Node.Owner" /> in addition to calling <see cref="M:Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)" />. This is typically relevant for <a href="$DOCS_URL/tutorials/plugins/running_code_in_the_editor.html">tool scripts</a> and <a href="$DOCS_URL/tutorials/plugins/editor/index.html">editor plugins</a>. If <see cref="M:Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)" /> is called without setting <see cref="P:Godot.Node.Owner" />, the newly added <see cref="T:Godot.Node" /> will not be visible in the scene tree, though it will be visible in the 2D/3D view.</para>
            </summary>
        </member>
    """,
           """Adds a child ``node``. Nodes can have any number of children, but every child must have a unique name. Child nodes are automatically deleted when the parent node is deleted, so an entire scene can be removed by deleting its topmost node.

If ``forceReadableName`` is ``true``, improves the readability of the added ``node``. If not named, the ``node`` is renamed to its type, and if it shares ``Godot.Node.Name`` with a sibling, a number is suffixed more appropriately. This operation is very slow. As such, it is recommended leaving this to ``false``, which assigns a dummy name featuring ``@`` in both situations.

If ``internal`` is different than ``Godot.Node.InternalMode.Disabled``, the child will be added as internal node. These nodes are ignored by methods like ``Godot.Node.GetChildren(System.Boolean)``, unless their parameter ``include_internal`` is ``true``. The intended usage is to hide the internal nodes from the user, so the user won't accidentally delete or modify them. Used by some GUI nodes, e.g. ``Godot.ColorPicker``. See ``Godot.Node.InternalMode`` for available modes.

Note: If ``node`` already has a parent, this method will fail. Use ``Godot.Node.RemoveChild(Godot.Node)`` first to remove ``node`` from its current parent. For example:


            Node childNode = GetChild(0);
            if (childNode.GetParent() != null)
            {
                childNode.GetParent().RemoveChild(childNode);
            }
            AddChild(childNode);

If you need the child node to be added below a specific node in the list of children, use ``Godot.Node.AddSibling(Godot.Node,System.Boolean)`` instead of this method.

Note: If you want a child to be persisted to a ``Godot.PackedScene``, you must set ``Godot.Node.Owner`` in addition to calling ``Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)``. This is typically relevant for tool scripts and editor plugins. If ``Godot.Node.AddChild(Godot.Node,System.Boolean,Godot.Node.InternalMode)`` is called without setting ``Godot.Node.Owner``, the newly added ``Godot.Node`` will not be visible in the scene tree, though it will be visible in the 2D/3D view.""")>]
[<InlineData("""
<summary>
Upserts an item as an asynchronous operation in the Azure Cosmos service.
</summary>
<param name="item">A JSON serializable object that must contain an id property. <see cref="T:Microsoft.Azure.Cosmos.CosmosSerializer" /> to implement a custom serializer</param>
<param name="partitionKey"><see cref="T:Microsoft.Azure.Cosmos.PartitionKey" /> for the item. If not specified will be populated by extracting from {T}</param>
<param name="requestOptions">(Optional) The options for the item request.</param>
<param name="cancellationToken">(Optional) <see cref="T:System.Threading.CancellationToken" /> representing request cancellation.</param>
<returns>The <see cref="T:Microsoft.Azure.Cosmos.ItemResponse`1" /> that was upserted contained within a <see cref="T:System.Threading.Tasks.Task" /> object representing the service response for the asynchronous operation.</returns>
<exception>https://aka.ms/cosmosdb-dot-net-exceptions#typed-api</exception>
""",
           """Upserts an item as an asynchronous operation in the Azure Cosmos service.

Parameters:
- ``item``: A JSON serializable object that must contain an id property. ``Microsoft.Azure.Cosmos.CosmosSerializer`` to implement a custom serializer
- ``partitionKey``: ``Microsoft.Azure.Cosmos.PartitionKey`` for the item. If not specified will be populated by extracting from {T}
- ``requestOptions``: (Optional) The options for the item request.
- ``cancellationToken``: (Optional) ``System.Threading.CancellationToken`` representing request cancellation.

Returns: The ``Microsoft.Azure.Cosmos.ItemResponse`1`` that was upserted contained within a ``System.Threading.Tasks.Task`` object representing the service response for the asynchronous operation.

Exceptions:
- ``(unspecified)``: https://aka.ms/cosmosdb-dot-net-exceptions#typed-api""")>]
let testFormatDocXml (inputXml, expectedMD: string) =
    let resultMd = String.Join("\n", formatDocXml inputXml)
    Assert.Equal(expectedMD.Replace("\r\n", "\n"), resultMd)
