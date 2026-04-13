# Decompiled and Source-Generated Document URIs

When the `metadataUris` feature is enabled, `textDocument/definition` on a
symbol that lives outside the user's own source files returns a virtual
`csharp:/` URI instead of a temporary file path. There are two kinds:

| Kind | URI format |
|------|-----------|
| Decompiled assembly symbol | `csharp:/<project.csproj>/decompiled/<Symbol>.cs` |
| Source-generated file | `csharp:/<project.csproj>/generated/<HintName>` |

The URI can be passed to the custom `csharp/metadata` LSP request to retrieve
the source text.

## Enabling the feature

Any one of the following is sufficient:

- Set `csharp.useMetadataUris = true` in your editor's workspace configuration
  (`workspace/configuration` / `workspace/didChangeConfiguration`)
- Pass `--features metadata-uris` on the command line
- Set the `experimental.csharp.metadataUris` client capability to `true` during
  LSP initialization

## `csharp/metadata` request

### Request

```json
{
  "textDocument": {
    "uri": "csharp:/<project.csproj>/decompiled/System.String.cs"
  }
}
```

The `uri` must be a `csharp:/` URI obtained from a prior `textDocument/definition`
(or similar) response. Both decompiled and source-generated URIs are accepted.

### Response

```json
{
  "projectName": "MyProject",
  "assemblyName": "System.Runtime",
  "symbolName": "System.String",
  "source": "using System.Buffers;\n..."
}
```

| Field | Description |
|-------|-------------|
| `projectName` | Name of the Roslyn project that owns the document |
| `assemblyName` | Assembly name (decompiled: containing assembly; generated: project assembly) |
| `symbolName` | Fully-qualified type name (decompiled) or hint name (source-generated) |
| `source` | Full source text of the decompiled or generated document |

## Editor integration

### Neovim

```lua
local params = { textDocument = { uri = uri } }
local result, err = client.request_sync("csharp/metadata", params, 10000)
if not err then
  local source = result.result.source
end
```

A ready-made plugin is available:
[csharpls-extended-lsp.nvim](https://github.com/Decodetalkers/csharpls-extended-lsp.nvim)

### Visual Studio Code

[vscode-csharp-ls](https://github.com/vytautassurvila/vscode-csharp-ls)
