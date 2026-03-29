# Support for decompilation

## API

The api is "csharp/metadata" and in neovim, you can request it like

```lua
  local result, err = client.request_sync("csharp/metadata", params, 10000)
```

## Request Parameters
You need to send a uri, it is like

**csharp:/metadata/projects/trainning2/assemblies/System.Console/symbols/System.Console.cs**

The `uri` parameter should be the URI of the symbol obtained from an LSP `textDocument/definition` (or similar) request.

The key to send is like

```lua
local params = {
	timeout = 5000,
	textDocument = {
		uri = uri,
	}
}
```

The key of textDocument is needed.

## Response Structure

The object received is like

```lua
{
	projectName = "csharp-test",
	assemblyName = "System.Runtime",
	symbolName = "System.String",
	source = "using System.Buffers;\n ...."
}
```

And In neovim, You receive the "result" above, you can get the decompile source from

```lua

local result, err = client.request_sync("csharp/metadata", params, 10000)
local source
if not err then
	source = result.result.source
end
```

And there is a plugin of neovim for you to decompile it.

[csharpls-extended-lsp.nvim](https://github.com/chen244/csharpls-extended-lsp.nvim)
