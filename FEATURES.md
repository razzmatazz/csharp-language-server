# Support for decompilation via "csharp/metadata"

## API

The api is "csharp/metadata" and in neovim, you can request it like

```lua
  local result, err = client.request_sync("csharp/metadata", params, 10000)
```

## sender
You need to send a uri, it is like

**csharp:/metadata/projects/trainning2/assemblies/System.Console/symbols/System.Console.cs**

In neovim, it will be result(s) from vim.lsp.handles["textDocument/definition"]

and the key of uri is the key,

The key to send is like

```lua
local params = {
	timeout = 5000,
	textDocument = {
		uri = uri,
	}
}
```

The key of textDocument is needed. And timeout is just for neovim. It is the same if is expressed by json.

## receiver

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
