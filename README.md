# Description

csharp-ls brings advanced language features like code completion, diagnostics,
and refactoring to your editor for C# projects. It supports projects targeting
older .NET SDK versions including .NET Core 3, .NET Framework 4.8,
and potentially earlier ones.

See [FEATURES.md](FEATURES.md) for a more detailed discussion regarding features
and customization provided with csharp-ls.

See [CHANGELOG.md](CHANGELOG.md) for the list of recent improvements/fixes.

csharp-ls is MIT-licensed (see [LICENSE](LICENSE)) and is provided with no
warranty of any kind.

# Quick Start

csharp-ls requires the .NET 9 SDK or later to be installed on your machine.
Please head to [Download .NET](https://dotnet.microsoft.com/en-us/download) to
download it.

The server can be installed as [csharp-ls dotnet tool](https://www.nuget.org/packages/csharp-ls/):

```
dotnet tool install --global csharp-ls
```
Once installed, your editor's LSP client should automatically detect and start
`csharp-ls` when opening C# project files.

# Clients

`csharp-ls` implements the standard LSP protocol to interact with your editor.
However, there are some features that require a non-standard implementation.
This is where editor-specific plugins can be helpful.

Notable clients:
* Neovim: [csharpls-extended-lsp.nvim](https://github.com/Decodetalkers/csharpls-extended-lsp.nvim)
* Emacs: [emacs/lsp-mode](https://github.com/emacs-lsp/lsp-mode)
* Visual Studio Code: [vscode-csharp-ls @ github](https://github.com/vytautassurvila/vscode-csharp-ls)

# Acknowledgements

* csharp-ls is not affiliated with Microsoft Corp.,
* csharp-ls uses [Roslyn](https://github.com/dotnet/roslyn) to parse and update code,
* csharp-ls uses the LSP interface from the [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol) project,
* csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source.

# Alternatives

- [OmniSharp](https://github.com/OmniSharp/omnisharp-roslyn)
- [C# Dev Kit for VSCode](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csdevkit)
