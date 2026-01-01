# Description

csharp-ls brings advanced language features like code completion, diagnostics,
and refactoring to your editor for C# projects. It supports projects targeting
older .NET SDK versions including .NET Core 3, .NET Framework 4.8,
and potentially earlier ones.

See [FEATURES.md](FEATURES.md) for a more detailed discussion regarding features
and customization provided with csharp-ls.

See [CHANGELOG.md](CHANGELOG.md) for the list of recent improvements/fixes.

See [TROUBLESHOOTING.md](TROUBLESHOOTING.md) for server troubleshooting and
diagnostics.

csharp-ls is MIT-licensed (see [LICENSE](LICENSE)) and is provided with no
warranty of any kind.

# Quick Start

csharp-ls requires the .NET 10 SDK or later to be installed on your machine.
Please head to [Download .NET](https://dotnet.microsoft.com/en-us/download) to
download it.

The server can be installed as [csharp-ls dotnet tool](https://www.nuget.org/packages/csharp-ls/):

```
dotnet tool install --global csharp-ls
```
Once installed, your editor's LSP client should automatically detect and start
`csharp-ls` when opening C# project files.

# Settings

## Configuration
- `csharp.solution` - solution to load, optional

- `csharp.applyFormattingOptions` - use formatting options as supplied by the
  client (may override `.editorconfig` values), defaults to `false`

## Command Line Arguments

```
USAGE: csharp-ls [--help]
                 [--version]
                 [--loglevel <level>]
                 [--solution <solution>]
                 [--debug]
                 [--diagnose]
                 [--features <features>]

OPTIONS:

    --version, -v         display versioning information
    --loglevel, -l <level>
                          set log level, <trace|debug|info|warning|error>; default is `info`
    --solution, -s <solution>
                          specify .sln file to load (relative to CWD)
    --debug               enable debug mode
    --diagnose            run diagnostics
    --features, -f <features>
                          enable optional features, comma-separated: metadata-uris
    --help                display this list of options.
```

See `csharp-ls --help`.

## Experimental Client Capabilities

### Decompiled Code/Metadata URIs
`csharp://` metadata URIs can be enabled by setting the
`experimental.csharp.metadataUris` client capability to `true`.

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
