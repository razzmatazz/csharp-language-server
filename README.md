[![Buy Me a Coffee](https://img.shields.io/badge/Buy%20Me%20a%20Coffee-orange?logo=buy-me-a-coffee)](https://www.buymeacoffee.com/razzmatazz)

# Description
This is a hacky Roslyn-based LSP server for C#, as an alternative to 
[omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn).

`csharp-ls` requires .NET 9 SDK to be installed. However it has been reported
to work with projects using older versions of dotnet SDK, including .NET Core 3, 
.NET Framework 4.8 and possibly older ones too as it uses the standard
Roslyn/MSBuild libs that Visual Studio & omnisharp does.

See [CHANGELOG.md](CHANGELOG.md) for the list of recent improvements/fixes.
See [FEATURES.md](FEATURES.md) for a more detailed discussion regarding features
and customization available in csharp-ls.

# Acknowledgements
- csharp-ls is not affiliated with Microsoft Corp;
- csharp-ls uses LSP interface from [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol);
- csharp-ls uses [Roslyn](https://github.com/dotnet/roslyn) to parse and update code; Roslyn maps really nicely to LSP w/relatively little impedance mismatch;
- csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source.

# Installation
`dotnet tool install --global csharp-ls`

See [csharp-ls nuget page](https://www.nuget.org/packages/csharp-ls/)

# Settings

- `csharp.solution` - solution to load, optional
- `csharp.applyFormattingOptions` - use formatting options as supplied by the client (may override `.editorconfig` values), defaults to `false`

# Clients

`csharp-ls` implements the standard LSP protocol to interact with your editor.
However there are some features that need a non-standard implementation and this
is where editor-specific plugins can be helpful.

## Emacs
### emacs/lsp-mode
Supports automatic installation, go-to-metatada (can view code from nuget/compiled dlls)
and some additional features.

See [emacs/lsp-mode](https://github.com/emacs-lsp/lsp-mode).

## Visual Studio Code
### vytautassurvila/vscode-csharp-ls
- Supports code decompilation from metadata

See [csharp-ls](https://marketplace.visualstudio.com/items?itemName=vytautassurvila.csharp-ls) and [vscode-csharp-ls @ github](https://github.com/vytautassurvila/vscode-csharp-ls).

### statiolake/vscode-csharp-ls
See [vscode-csharp-ls](https://marketplace.visualstudio.com/items?itemName=statiolake.vscode-csharp-ls).

# Building

## On Linux/macOS

```
$ dotnet build
```

