# Description
This is a hacky Roslyn-based LSP server as an alternative to 
[omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn).

`csharp-ls` requires .NET 6 SDK to be installed. However it has been reported 
to work with projects using older versions of dotnet SDK, including .NET Core 3, 
.NET Framework 4.8 and possibly older ones too as it uses the standard
Roslyn/MSBuild libs that Visual Studio & omnisharp does.

## Features
- symbol rename;
- go-to-definition;
- find references;
- document/workspace symbol search;
- `textDocument/documentHighlight` support:
  - highlighting other symbol references in the document on hover;
- `codeAction/resolve` support for better performance when invoking code actions;
- go-to-definition in metadata support:
  - (needs integration from your LSP client -- `emacs-lsp/lsp-mode` has it.).

See [CHANGELOG.md](CHANGELOG.md) for the list of recent improvements/fixes.

# Installation
`dotnet tool install --global csharp-ls`

See [csharp-ls nuget page](https://www.nuget.org/packages/csharp-ls/)

# Acknowledgements
- csharp-ls uses LSP interface from [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol);
- csharp-ls uses Roslyn to parse and update code; Roslyn maps really nicely to LSP w/relatively little impedance mismatch;
- csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source;
- csharp-ls is not affiliated with Microsoft Corp.

# TODO list
 - adding new file does not register it into new solution anymore, for some reason;
 - support for inlay hints (lsp 3.17);
 - go-to-def in metadata does not work for Attribute as those have Attribute suffix;
 - progress support;
 - properly escape docxml text, e.g. backquote is a special character in markdown;
 - selection range provider
 - semantic tokens
 - ability to run tests / test browser support like fsac has?
 - razorls integration (server-side)
 - analyzer support
 - code generator support
 - vscode plugin
