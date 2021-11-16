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
 - `textDocument/implementation` missing;
 - intellisense/autocomplete does not always trigger where it should be
 - on-hover has issues:
    - does not show arg names when hovering on method/.ctor
    - type displayed is not the most appropriate in some contexts
    - some tags are still not supported, like `<typeparam>`
 - `textDocument/signatureHelp` support for overload listing/selection
 - selection range provider
 - semantic tokens
 - code lenses
 - formatting, on type and otherwise
 - ability to run tests
 - razorls integration (server-side)
 - analyzer support
 - code generator support
 - remove dependency on Newtonsoft.Json
