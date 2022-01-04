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
 - don't publish diagnostics on every keypress, but every 1-2-3 seconds instead;
 - we probably want to reload file from disk in case the editor closes the file;
   - this will help us to fix sync problems as well, as now we're stuck if the file is out of sync and need to restart LSP;
 - don't ignore notifications but queue and run them after initialization;
   - we're using file modifications otherwise made during initialization as we're working with incremental file changes now;
 - asyncwrlock still does not provide us proper sequencing, operation scheduling needs proper impl:
   - to avoid problems where r-o operation is sequenced BEFORE r-w operation but r-w is execute first because thats how asyncrwlock works..;
 - progress support;
 - internally-forced timeout for codelens requests (sometimes those seem to take excessive time)?;
   - or we could limit concurrency level;
 - intellisense/autocomplete does not always trigger where it should be
 - `textDocument/signatureHelp` support for overload listing/selection
 - selection range provider
 - semantic tokens
 - formatting, on type and otherwise
 - ability to run tests
 - razorls integration (server-side)
 - analyzer support
 - code generator support
