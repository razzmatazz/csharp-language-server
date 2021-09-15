# Description
This is a hacky Roslyn-based LSP server as an alternative to 
[omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn).

# Features
- Most of basic LSP features: rename/go-to-def/find references, etc;
- Go-to-metadata (needs integration from your LSP client.)

# Acknowledgements
- LSP interface code here is based on (copied from)  [FSharpAutoComplete](https://github.com/fsharp/FsAutoComplete) code;
- csharp-ls uses Roslyn to parse and update code; Roslyn maps really nicely to LSP w/relatively little impedance mismatch;
- csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source.

# Changelog
See [CHANGELOG.md](CHANGELOG.md)

# TODO list
 - ability to run tests
 - implement TextDocumentImplementation
 - code lenses
 - formatting, on type and otherwise
 - parse documentation into format needed for lsp
 - razorls integration (server-side)
