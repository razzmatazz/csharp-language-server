# Description
This is a hacky Roslyn-based LSP server as an alternative to 
[omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn).

# Features
- Most of basic LSP features: rename/go-to-def/find references, etc;
- Go-to-metadata (needs integration from your LSP client.)

# Installation
`dotnet tool install --global csharp-ls`

See [csharp-ls nuget page](https://www.nuget.org/packages/csharp-ls/)

# Acknowledgements
- LSP interface code here is based on (copied from)  [FSharpAutoComplete](https://github.com/fsharp/FsAutoComplete) code;
- csharp-ls uses Roslyn to parse and update code; Roslyn maps really nicely to LSP w/relatively little impedance mismatch;
- csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source;
- csharp-ls is not affiliated with Microsoft Corp.

# Change log
See [CHANGELOG.md](CHANGELOG.md)

# TODO list
 - intellisense/autocomplete does not always trigger where it should be
 - on-hover has issues:
    - symbol displayed/assembly is not the most appropriate in some contexts; 
    - some tags are still not supported, like `<typeparam>`
 - selection range provider
 - semantic tokens
 - code lenses
 - formatting, on type and otherwise
 - ability to run tests
 - razorls integration (server-side)
 - analyzer support
 - code generator support
