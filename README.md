# Description

This is a hacky Roslyn-based LSP server as an alternative to 
[omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn).

The server code here is based on (copied from) 
[FSharpAutoComplete](https://github.com/fsharp/FsAutoComplete) code.

# Features
 - Most of basic LSP features: rename/go-to-def/find references, etc;
 - Go-to-metadata (needs integration from your LSP client.)

# TODO list

## For the next version (v0.1-alpha)
 - publish as `dotnet tool csharp-ls`
 
## On queue
 - ability to run tests
 - implement TextDocumentImplementation
 - tests
 - code lenses
 - formatting, on type and otherwise
 - parse documentation into format needed for lsp
 - razorls integration (server-side)
