# Description

This is a hacky Roslyn-based LSP server written in F#. After some experimentation
I felt I could try to hack my own custom LSP server for C#.

The code here is based on
[F# Language Server](https://github.com/fsprojects/fsharp-language-server) and
[FSharpAutoComplete](https://github.com/fsharp/FsAutoComplete) code.

# TODO lists
## Next version
 - work around sharp corners, test as daily tool
 - publish as `dotnet tool csharp-ls`

## Wish list
 - implement TextDocumentImplementation
 - tests
 - code lenses
 - formatting
 - parse documentation into format needed for lsp
 - go-to definition on symbol in metada
 - ability to run tests
