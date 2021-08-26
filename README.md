# Description

This is a hacky Roslyn-based LSP server written in F#. After some experimentation
I felt I could try to hack my own custom LSP server for C#.

The code here is based on [FSharpAutoComplete](https://github.com/fsharp/FsAutoComplete) code.

# TODO lists

## Next version
 - implement go to metadata
   - link generated document to project so we can do recursive go-to-def in metadata
   - fix an issue with SourceText/Source names when returning from OmnisharpMetadata()
   - more things..
 - work around sharp corners, test as daily tool
 - publish as `dotnet tool csharp-ls`

## Wish list
 - implement TextDocumentImplementation
 - tests
 - code lenses
 - formatting, on type and otherwise
 - parse documentation into format needed for lsp
 - go-to definition on symbol in metada
 - ability to run tests
