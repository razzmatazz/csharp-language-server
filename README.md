# Description

This is a hacky Roslyn-based LSP server written in F#. After some experimentation
I felt I could try to hack my own custom LSP server for C#.

The code here is based on
[F# Language Server](https://github.com/fsprojects/fsharp-language-server) code.

# TODO list
 - navigation to symbol on file
 - code lenses
 - formatting
 - ability to run tests
 - navigate all symbols on solution
 - go-to definition on symbol in metada
 - code actions:
   - fix usings
   - more?
 - parse documentation into format needed for lsp
