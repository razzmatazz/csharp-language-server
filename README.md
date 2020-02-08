# Description

This is a hacky Roslyn-based LSP server written in F#. I got somewhat frustrated
trying to fix issues in omnisharp-roslyn LSP mode code and felt I could try to
hack my own custom LSP server for C#.

The code here is based on
[F# Language Server](https://github.com/fsprojects/fsharp-language-server) code.

# TODO list
 - completion seems to be doing the wrong thing
 - formatting
 - ability to run tests
 - navigate all symbols on solution
 - go-to definition on symbol in metada
 - code actions:
   - fix usings
   - more?
 - send incremental changes to the server when file is changed
   - this should fix performance quite a bit on large files

