module CSharpLanguageServer.Lsp.Workspace

open System
open System.IO

open Microsoft.CodeAnalysis

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types


type LspWorkspaceFolder =
    { Uri: string
      Name: string option
      RoslynWorkspace: Workspace option
      Solution: Solution option }


type LspWorkspace =
    { Folders: LspWorkspaceFolder list }

    static member Empty = { Folders = [] }
