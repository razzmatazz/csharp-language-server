namespace CSharpLanguageServer.Common

open System
open Ionide.LanguageServerProtocol.Types


module Uri =
    let toPath (uri: string) = Uri.UnescapeDataString(Uri(uri).LocalPath)

    let fromPath (path: string) = Uri(path).ToString()

    let toWorkspaceFolder(uri: string): WorkspaceFolder =
        { Uri = uri
          Name = Uri.UnescapeDataString(Uri(uri).Segments |> Array.last) }


module Path =
    let toUri = Uri.fromPath

    let fromUri = Uri.toPath

    let toWorkspaceFolder = toUri >> Uri.toWorkspaceFolder
