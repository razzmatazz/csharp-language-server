namespace CSharpLanguageServer.Common

open System
open Ionide.LanguageServerProtocol.Types


module Uri =
    // Unescape some necessary char before passing string to Uri.
    // Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
    // ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
    /// "#/ProjDir" is Fragment instead of part of LocalPath.
    let private unescape (uri: string) = uri.Replace("%3a", ":", true, null)

    let toPath (uri: string) = Uri.UnescapeDataString(Uri(unescape(uri)).LocalPath)

    let fromPath (path: string) = Uri(path).ToString()

    let toWorkspaceFolder(uri: string): WorkspaceFolder =
        { Uri = uri
          Name = Uri.UnescapeDataString(Uri(unescape(uri)).Segments |> Array.last) }


module Path =
    let toUri = Uri.fromPath

    let fromUri = Uri.toPath

    let toWorkspaceFolder = toUri >> Uri.toWorkspaceFolder
