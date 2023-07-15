namespace CSharpLanguageServer.Common

open System
open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis.Text


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


module Position =
    let fromLinePosition (pos: LinePosition): Position =
        { Line = pos.Line ; Character = pos.Character }

    let toLinePosition (lines: TextLineCollection) (pos: Position): LinePosition =
        if pos.Line < 0 then
            LinePosition(0, 0)
        else if pos.Line >= lines.Count then
            LinePosition(lines.Count - 1, lines[lines.Count - 1].EndIncludingLineBreak - lines[lines.Count - 1].Start)
        else
            LinePosition(pos.Line, pos.Character)


module Range =
    let toLinePositionSpan (lines: TextLineCollection) (range: Range): LinePositionSpan =
        LinePositionSpan(
            Position.toLinePosition lines range.Start,
            Position.toLinePosition lines range.End)
