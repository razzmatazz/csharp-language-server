namespace CSharpLanguageServer.Workspace

open System.IO

module Util =

    type Interesting =
        | Solution of string
        | Projects of string []

    // TODO: Ignore some dirs? Like .git, node_modules.
    let findInteresting (rootDir: string) : Interesting option =
        try
            match Directory.EnumerateFiles(rootDir, "*.sln", SearchOption.AllDirectories) |> Seq.tryHead with
            | Some slnPath ->
                Some (Solution slnPath)
            | None ->
                let projs = Directory.EnumerateFiles(rootDir, "*.csproj", SearchOption.AllDirectories) |> Seq.toArray
                if Array.isEmpty projs then
                    None
                else
                    Some (Projects projs)
        with
        | _ ->
            None
