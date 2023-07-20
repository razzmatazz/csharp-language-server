namespace CSharpLanguageServer.Workspace

open System.IO

module Util =

    type Interesting =
        | Solution of string
        | Projects of string []

    // TODO: Ignore some dirs? Like .git, node_modules.
    // TODO: Is it possible to make it lazy? It will start to find csproj files only if it can't find sln file.
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


// Source from https://nbevans.wordpress.com/2014/08/09/a-simple-stereotypical-javascript-like-debounce-service-for-f/
type Debounce(timeout, fn) as x =
    let mailbox = MailboxProcessor.Start(fun agent ->
        let rec loop ida idb = async {
            match! agent.TryReceive(x.Timeout) with
            | Some _ -> return! loop ida (idb + 1)
            | None when ida <> idb ->
                fn ()
                return! loop 0 0
            | None -> return! loop 0 0
        }

        loop 0 0)

    /// Calls the function, after debouncing has been applied.
    member __.Bounce() = mailbox.Post(null)

    /// Timeout in ms
    member val Timeout = timeout with get, set
