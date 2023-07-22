namespace CSharpLanguageServer.Workspace

open System.IO
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Handlers

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


    let getRegistrations (clientCapabilities: ClientCapabilities option): Registration list =
        let registrationBuilders =
            [ CallHierarchy.registration
              CodeAction.registration
              CodeLens.registration
              Color.registration
              Completion.registration
              Declaration.registration
              Definition.registration
              Diagnostic.registration
              DocumentFormatting.registration
              DocumentHighlight.registration
              DocumentLink.registration
              DocumentOnTypeFormatting.registration
              DocumentRangeFormatting.registration
              DocumentSymbol.registration
              ExecuteCommand.registration
              FoldingRange.registration
              Hover.registration
              Implementation.registration
              InlayHint.registration
              InlineValue.registration
              LinkEditingRange.registration
              Moniker.registration
              References.registration
              Rename.registration
              SelectionRange.registration
              SemanticTokens.registration
              SignatureHelp.registration
              TextDocumentSync.registration
              TypeDefinition.registration
              TypeHierarchy.registration
              WorkspaceSymbol.registration ]
        registrationBuilders
        |> List.map ((|>) clientCapabilities)
        |> List.filter (Option.isSome)
        |> List.map (Option.get)


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
