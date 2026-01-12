namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module DocumentHighlight =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.DocumentHighlight
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, DocumentHighlightOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DocumentHighlightRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/documentHighlight"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    // We only need to find references in the file (not the whole workspace), so we don't use
    // context.FindSymbol & context.FindReferences here.
    let private getHighlights symbol (project: Project) (docMaybe: Document option) wfPathToUri (filePath: string) = async {
        let! ct = Async.CancellationToken

        let docSet: ImmutableHashSet<Document> option =
            docMaybe |> Option.map (fun doc -> ImmutableHashSet.Create(doc))

        let! refs =
            SymbolFinder.FindReferencesAsync(symbol, project.Solution, docSet |> Option.toObj, cancellationToken = ct)
            |> Async.AwaitTask

        let! def =
            SymbolFinder.FindSourceDefinitionAsync(symbol, project.Solution, cancellationToken = ct)
            |> Async.AwaitTask

        let locations =
            refs
            |> Seq.collect (fun r -> r.Locations)
            |> Seq.map (fun rl -> rl.Location)
            |> Seq.filter (fun l -> l.IsInSource && l.GetMappedLineSpan().Path = filePath)
            |> Seq.append (def |> Option.ofObj |> Option.toList |> Seq.collect (fun sym -> sym.Locations))

        return
            locations
            |> Seq.choose (Location.fromRoslynLocation wfPathToUri)
            |> Seq.map (fun l ->
                { Range = l.Range
                  Kind = Some DocumentHighlightKind.Read })
    }

    let handle
        (context: ServerRequestContext)
        (p: DocumentHighlightParams)
        : AsyncLspResult<DocumentHighlight[] option> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, docMaybe) ->
                if shouldHighlight symbol then
                    let wfPathToUri = workspaceFolderPathToUri wf

                    let! highlights =
                        getHighlights
                            symbol
                            project
                            docMaybe
                            wfPathToUri
                            (workspaceFolderUriToPath wf p.TextDocument.Uri |> _.Value)

                    return highlights |> Seq.toArray |> Some |> LspResult.success
                else
                    return None |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
