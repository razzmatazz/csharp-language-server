namespace CSharpLanguageServer.Handlers

open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module DocumentHighlight =
    let provider (_cc: ClientCapabilities) : U2<bool, DocumentHighlightOptions> option = Some(U2.C1 true)

    let private shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    let handle
        (context: ServerRequestContext)
        (p: DocumentHighlightParams)
        : AsyncLspResult<DocumentHighlight[] option> =
        async {
            let! ct = Async.CancellationToken

            // We only need to find references in the file (not the whole workspace), so we don't use
            // context.FindSymbol & context.FindReferences here.
            let getHighlights (filePath: string) (symbol: ISymbol) (doc: Document) = async {
                let docSet = ImmutableHashSet.Create(doc)

                let! refs =
                    SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, docSet, cancellationToken = ct)
                    |> Async.AwaitTask

                let! def =
                    SymbolFinder.FindSourceDefinitionAsync(symbol, doc.Project.Solution, cancellationToken = ct)
                    |> Async.AwaitTask

                return
                    refs
                    |> Seq.collect (fun r -> r.Locations)
                    |> Seq.map (fun rl -> rl.Location)
                    |> Seq.filter (fun l -> l.IsInSource && l.GetMappedLineSpan().Path = filePath)
                    |> Seq.append (def |> Option.ofObj |> Option.toList |> Seq.collect (fun sym -> sym.Locations))
                    |> Seq.choose Location.fromRoslynLocation
                    |> Seq.map (fun l ->
                        { Range = l.Range
                          Kind = Some DocumentHighlightKind.Read })
            }

            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, _, Some doc) ->
                let filePath = p.TextDocument.Uri |> workspaceFolderUriToPath wf

                match shouldHighlight symbol, filePath with
                | true, Some filePath ->
                    let! highlights = getHighlights filePath symbol doc
                    return highlights |> Seq.toArray |> Some |> LspResult.success
                | _, _ -> return None |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
