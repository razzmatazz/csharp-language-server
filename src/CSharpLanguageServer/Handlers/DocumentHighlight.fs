namespace CSharpLanguageServer.Handlers

open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module DocumentHighlight =
    let provider (_cc: ClientCapabilities) : U2<bool, DocumentHighlightOptions> option = Some(U2.C1 true)

    let private shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    // We only need to find references in the file (not the whole workspace), so we don't use
    // context.FindSymbol & context.FindReferences here.
    let private getHighlights symbol (project: Project) (docMaybe: Document option) (filePath: string) = async {
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
            |> Seq.choose Location.fromRoslynLocation
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
            | Some _wf, Some(symbol, project, docMaybe) ->
                if shouldHighlight symbol then
                    let! highlights = getHighlights symbol project docMaybe (Uri.toPath p.TextDocument.Uri)
                    return highlights |> Seq.toArray |> Some |> LspResult.success
                else
                    return None |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
