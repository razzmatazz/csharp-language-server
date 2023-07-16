namespace CSharpLanguageServer.Handlers

open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module DocumentHighlight =
    let provider: bool option = Some true

    let private shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    let handle (wm: IWorkspaceManager) (p: TextDocumentPositionParams) : AsyncLspResult<DocumentHighlight[] option> = async {
        let filePath = Uri.toPath p.TextDocument.Uri

        // We only need to find references in the file (not the whole workspace), so we don't use
        // wm.FindSymbol & wm.FindReferences here.
        let getHighlights (symbol: ISymbol) (doc: Document) = async {
            let docSet = ImmutableHashSet.Create(doc)
            let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, docSet) |> Async.AwaitTask
            let! def = SymbolFinder.FindSourceDefinitionAsync(symbol, doc.Project.Solution) |> Async.AwaitTask

            let locations =
                refs
                |> Seq.collect (fun r -> r.Locations)
                |> Seq.map (fun rl -> rl.Location)
                |> Seq.filter (fun l -> l.IsInSource && l.SourceTree.FilePath = filePath)
                |> Seq.append (def |> Option.ofObj |> Option.toList |> Seq.collect (fun sym -> sym.Locations))

            return
                locations
                |> Seq.map (fun l ->
                    { Range = (Location.fromRoslynLocation l).Range
                      Kind = Some DocumentHighlightKind.Read })
        }

        match wm.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let position = Position.toRoslynPosition sourceText.Lines p.Position
            let! symbol = SymbolFinder.FindSymbolAtPositionAsync(doc, position) |> Async.AwaitTask

            match Option.ofObj symbol with
            | Some symbol when shouldHighlight symbol ->
                let! highlights = getHighlights symbol doc
                return highlights |> Seq.toArray |> Some |> success
            | _ -> return None |> success
    }
