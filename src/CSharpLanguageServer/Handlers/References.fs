namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module References =
    let provider (_cc: ClientCapabilities) : U2<bool, ReferenceOptions> option = Some(U2.C1 true)

    let handle (context: ServerRequestContext) (p: ReferenceParams) : AsyncLspResult<Location[] option> = async {
        let! ct = Async.CancellationToken

        match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
        | Some wf, Some(symbol, _, _) ->
            let! refs =
                SymbolFinder.FindReferencesAsync(symbol, wf.Solution.Value, cancellationToken = ct)
                |> Async.AwaitTask

            let locationsFromReferencedSym (r: ReferencedSymbol) =
                let locations = r.Locations |> Seq.map _.Location

                match p.Context.IncludeDeclaration with
                | true -> locations |> Seq.append r.Definition.Locations
                | false -> locations

            return
                refs
                |> Seq.collect locationsFromReferencedSym
                |> Seq.choose Location.fromRoslynLocation
                |> Seq.distinct
                |> Seq.toArray
                |> Some
                |> LspResult.success
        | _, _ -> return None |> LspResult.success
    }
