namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module DocumentHighlight =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.DocumentHighlight)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DocumentHighlightOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: DocumentHighlightRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None
                }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/documentHighlight"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let private shouldHighlight (symbol: ISymbol) =
        match symbol with
        | :? INamespaceSymbol -> false
        | _ -> true

    let handle (context: ServerRequestContext) (p: DocumentHighlightParams) : AsyncLspResult<DocumentHighlight[] option> = async {
        let! ct = Async.CancellationToken
        let filePath = Uri.toPath p.TextDocument.Uri

        // We only need to find references in the file (not the whole workspace), so we don't use
        // context.FindSymbol & context.FindReferences here.
        let getHighlights (symbol: ISymbol) (doc: Document) = async {
            let docSet = ImmutableHashSet.Create(doc)
            let! refs = SymbolFinder.FindReferencesAsync(symbol, doc.Project.Solution, docSet, cancellationToken=ct) |> Async.AwaitTask
            let! def = SymbolFinder.FindSourceDefinitionAsync(symbol, doc.Project.Solution, cancellationToken=ct) |> Async.AwaitTask

            let locations =
                refs
                |> Seq.collect (fun r -> r.Locations)
                |> Seq.map (fun rl -> rl.Location)
                |> Seq.filter (fun l -> l.IsInSource && l.GetMappedLineSpan().Path = filePath)
                |> Seq.append (def |> Option.ofObj |> Option.toList |> Seq.collect (fun sym -> sym.Locations))

            return
                locations
                |> Seq.map Location.fromRoslynLocation
                |> Seq.filter _.IsSome
                |> Seq.map _.Value
                |> Seq.map (fun l ->
                    { Range = l.Range
                      Kind = Some DocumentHighlightKind.Read })
        }

        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> LspResult.success
        | Some (symbol, doc) ->
            if shouldHighlight symbol then
                let! highlights = getHighlights symbol doc
                return highlights |> Seq.toArray |> Some |> LspResult.success
            else
                return None |> LspResult.success
    }
