namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module DocumentSymbol =
    let provider (clientCapabilities: ClientCapabilities option) : U2<bool,DocumentSymbolOptions> option =
        true |> U2.First |> Some

    let handle (scope: ServerRequestScope) (p: DocumentSymbolParams): AsyncLspResult<DocumentSymbol [] option> = async {
        let canEmitDocSymbolHierarchy =
            scope.ClientCapabilities
            |> Option.bind (fun cc -> cc.TextDocument)
            |> Option.bind (fun cc -> cc.DocumentSymbol)
            |> Option.bind (fun cc -> cc.HierarchicalDocumentSymbolSupport)
            |> Option.defaultValue false

        let docMaybe = scope.GetAnyDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let! syntaxTree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask

            let collector = DocumentSymbolCollector(docText, semanticModel)
            collector.Init(doc.Name)
            collector.Visit(syntaxTree.GetRoot())

            return collector.GetDocumentSymbols(canEmitDocSymbolHierarchy)
                   |> Some
                   |> LspResult.success

        | None ->
            return None |> LspResult.success
    }
