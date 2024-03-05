namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Definition =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (def: TextDocumentPositionParams) : AsyncLspResult<GotoResult option> = async {
        let docMaybe = scope.GetAnyDocumentForUri def.TextDocument.Uri

        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

            let symbols =
                match Option.ofObj symbolMaybe with
                | Some sym -> [sym]
                | None -> []

            let! locations =
                    scope.ResolveSymbolLocations doc.Project symbols

            return
                locations
                |> Array.ofList
                |> GotoResult.Multiple
                |> Some
                |> success
    }
