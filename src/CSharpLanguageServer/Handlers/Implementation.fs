namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols

open CSharpLanguageServer
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Implementation =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (def: TextDocumentPositionParams): AsyncLspResult<GotoResult option> = async {
        let docMaybe = scope.GetAnyDocumentForUri def.TextDocument.Uri
        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position = sourceText.Lines.GetPosition(LinePosition(def.Position.Line, def.Position.Character))
            let! symbolMaybe = SymbolFinder.FindSymbolAtPositionAsync(doc, position, ct) |> Async.AwaitTask

            let! symbols = async {
                match Option.ofObj symbolMaybe with
                | Some sym ->
                    let! implSymbols =
                        SymbolFinder.FindImplementationsAsync(sym, scope.Solution)
                        |> Async.AwaitTask

                    return implSymbols |> List.ofSeq
                | None -> return []
            }

            let! locations = scope.ResolveSymbolLocations doc.Project symbols

            return
                locations
                |> Array.ofSeq
                |> GotoResult.Multiple
                |> Some
                |> success
    }
