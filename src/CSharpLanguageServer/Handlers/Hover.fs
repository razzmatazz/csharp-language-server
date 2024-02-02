namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open CSharpLanguageServer
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module Hover =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (hoverPos: Types.TextDocumentPositionParams): AsyncLspResult<Types.Hover option> = async {
        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument hoverPos.TextDocument.Uri hoverPos.Position

        let! contents =
            match maybeSymbol with
            | Some (sym, doc, pos) -> async {
                let! ct = Async.CancellationToken
                let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

                return Documentation.markdownDocForSymbolWithSignature sym semanticModel pos
                       |> fun s -> MarkedString.WithLanguage { Language = "markdown"; Value = s }
                       |> fun s -> [s]
              }
            | _ -> async { return [] }

        return Some { Contents = contents |> Array.ofSeq |> MarkedStrings
                      Range = None } |> success
    }
