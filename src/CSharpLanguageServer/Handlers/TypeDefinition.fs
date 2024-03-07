namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.State

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols

[<RequireQualifiedAccess>]
module TypeDefinition =
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

            let typeSymbols =
                match symbols with
                | [] -> []
                | _  ->
                    match symbols.Head with
                    | :? ILocalSymbol as loc -> [loc.Type]
                    | :? IFieldSymbol as field -> [field.Type]
                    | :? IPropertySymbol as prop -> [prop.Type]
                    | :? IParameterSymbol as param -> [param.Type]
                    | _ -> []

            let! locations =
                    scope.ResolveTypeSymbolLocations doc.Project typeSymbols

            return locations |> Array.ofSeq |> GotoResult.Multiple |> Some |> success
    }
