namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Implementation =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        false
        // TODO: 
        // clientCapabilities
        // |> Option.bind (fun x -> x.TextDocument)
        // |> Option.bind (fun x -> x.Implementation)
        // |> Option.bind (fun x -> x.DynamicRegistration)
        // |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/implementation"
                  RegisterOptions = { DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

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
