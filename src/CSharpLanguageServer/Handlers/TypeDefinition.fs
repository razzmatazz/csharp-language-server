namespace CSharpLanguageServer.Handlers

open System

open FSharpPlus
open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Types
open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module TypeDefinition =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) : bool =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.TypeDefinition)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some true

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some {
                Id = Guid.NewGuid().ToString()
                Method = "textDocument/typeDefinition"
                RegisterOptions = { DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: TextDocumentPositionParams) : AsyncLspResult<GotoResult option> = async {
        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc) ->
            let typeSymbol =
                match symbol with
                | :? ILocalSymbol as localSymbol -> [localSymbol.Type]
                | :? IFieldSymbol as fieldSymbol -> [fieldSymbol.Type]
                | :? IPropertySymbol as propertySymbol -> [propertySymbol.Type]
                | :? IParameterSymbol as parameterSymbol -> [parameterSymbol.Type]
                | _ -> []
            let! locations =
                typeSymbol
                |> map (flip context.ResolveSymbolLocations (Some doc.Project))
                |> Async.Parallel
                |> map (Seq.collect id >> Seq.toArray)
            return
                locations
                |> GotoResult.Multiple
                |> Some
                |> success
    }
