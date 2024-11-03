namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module TypeDefinition =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) : bool =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.TypeDefinition)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U3<bool,TypeDefinitionOptions,TypeDefinitionRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U3.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: TypeDefinitionRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  Id = None
                  WorkDoneProgress = None }
            Some {
                Id = Guid.NewGuid().ToString()
                Method = "textDocument/typeDefinition"
                RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: TextDocumentPositionParams) : AsyncLspResult<Declaration option> = async {
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
                |> Seq.map (flip context.ResolveSymbolLocations (Some doc.Project))
                |> Async.Parallel
                |> Async.map (Seq.collect id >> Seq.toArray)
            return
                locations
                |> Declaration.C2
                |> Some
                |> success
    }
