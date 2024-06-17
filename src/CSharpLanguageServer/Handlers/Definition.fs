namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Definition =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Definition)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, DefinitionOptions> option =
        match dynamicRegistration (Some clientCapabilities) with
        | true -> None
        | false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: DefinitionRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  WorkDoneProgress = None }
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/definition"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: TextDocumentPositionParams) : AsyncLspResult<Declaration option> = async {
        match! context.FindSymbol' p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some (symbol, doc) ->
            let! locations = context.ResolveSymbolLocations symbol (Some doc.Project)
            return
                locations
                |> Array.ofList
                |> Declaration.C2
                |> Some
                |> success
    }
