namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module References =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.References)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, ReferenceOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some (U2.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: ReferenceRegistrationOptions =
                {
                    DocumentSelector = Some defaultDocumentSelector
                    WorkDoneProgress = None
                }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/references"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle (context: ServerRequestContext) (p: ReferenceParams) : AsyncLspResult<Location[] option> = async {
        match! context.FindSymbol p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some symbol ->
            let! locations = context.FindReferences symbol p.Context.IncludeDeclaration

            return
                locations
                |> Seq.map Location.fromRoslynLocation
                |> Seq.distinct
                |> Seq.toArray
                |> Some
                |> success
    }
