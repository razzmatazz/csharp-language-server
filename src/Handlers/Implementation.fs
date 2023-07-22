namespace CSharpLanguageServer.Handlers

open System
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module Implementation =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Implementation)
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
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/implementation"
                  RegisterOptions = { DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

    let handle (wm: IWorkspaceManager) (p: TextDocumentPositionParams) : AsyncLspResult<GotoResult option> = async {
        match! wm.FindSymbol p.TextDocument.Uri p.Position with
        | None -> return None |> success
        | Some symbol ->
            let! impls = wm.FindImplementations symbol
            let! locations = impls |> Seq.map wm.ResolveSymbolLocations |> Async.Parallel

            return
                locations
                |> Array.collect List.toArray
                |> GotoResult.Multiple
                |> Some
                |> success
    }
