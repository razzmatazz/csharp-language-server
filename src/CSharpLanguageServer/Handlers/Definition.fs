namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Definition =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Definition
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, DefinitionOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DefinitionRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/definition"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: ServerRequestContext)
        (p: DefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, _) ->
                let! locations, updatedWf = workspaceFolderSymbolLocations wf context.State.Settings symbol project

                context.Emit(WorkspaceFolderChange updatedWf)

                return locations |> Array.ofList |> Definition.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
