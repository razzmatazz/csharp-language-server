namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime
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

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: DefinitionRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/definition"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: RequestContext)
        (p: DefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            let! wf = p.TextDocument.Uri |> context.GetWorkspaceFolder

            match wf with
            | None -> return None |> LspResult.success
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol wf AnyDocument p.TextDocument.Uri p.Position

                match symInfo with
                | None -> return None |> LspResult.success
                | Some(symbol, project, _) ->
                    let! locations, updatedWf = workspaceFolderSymbolLocations wf context.Config symbol project

                    context.UpdateEffects(_.WithWorkspaceFolderChange(updatedWf))

                    return locations |> Array.ofList |> Definition.C2 |> U2.C1 |> Some |> LspResult.success
        }
