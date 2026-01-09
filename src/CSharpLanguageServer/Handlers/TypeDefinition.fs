namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module TypeDefinition =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) : bool =
        clientCapabilities.TextDocument
        |> Option.bind _.TypeDefinition
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider
        (clientCapabilities: ClientCapabilities)
        : U3<bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: TypeDefinitionRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/typeDefinition"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: ServerRequestContext)
        (p: TypeDefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =

        async {
            match! workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position with
            | Some wf, Some(symbol, project, _) ->
                let typeSymbol =
                    match symbol with
                    | :? ILocalSymbol as localSymbol -> Some localSymbol.Type
                    | :? IFieldSymbol as fieldSymbol -> Some fieldSymbol.Type
                    | :? IPropertySymbol as propertySymbol -> Some propertySymbol.Type
                    | :? IParameterSymbol as parameterSymbol -> Some parameterSymbol.Type
                    | _ -> None

                let! locations, wf =
                    match typeSymbol with
                    | None -> async.Return([], wf)
                    | Some symbol -> async {
                        let! aggregatedLspLocations, updatedWf =
                            workspaceFolderSymbolLocations wf context.State.Settings symbol project

                        context.Emit(WorkspaceFolderChange updatedWf)
                        return aggregatedLspLocations, updatedWf
                      }

                return locations |> Seq.toArray |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
