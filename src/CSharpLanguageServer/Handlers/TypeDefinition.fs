namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module TypeDefinition =
    let private dynamicRegistration (cc: ClientCapabilities) : bool =
        cc.TextDocument
        |> Option.bind _.TypeDefinition
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: TypeDefinitionRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/typeDefinition"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: RequestContext)
        (p: TypeDefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =

        async {
            let! wf = p.TextDocument.Uri |> context.GetWorkspaceFolder

            match wf with
            | None -> return None |> LspResult.success
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol wf AnyDocument p.TextDocument.Uri p.Position

                match symInfo with
                | None -> return LspResult.success None
                | Some(symbol, project, _) ->
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
                                workspaceFolderSymbolLocations wf context.Config symbol project

                            context.UpdateEffects(_.WithWorkspaceFolderChange(updatedWf))
                            return aggregatedLspLocations, updatedWf
                          }

                    return locations |> Seq.toArray |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success
        }
