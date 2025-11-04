namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module TypeDefinition =
    let provider (_cc: ClientCapabilities) : U3<bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions> option =
        Some(U3.C1 true)

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
                        let! aggregatedLspLocations, updatedWf = workspaceFolderSymbolLocations symbol (Some project) wf

                        context.Emit(WorkspaceFolderChange updatedWf)
                        return (aggregatedLspLocations, updatedWf)
                      }

                return locations |> Seq.toArray |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
