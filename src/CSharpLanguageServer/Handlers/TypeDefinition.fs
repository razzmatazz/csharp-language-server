namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module TypeDefinition =
    let provider (_: ClientCapabilities) : U3<bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions> option =
        Some(U3.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: TypeDefinitionParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            match! context.FindSymbol' p.TextDocument.Uri p.Position with
            | None -> return None |> LspResult.success
            | Some(symbol, project, _) ->
                let typeSymbol =
                    match symbol with
                    | :? ILocalSymbol as localSymbol -> [ localSymbol.Type ]
                    | :? IFieldSymbol as fieldSymbol -> [ fieldSymbol.Type ]
                    | :? IPropertySymbol as propertySymbol -> [ propertySymbol.Type ]
                    | :? IParameterSymbol as parameterSymbol -> [ parameterSymbol.Type ]
                    | _ -> []

                let! locations =
                    typeSymbol
                    |> Seq.map (flip context.ResolveSymbolLocations (Some project))
                    |> Async.Parallel
                    |> Async.map (Seq.collect id >> Seq.toArray)

                return locations |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success
        }
