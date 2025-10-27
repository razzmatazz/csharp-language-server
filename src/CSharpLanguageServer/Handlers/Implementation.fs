namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Implementation =
    let provider (_: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        Some(U3.C1 true)

    let findImplementationsOfSymbol (context: ServerRequestContext) sym = async {
        let! impls = context.FindImplementations sym
        let! locations = impls |> Seq.map (flip context.ResolveSymbolLocations None) |> Async.Parallel

        return locations |> Array.collect List.toArray |> Declaration.C2 |> U2.C1 |> Some
    }

    let handle
        (context: ServerRequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        context.FindSymbol p.TextDocument.Uri p.Position
        |> Async.bindOption (findImplementationsOfSymbol context)
        |> Async.map LspResult.success
