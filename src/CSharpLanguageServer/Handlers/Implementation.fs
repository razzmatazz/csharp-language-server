namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Implementation =
    let provider (_cc: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        Some(U3.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =

        let findImplementationsOfSymbol (sym: ISymbol) = async {
            let! ct = Async.CancellationToken

            let! impls =
                SymbolFinder.FindImplementationsAsync(sym, context.Solution, cancellationToken = ct)
                |> Async.AwaitTask

            let! locations = impls |> Seq.map (flip context.ResolveSymbolLocations None) |> Async.Parallel

            return locations |> Array.collect List.toArray |> Declaration.C2 |> U2.C1 |> Some
        }

        context.FindSymbol p.TextDocument.Uri p.Position
        |> Async.bindOption findImplementationsOfSymbol
        |> Async.map LspResult.success
