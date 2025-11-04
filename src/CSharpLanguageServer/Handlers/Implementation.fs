namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module Implementation =
    let provider (_cc: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        Some(U3.C1 true)

    let handle
        (context: ServerRequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {

            let findImplementationsOfSymbol sln (sym: ISymbol) = async {
                let! ct = Async.CancellationToken

                let! impls =
                    SymbolFinder.FindImplementationsAsync(sym, sln, cancellationToken = ct)
                    |> Async.AwaitTask

                let! locations =
                    impls
                    |> Seq.map (fun i -> context.ResolveSymbolLocations i None)
                    |> Async.Parallel

                return locations |> Array.collect List.toArray |> Declaration.C2 |> U2.C1 |> Some
            }

            let! wf, symInfo = workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position

            match wf, symInfo with
            | Some wf, Some(sym, _, _) ->
                let! impls = findImplementationsOfSymbol wf.Solution.Value sym
                return impls |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
