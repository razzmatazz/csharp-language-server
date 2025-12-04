namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module Implementation =
    let provider (_cc: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        Some(U3.C1 true)

    let findImplLocationsOfSymbol wf project (sym: ISymbol) = async {
        let! ct = Async.CancellationToken

        let! impls =
            SymbolFinder.FindImplementationsAsync(sym, wf.Solution.Value, cancellationToken = ct)
            |> Async.AwaitTask

        let mutable updatedWf = wf

        let locations = System.Collections.Generic.List<Location>()

        for i in impls do
            let! implLocations, wf = workspaceFolderSymbolLocations i project updatedWf

            locations.AddRange(implLocations)
            updatedWf <- wf

        return locations |> Seq.toArray, updatedWf
    }

    let handle
        (context: ServerRequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option>> =
        async {
            let! wf, symInfo = workspaceDocumentSymbol context.Workspace AnyDocument p.TextDocument.Uri p.Position

            match wf, symInfo with
            | Some wf, Some(sym, project, _) ->
                let! impls, updatedWf = findImplLocationsOfSymbol wf project sym
                context.Emit(WorkspaceFolderChange updatedWf)

                return impls |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
