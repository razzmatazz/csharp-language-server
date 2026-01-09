namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Implementation =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind _.Implementation
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider
        (clientCapabilities: ClientCapabilities)
        : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: ImplementationRegistrationOptions =
                { DocumentSelector = Some defaultDocumentSelector
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/implementation"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let findImplLocationsOfSymbol wf settings project (sym: ISymbol) = async {
        let! ct = Async.CancellationToken

        let! impls =
            SymbolFinder.FindImplementationsAsync(sym, wf.Solution.Value, cancellationToken = ct)
            |> Async.AwaitTask

        let mutable updatedWf = wf

        let locations = System.Collections.Generic.List<Location>()

        for i in impls do
            let! implLocations, wf = workspaceFolderSymbolLocations updatedWf settings i project

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
                let! impls, updatedWf = findImplLocationsOfSymbol wf context.State.Settings project sym
                context.Emit(WorkspaceFolderChange updatedWf)

                return impls |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success

            | _, _ -> return None |> LspResult.success
        }
