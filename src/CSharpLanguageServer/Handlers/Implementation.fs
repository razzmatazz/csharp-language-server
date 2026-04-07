namespace CSharpLanguageServer.Handlers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Implementation =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Implementation
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U3<bool, ImplementationOptions, ImplementationRegistrationOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U3.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: ImplementationRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  Id = None
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/implementation"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let findImplLocationsOfSymbol wf config project (sym: ISymbol) = async {
        let! ct = Async.CancellationToken

        match wf.Solution with
        | Ready(_, solution) ->
            let! impls =
                SymbolFinder.FindImplementationsAsync(sym, solution, cancellationToken = ct)
                |> Async.AwaitTask

            let mutable updatedWf = wf

            let locations = System.Collections.Generic.List<Location>()

            for i in impls do
                let! implLocations, wf = workspaceFolderSymbolLocations updatedWf config i project

                locations.AddRange(implLocations)
                updatedWf <- wf

            return locations |> Seq.toArray, updatedWf

        | _ -> return [||], wf
    }

    let handle
        (context: RequestContext)
        (p: ImplementationParams)
        : Async<LspResult<U2<Definition, DefinitionLink array> option> * RequestEffects> =
        async {
            let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

            match wf with
            | None -> return None |> LspResult.success, RequestEffects.Empty
            | Some wf ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | None -> return LspResult.success None, RequestEffects.Empty
                | Some(sym, project, _) ->
                    let! impls, updatedWf = findImplLocationsOfSymbol wf context.Config project sym

                    return
                        impls |> Declaration.C2 |> U2.C1 |> Some |> LspResult.success,
                        RequestEffects.Empty.WithWorkspaceFolderChange(updatedWf)
        }
