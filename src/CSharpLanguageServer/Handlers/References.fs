namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module References =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.References
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, ReferenceOptions> option =
        match dynamicRegistration cc with
        | true -> None
        | false -> Some(U2.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: ReferenceRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/references"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let handle
        (context: RequestContext)
        (p: ReferenceParams)
        : Async<LspResult<Location[] option> * LspWorkspaceUpdate> =
        async {
            let! ct = Async.CancellationToken
            let! wf, solution = p.TextDocument.Uri |> context.LoadWorkspaceFolder

            match wf, solution with
            | Some wf, Some solution ->
                let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

                match symInfo with
                | None -> return LspResult.success None, LspWorkspaceUpdate.Empty
                | Some(symbol, _, _) ->
                    let wfPathToUri path = workspaceFolderPathToUri path wf

                    // SymbolFinder.FindReferencesAsync(symbol, solution) does not search
                    // SourceGeneratedDocuments (Roslyn bug #63375). The document-set
                    // overload restricts the search to only the given set, so we must
                    // include all regular documents too — not just the generated ones.
                    let! sourceGenDocArrays =
                        solution.Projects
                        |> Seq.map (fun p -> p.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask)
                        |> Async.Parallel

                    let allDocs =
                        let regularDocs = solution.Projects |> Seq.collect _.Documents |> Seq.cast<Document>

                        sourceGenDocArrays
                        |> Seq.concat
                        |> Seq.cast<Document>
                        |> Seq.append regularDocs
                        |> ImmutableHashSet.CreateRange

                    let! refs =
                        SymbolFinder.FindReferencesAsync(symbol, solution, allDocs, ct)
                        |> Async.AwaitTask

                    let refLocations =
                        refs
                        |> Seq.collect _.Locations
                        |> Seq.map _.Location
                        |> Seq.choose (Location.fromRoslynLocation wfPathToUri)
                        |> Seq.distinct
                        |> List.ofSeq

                    // Resolve definition locations through workspaceFolderSymbolLocations
                    // so that useMetadataUris decompilation is triggered for BCL symbols,
                    // just like textDocument/definition and textDocument/implementation.
                    let! defLocations, wfUpdates =
                        if not p.Context.IncludeDeclaration then
                            async.Return([], [])
                        else
                            async {
                                let mutable aggregatedWf = wf
                                let mutable aggregatedWfUpdates = []
                                let defLocs = System.Collections.Generic.List<Location>()

                                // Any project suffices for metadata decompilation; for source
                                // symbols Location.fromRoslynLocation resolves without a project.
                                let anyProject = solution.Projects |> Seq.head

                                for r in refs do
                                    let! lspLocs, wfUpds =
                                        aggregatedWf
                                        |> workspaceFolderSymbolLocations context.Config r.Definition anyProject

                                    aggregatedWf <- wfUpds |> List.fold (|>) aggregatedWf
                                    aggregatedWfUpdates <- aggregatedWfUpdates @ wfUpds
                                    defLocs.AddRange(lspLocs)

                                return defLocs |> List.ofSeq, aggregatedWfUpdates
                            }

                    let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)

                    return
                        Seq.append defLocations refLocations
                        |> Seq.distinct
                        |> Seq.toArray
                        |> Some
                        |> LspResult.success,
                        wsUpdate

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
