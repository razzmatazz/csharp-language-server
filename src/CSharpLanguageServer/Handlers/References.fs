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
                | Some(symbol, symbolProject, _) ->
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

                    let! defLocations, wfUpdates =
                        match p.Context.IncludeDeclaration with
                        | false -> async.Return([], [])
                        | true -> async {
                            // Resolve definition locations through workspaceFolderSymbolsLocations so
                            // that useMetadataUris decompilation is triggered for BCL symbols, just
                            // like textDocument/definition and textDocument/implementation.
                            //
                            // Use the project that owns the document the request originated from
                            // (the same project `workspaceFolderDocumentSymbol` resolved `symbol`
                            // against), not an arbitrary project from the solution. Metadata
                            // decompilation needs a project whose compilation actually references
                            // the found symbol's containing assembly:
                            // `compilation.GetMetadataReference(containingAssembly)` returns null
                            // (crashing with "A non-null value was expected") when handed a project
                            // that has no such reference, which an arbitrary "first" project in a
                            // large multi-project solution is not guaranteed to have.
                            let! symbolLocations, wfUpdates =
                                wf
                                |> workspaceFolderSymbolsLocations
                                    context.Config
                                    symbolProject
                                    (refs |> Seq.map _.Definition)

                            return symbolLocations |> List.collect snd, wfUpdates
                          }

                    let lspResult =
                        Seq.append defLocations refLocations
                        |> Seq.distinct
                        |> Seq.toArray
                        |> Some
                        |> LspResult.success

                    let wsUpdate = LspWorkspaceUpdate.Empty.WithFolderUpdates(wf.Uri, wfUpdates)

                    return lspResult, wsUpdate

            | _, _ -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
        }
