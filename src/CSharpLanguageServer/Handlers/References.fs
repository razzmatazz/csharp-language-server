namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types

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

    let handle (context: RequestContext) (p: ReferenceParams) : AsyncLspResult<Location[] option> = async {
        let! ct = Async.CancellationToken
        let! wf = p.TextDocument.Uri |> context.GetWorkspaceFolder

        match wf with
        | None -> return None |> LspResult.success
        | Some wf ->
            let! symInfo = workspaceFolderDocumentSymbol wf AnyDocument p.TextDocument.Uri p.Position

            match symInfo with
            | None -> return LspResult.success None
            | Some(symbol, _, _) ->
                let wfPathToUri = workspaceFolderPathToUri wf

                let! refs =
                    SymbolFinder.FindReferencesAsync(symbol, wf.Solution.Value, cancellationToken = ct)
                    |> Async.AwaitTask

                let locationsFromReferencedSym (r: ReferencedSymbol) =
                    let locations = r.Locations |> Seq.map _.Location

                    match p.Context.IncludeDeclaration with
                    | true -> locations |> Seq.append r.Definition.Locations
                    | false -> locations

                return
                    refs
                    |> Seq.collect locationsFromReferencedSym
                    |> Seq.choose (Location.fromRoslynLocation wfPathToUri)
                    |> Seq.distinct
                    |> Seq.toArray
                    |> Some
                    |> LspResult.success
    }
