namespace CSharpLanguageServer.Handlers

open System
open System.IO
open System.Text

open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Roslyn.Document
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module TextDocumentSync =
    let private logger = Logging.getLoggerByName "TextDocumentSync"

    let private applyLspContentChangesOnRoslynSourceText
        (changes: TextDocumentContentChangeEvent[])
        (initialSourceText: SourceText)
        =

        let applyLspContentChangeOnRoslynSourceText (sourceText: SourceText) (change: TextDocumentContentChangeEvent) =
            match change with
            | U2.C1 change ->
                let changeTextSpan =
                    change.Range
                    |> Range.toLinePositionSpan sourceText.Lines
                    |> sourceText.Lines.GetTextSpan

                TextChange(changeTextSpan, change.Text) |> sourceText.WithChanges
            | U2.C2 changeWoRange -> SourceText.From(changeWoRange.Text)

        changes |> Seq.fold applyLspContentChangeOnRoslynSourceText initialSourceText

    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind (fun x -> x.Synchronization)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : TextDocumentSyncOptions option =
        match dynamicRegistration cc with
        | true -> None
        | false ->
            Some
                { TextDocumentSyncOptions.Default with
                    OpenClose = Some true
                    Save = Some(U2.C2 { IncludeText = Some true })
                    Change = Some TextDocumentSyncKind.Incremental }

    let didOpenRegistration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didOpen"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didChangeRegistration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  SyncKind = TextDocumentSyncKind.Incremental }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didChange"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didSaveRegistration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  IncludeText = Some true }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didSave"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didCloseRegistration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didClose"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let willSaveRegistration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option = None

    let willSaveWaitUntilRegistration (_config: CSharpConfiguration) (_cc: ClientCapabilities) : Registration option =
        None

    let didOpenCshtmlFile wf (p: DidOpenTextDocumentParams) : Async<option<LspWorkspaceFolder>> = async {
        let cshtmlPath = workspaceFolderUriToPath p.TextDocument.Uri wf
        let newSourceText = SourceText.From(p.TextDocument.Text, Encoding.UTF8)

        match cshtmlPath with
        | None -> return None
        | Some cshtmlPath ->
            match workspaceFolderAdditionalTextDocumentForPath cshtmlPath wf with
            | Some doc ->
                let updatedWf =
                    workspaceFolderWithAdditionalTextDocumentTextUpdated doc newSourceText wf

                return Some updatedWf

            | None ->
                let updatedWf, _ =
                    workspaceFolderWithAdditionalTextDocumentAdded cshtmlPath p.TextDocument.Text wf

                return Some updatedWf
    }

    let didOpenCsharpFile
        wf
        (context: RequestContext)
        (p: DidOpenTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            let docInfo = workspaceFolderDocumentDetails AnyDocument p.TextDocument.Uri wf

            match docInfo with
            | Some(doc, docType) ->
                match docType with
                | UserDocument ->
                    // we want to load the document in case it has been changed since we have the solution loaded
                    // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                    // went out of sync with editor
                    let updatedWf =
                        workspaceFolderWithDocumentTextUpdated doc (p.TextDocument.Text |> SourceText.From) wf

                    return Some updatedWf

                | _ -> return None

            | None ->
                let docFilePathMaybe = workspaceFolderUriToPath p.TextDocument.Uri wf

                match docFilePathMaybe with
                | None -> return None
                | Some docFilePath ->
                    // ok, this document is not in solution, register a new document
                    let! updatedWf, newDocMaybe = workspaceFolderWithDocumentAdded docFilePath p.TextDocument.Text wf

                    return newDocMaybe |> Option.map (fun _ -> updatedWf)
        }

    let didOpen (context: RequestContext) (p: DidOpenTextDocumentParams) : Async<LspResult<unit> * RequestEffects> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        match wf with
        | None -> return Ok(), RequestEffects.Empty
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didOpenCshtmlFile wf p
                else
                    didOpenCsharpFile wf context p

            let effects =
                match updatedWf with
                | None -> RequestEffects.Empty
                | Some updatedWf ->
                    RequestEffects.Empty
                        .WithWorkspaceFolderChange(updatedWf)
                        .WithDocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now)

            return Ok(), effects
    }

    let didChangeCshtmlFile wf (p: DidChangeTextDocumentParams) : Async<option<LspWorkspaceFolder>> = async {
        let cshtmlPath = workspaceFolderUriToPath p.TextDocument.Uri wf

        let additionalDoc =
            cshtmlPath
            |> Option.bind (fun path -> workspaceFolderAdditionalTextDocumentForPath path wf)

        match additionalDoc with
        | None -> return None
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let updatedSourceText =
                sourceText |> applyLspContentChangesOnRoslynSourceText p.ContentChanges

            let updatedWf =
                workspaceFolderWithAdditionalTextDocumentTextUpdated doc updatedSourceText wf

            return Some updatedWf
    }

    let didChangeCsharpFile
        wf
        (context: RequestContext)
        (p: DidChangeTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            let doc = workspaceFolderDocument UserDocument p.TextDocument.Uri wf

            match doc with
            | None -> return None
            | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedSourceText =
                    sourceText |> applyLspContentChangesOnRoslynSourceText p.ContentChanges

                let updatedWf = workspaceFolderWithDocumentTextUpdated doc updatedSourceText wf
                return Some updatedWf
        }

    let didChange (context: RequestContext) (p: DidChangeTextDocumentParams) : Async<LspResult<unit> * RequestEffects> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        match wf with
        | None -> return Ok(), RequestEffects.Empty
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didChangeCshtmlFile wf p
                else
                    didChangeCsharpFile wf context p

            let effects =
                match updatedWf with
                | None -> RequestEffects.Empty
                | Some updatedWf ->
                    RequestEffects.Empty
                        .WithWorkspaceFolderChange(updatedWf)
                        .WithDocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now)

            return Ok(), effects
    }

    let willSave (_context: RequestContext) (_p: WillSaveTextDocumentParams) : Async<LspResult<unit> * RequestEffects> = async {
        return Ok(), RequestEffects.Empty
    }

    let willSaveWaitUntil
        (_context: RequestContext)
        (_p: WillSaveTextDocumentParams)
        : Async<LspResult<TextEdit[] option> * RequestEffects> =
        async { return LspResult.notImplemented<TextEdit[] option>, RequestEffects.Empty }

    let didSave (context: RequestContext) (p: DidSaveTextDocumentParams) : Async<LspResult<unit> * RequestEffects> = async {
        return Ok(), RequestEffects.Empty
    }

    let didCloseCshtmlFile wf (p: DidCloseTextDocumentParams) : Async<option<LspWorkspaceFolder>> = async {
        // reload this particular file from disk into Solution as there
        // could've been changes made to the in-memory file using didChange
        // and not persisted to disk before didClose (i.e. no didSave)
        //
        // if file does not exist on disk, remove it from the solution

        let cshtmlPath = workspaceFolderUriToPath p.TextDocument.Uri wf

        let additionalDoc =
            cshtmlPath
            |> Option.bind (fun path -> workspaceFolderAdditionalTextDocumentForPath path wf)

        match additionalDoc, cshtmlPath with
        | Some doc, Some filename ->
            if File.Exists filename then
                let sourceFromDisk = sourceTextFromFile filename

                let updatedWf =
                    workspaceFolderWithAdditionalTextDocumentTextUpdated doc sourceFromDisk wf

                return Some updatedWf
            else
                let updatedWf = workspaceFolderWithAdditionalDocumentRemoved p.TextDocument.Uri wf
                return Some updatedWf

        | _, _ -> return None
    }

    let didCloseCsharpFile
        wf
        (context: RequestContext)
        (p: DidCloseTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            // reload this particular file from disk into Solution as there
            // could've been changes made to the in-memory file using didChange
            // and not persisted to disk before didClose (i.e. no didSave)

            let docInfo = workspaceFolderDocumentDetails AnyDocument p.TextDocument.Uri wf

            let filename = workspaceFolderUriToPath p.TextDocument.Uri wf

            match docInfo, filename with
            | Some(doc, docType), Some filename ->
                match docType with
                | UserDocument ->
                    if File.Exists filename then
                        // reverting the file to original contents
                        let sourceFromDisk = sourceTextFromFile filename
                        let updatedWf = workspaceFolderWithDocumentTextUpdated doc sourceFromDisk wf
                        return Some updatedWf
                    else
                        let updatedWf = workspaceFolderWithDocumentRemoved p.TextDocument.Uri wf
                        return Some updatedWf

                | _ -> return None

            | _, _ -> return None
        }

    let didClose (context: RequestContext) (p: DidCloseTextDocumentParams) : Async<LspResult<unit> * RequestEffects> = async {
        let! wf, _ = context.GetWorkspaceFolderReadySolution(p.TextDocument.Uri)

        match wf with
        | None -> return Ok(), RequestEffects.Empty
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didCloseCshtmlFile wf p
                else
                    didCloseCsharpFile wf context p

            let effects =
                (match updatedWf with
                 | Some updatedWf -> RequestEffects.Empty.WithWorkspaceFolderChange(updatedWf)
                 | None -> RequestEffects.Empty)
                    .WithDocumentClosed(p.TextDocument.Uri)

            return Ok(), effects
    }
