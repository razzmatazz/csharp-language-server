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
open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Roslyn.Solution
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

    let didOpenRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didOpen"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didChangeRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  SyncKind = TextDocumentSyncKind.Incremental }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didChange"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didSaveRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some
                  IncludeText = Some true }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didSave"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let didCloseRegistration (settings: ServerSettings) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments settings |> Some }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/didClose"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let willSaveRegistration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let willSaveWaitUntilRegistration (_settings: ServerSettings) (_cc: ClientCapabilities) : Registration option = None

    let didOpenCshtmlFile wf (p: DidOpenTextDocumentParams) : Async<option<LspWorkspaceFolder>> = async {
        let cshtmlPath = p.TextDocument.Uri |> workspaceFolderUriToPath wf
        let project = cshtmlPath |> Option.bind (workspaceFolderProjectForPath wf)

        let textDocument =
            match project with
            | None -> None
            | Some project ->
                let existingAdditionalDoc =
                    project
                    |> _.AdditionalDocuments
                    |> Seq.filter (fun d -> Some d.FilePath = cshtmlPath)
                    |> List.ofSeq
                    |> List.singleOrNone

                let newSourceText = SourceText.From(p.TextDocument.Text, Encoding.UTF8)

                match existingAdditionalDoc with
                | Some doc ->
                    project
                    |> _.RemoveAdditionalDocument(doc.Id)
                    |> _.AddAdditionalDocument(doc.Name, newSourceText, doc.Folders, doc.FilePath)
                    |> Some

                | None ->
                    let cshtmlPath = cshtmlPath.Value
                    let projectBaseDir = Path.GetDirectoryName project.FilePath
                    let relativePath = Path.GetRelativePath(projectBaseDir, cshtmlPath)

                    let folders = relativePath.Split Path.DirectorySeparatorChar
                    let folders = folders |> Seq.take (folders.Length - 1)

                    project
                    |> _.AddAdditionalDocument(Path.GetFileName cshtmlPath, newSourceText, folders, cshtmlPath)
                    |> Some

        match textDocument with
        | None -> return None
        | Some textDocument ->
            let updatedWf =
                textDocument
                |> _.Project.Solution
                |> (fun sln -> { wf with Solution = Some sln })

            return Some updatedWf
    }

    let didOpenCsharpFile
        wf
        (context: ServerRequestContext)
        (p: DidOpenTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            let _, docInfo =
                workspaceDocumentDetails context.Workspace AnyDocument p.TextDocument.Uri

            match docInfo with
            | Some(doc, docType) ->
                match docType with
                | UserDocument ->
                    // we want to load the document in case it has been changed since we have the solution loaded
                    // also, as a bonus we can recover from corrupted document view in case document in roslyn solution
                    // went out of sync with editor

                    let updatedWf =
                        p.TextDocument.Text
                        |> SourceText.From
                        |> doc.WithText
                        |> _.Project.Solution
                        |> (fun sln -> { wf with Solution = Some sln })

                    return Some updatedWf

                | _ -> return None

            | None ->
                let docFilePathMaybe = p.TextDocument.Uri |> workspaceFolderUriToPath wf

                match docFilePathMaybe with
                | None -> return None
                | Some docFilePath ->
                    // ok, this document is not in solution, register a new document
                    let! updatedWf, newDocMaybe = workspaceFolderWithDocumentAdded wf docFilePath p.TextDocument.Text

                    return newDocMaybe |> Option.map (fun _ -> updatedWf)
        }

    let didOpen (context: ServerRequestContext) (p: DidOpenTextDocumentParams) : Async<LspResult<unit>> = async {
        let wf = p.TextDocument.Uri |> workspaceFolder context.Workspace

        match wf with
        | None -> return Ok()
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didOpenCshtmlFile wf p
                else
                    didOpenCsharpFile wf context p

            match updatedWf with
            | None -> ()
            | Some updatedWf ->
                context.Emit(WorkspaceFolderChange updatedWf)
                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

            return Ok()
    }

    let didChangeCshtmlFile wf (p: DidChangeTextDocumentParams) : Async<option<LspWorkspaceFolder>> = async {
        let cshtmlPath = p.TextDocument.Uri |> workspaceFolderUriToPath wf
        let project = cshtmlPath |> Option.bind (workspaceFolderProjectForPath wf)

        let existingAdditionalDoc =
            project
            |> (fun p -> if p.IsSome then p.Value.AdditionalDocuments else [])
            |> Seq.filter (fun d -> Some d.FilePath = cshtmlPath)
            |> List.ofSeq
            |> List.singleOrNone

        match existingAdditionalDoc with
        | None -> return None
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let updatedSourceText =
                sourceText |> applyLspContentChangesOnRoslynSourceText p.ContentChanges

            let updatedSolution =
                doc.Project
                |> _.RemoveAdditionalDocument(doc.Id)
                |> _.AddAdditionalDocument(doc.Name, updatedSourceText, doc.Folders, doc.FilePath)
                |> _.Project.Solution
                |> Some

            let updatedWf = { wf with Solution = updatedSolution }
            return Some updatedWf
    }

    let didChangeCsharpFile
        wf
        (context: ServerRequestContext)
        (p: DidChangeTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            let _, doc = workspaceDocument context.Workspace UserDocument p.TextDocument.Uri

            match doc with
            | None -> return None
            | Some doc ->
                let! ct = Async.CancellationToken
                let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

                let updatedSolution =
                    sourceText
                    |> applyLspContentChangesOnRoslynSourceText p.ContentChanges
                    |> doc.WithText
                    |> _.Project.Solution
                    |> Some

                let updatedWf = { wf with Solution = updatedSolution }
                return Some updatedWf
        }

    let didChange (context: ServerRequestContext) (p: DidChangeTextDocumentParams) : Async<LspResult<unit>> = async {
        let wf = p.TextDocument.Uri |> workspaceFolder context.Workspace

        match wf with
        | None -> return Ok()
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didChangeCshtmlFile wf p
                else
                    didChangeCsharpFile wf context p

            match updatedWf with
            | None -> ()
            | Some updatedWf ->
                context.Emit(WorkspaceFolderChange updatedWf)
                context.Emit(DocumentOpened(p.TextDocument.Uri, p.TextDocument.Version, DateTime.Now))

            return Ok()
    }

    let willSave (_context: ServerRequestContext) (_p: WillSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        return Ok()
    }

    let willSaveWaitUntil
        (_context: ServerRequestContext)
        (_p: WillSaveTextDocumentParams)
        : AsyncLspResult<TextEdit[] option> =
        async { return LspResult.notImplemented<TextEdit[] option> }

    let didSave (context: ServerRequestContext) (p: DidSaveTextDocumentParams) : Async<LspResult<unit>> = async {
        return Ok()
    }

    let didCloseCshtmlFile
        wf
        (context: ServerRequestContext)
        (p: DidCloseTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            // TODO: reload this particular file from disk into Solution as there
            //       could've been changes made to the in-memory file using didChange
            //       and not persisted to disk before didClose (i.e. no didSave)

            return None
        }

    let didCloseCsharpFile
        wf
        (context: ServerRequestContext)
        (p: DidCloseTextDocumentParams)
        : Async<option<LspWorkspaceFolder>> =
        async {
            // TODO: reload this particular file from disk into Solution as there
            //       could've been changes made to the in-memory file using didChange
            //       and not persisted to disk before didClose (i.e. no didSave)

            return None
        }

    let didClose (context: ServerRequestContext) (p: DidCloseTextDocumentParams) : Async<LspResult<unit>> = async {
        let wf = p.TextDocument.Uri |> workspaceFolder context.Workspace

        match wf with
        | None -> return Ok()
        | Some wf ->
            let! updatedWf =
                if p.TextDocument.Uri.EndsWith ".cshtml" then
                    didCloseCshtmlFile wf context p
                else
                    didCloseCsharpFile wf context p

            context.Emit(DocumentClosed p.TextDocument.Uri)

            return Ok()
    }
