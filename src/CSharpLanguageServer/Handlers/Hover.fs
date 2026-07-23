namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer
open CSharpLanguageServer.Types
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module Hover =
    let private dynamicRegistration (cc: ClientCapabilities) =
        cc.TextDocument
        |> Option.bind _.Hover
        |> Option.bind _.DynamicRegistration
        |> Option.defaultValue false

    let provider (cc: ClientCapabilities) : U2<bool, HoverOptions> option =
        match dynamicRegistration cc with
        | true -> Some(U2.C1 false)
        | false -> Some(U2.C1 true)

    let registration (config: CSharpConfiguration) (cc: ClientCapabilities) : Registration option =
        match dynamicRegistration cc with
        | false -> None
        | true ->
            let registerOptions: HoverRegistrationOptions =
                { DocumentSelector = documentSelectorForCSharpAndRazorDocuments config |> Some
                  WorkDoneProgress = None }

            Some
                { Id = Guid.NewGuid() |> string
                  Method = "textDocument/hover"
                  RegisterOptions = registerOptions |> serialize |> Some }

    let makeHoverForSymbol (project: Microsoft.CodeAnalysis.Project) symbol = async {
        let! ct = Async.CancellationToken
        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask

        let content =
            DocumentationUtil.markdownDocForSymbolWithSignature
                (compilation |> nonNull "project.GetCompilationAsync()")
                symbol

        return
            { Contents =
                { Kind = MarkupKind.Markdown
                  Value = content }
                |> U3.C1
              Range = None } // TODO: Support range
            |> Some
    }

    let handle (context: RequestContext) (p: HoverParams) : Async<LspResult<Hover option> * LspWorkspaceUpdate> = async {
        let! wf, _ = context.LoadWorkspaceFolder(p.TextDocument.Uri)

        match wf with
        | None -> return LspResult.success None, LspWorkspaceUpdate.Empty
        | Some wf ->
            let! symInfo = workspaceFolderDocumentSymbol AnyDocument p.TextDocument.Uri p.Position wf

            match symInfo with
            | Some(sym, project, _) ->
                let! hover = makeHoverForSymbol project sym
                return hover |> LspResult.success, LspWorkspaceUpdate.Empty
            | None -> return None |> LspResult.success, LspWorkspaceUpdate.Empty
    }
