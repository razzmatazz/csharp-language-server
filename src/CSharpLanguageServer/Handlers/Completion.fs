namespace CSharpLanguageServer.Handlers

open System

open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Types

[<RequireQualifiedAccess>]
module Completion =
    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.Completion)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : CompletionOptions option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false ->
            Some { ResolveProvider = None
                   TriggerCharacters = Some ([| '.'; '''; |])
                   AllCommitCharacters = None
                   CompletionItem = None
                 }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/completion"
                  RegisterOptions =
                    { ResolveProvider = None
                      TriggerCharacters = Some ([| '.'; '''; |])
                      AllCommitCharacters = None
                      DocumentSelector = Some defaultDocumentSelector }
                    |> serialize
                    |> Some }

    let private roslynTagToLspCompletion tag =
        match tag with
        | "Class" -> CompletionItemKind.Class
        | "Delegate" -> CompletionItemKind.Function
        | "Enum" -> CompletionItemKind.Enum
        | "EnumMember" -> CompletionItemKind.EnumMember
        | "Interface" -> CompletionItemKind.Interface
        | "Struct" -> CompletionItemKind.Struct
        | "Local" -> CompletionItemKind.Variable
        | "Parameter" -> CompletionItemKind.Variable
        | "RangeVariable" -> CompletionItemKind.Variable
        | "Const" -> CompletionItemKind.Constant
        | "Event" -> CompletionItemKind.Event
        | "Field" -> CompletionItemKind.Field
        | "Method" -> CompletionItemKind.Method
        | "Property" -> CompletionItemKind.Property
        | "Label" -> CompletionItemKind.Unit
        | "Keyword" -> CompletionItemKind.Keyword
        | "Namespace" -> CompletionItemKind.Module
        | _ -> CompletionItemKind.Property

    // TODO: Add parameters to label so that we can distinguish override versions?
    // TODO: Add doc to response
    // TODO: Change parameters to snippets like clangd
    let private makeLspCompletionItem (item: Microsoft.CodeAnalysis.Completion.CompletionItem) =
        { Ionide.LanguageServerProtocol.Types.CompletionItem.Create(item.DisplayText) with
            Kind             = item.Tags |> Seq.tryHead |> Option.map roslynTagToLspCompletion
            SortText         = item.SortText |> Option.ofObj
            FilterText       = item.FilterText |> Option.ofObj
            Detail           = item.InlineDescription |> Option.ofObj
            InsertTextFormat = Some InsertTextFormat.PlainText }

    let handle (scope: ServerRequestScope) (p: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = async {
        let docMaybe = scope.GetUserDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let position =
                sourceText.Lines.GetPosition(LinePosition(p.Position.Line, p.Position.Character))

            let completionService = CompletionService.GetService(doc)
            // TODO: Avoid unnecessary GetCompletionsAsync. For example, for the first time, we will always get
            // `AbandonedMutexException`, `Accessibility`, ..., which are unnecessary and time-consuming.
            if isNull completionService then
                return ()

            let! completions = completionService.GetCompletionsAsync(doc, position) |> Async.AwaitTask

            match Option.ofObj completions with
            | None -> return None |> success
            | Some completions ->
                let items =
                    completions.ItemsList
                    |> Seq.map makeLspCompletionItem
                    |> Array.ofSeq
                let completionList =
                    { IsIncomplete = false
                      ItemDefaults = None
                      Items = items }
                return completionList |> Some |> success

        | None -> return success None
    }
