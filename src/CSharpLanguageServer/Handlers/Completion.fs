namespace CSharpLanguageServer.Handlers

open System

open FSharpPlus
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions
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
                   CompletionItem = None }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: CompletionRegistrationOptions =
                    { DocumentSelector = Some defaultDocumentSelector
                      ResolveProvider = Some true
                      TriggerCharacters = Some ([| '.'; '''; |])
                      AllCommitCharacters = None
                      CompletionItem = None
                    }

            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/completion"
                  RegisterOptions = registerOptions |> serialize |> Some }

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
    // TODO: Change parameters to snippets like clangd
    let private makeLspCompletionItem
        (item: Microsoft.CodeAnalysis.Completion.CompletionItem)
        (cacheKey: uint64) =
        { CompletionItem.Create(item.DisplayText) with
            Kind             = item.Tags |> Seq.tryHead |> Option.map roslynTagToLspCompletion
            SortText         = item.SortText |> Option.ofString
            FilterText       = item.FilterText |> Option.ofString
            Detail           = item.InlineDescription |> Option.ofString
            TextEditText     = item.DisplayTextPrefix |> Option.ofObj
            InsertTextFormat = Some InsertTextFormat.PlainText
            Data             = cacheKey |> serialize |> Some }

    let private cache = new LruCache<(Microsoft.CodeAnalysis.Document * Microsoft.CodeAnalysis.Completion.CompletionList)>(5)

    let handle (context: ServerRequestContext) (p: CompletionParams) : AsyncLspResult<CompletionList option> = async {
        match context.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let position = Position.toRoslynPosition sourceText.Lines p.Position

            let completionService = Microsoft.CodeAnalysis.Completion.CompletionService.GetService(doc)
            let completionTrigger = CompletionContext.toCompletionTrigger p.Context
            let shouldTriggerCompletion =
                p.Context |> Option.exists (fun x -> x.TriggerKind = CompletionTriggerKind.TriggerForIncompleteCompletions) ||
                completionService.ShouldTriggerCompletion(sourceText, position, completionTrigger)
            let! completions =
                if shouldTriggerCompletion then
                    completionService.GetCompletionsAsync(doc, position, completionTrigger, cancellationToken=ct) |> Async.AwaitTask
                else
                    async.Return null

            return
                completions
                |> Option.ofObj
                |> map (fun completions ->
                    let key = cache.add((doc, completions))
                    let items =
                        completions.ItemsList
                        |> Seq.map (flip makeLspCompletionItem key)
                        |> Array.ofSeq
                    { IsIncomplete = true
                      Items = items
                      ItemDefaults = None })
                |> success
    }

    let resolve (context: ServerRequestContext) (item: CompletionItem) : AsyncLspResult<CompletionItem> = async {
        match
            item.Data
            |> Option.bind deserialize
            |> Option.bind cache.get
            |> Option.bind (fun (doc, cachedItems) ->
                cachedItems.ItemsList
                |> Seq.tryFind (fun x -> x.DisplayText = item.Label && (item.SortText.IsNone || x.SortText = item.SortText.Value))
                |> Option.map (fun x -> (doc, x)))
        with
        | None -> return item |> success
        | Some (doc, cachedItem) ->
            let completionService = Microsoft.CodeAnalysis.Completion.CompletionService.GetService(doc)
            let! ct = Async.CancellationToken
            let! description =
                completionService.GetDescriptionAsync(doc, cachedItem, ct)
                |> map Option.ofObj
                |> Async.AwaitTask
            // TODO: make the doc as a markdown string instead of a plain text
            return { item with Documentation = description |> map (fun x -> Documentation.String x.Text) } |> success
    }
