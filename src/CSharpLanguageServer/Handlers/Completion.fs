namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module Completion =
    let provider (clientCapabilities: ClientCapabilities option) : CompletionOptions option =
        Some { ResolveProvider = None
               TriggerCharacters = Some ([| '.'; '''; |])
               AllCommitCharacters = None
               CompletionItem = None
             }

    let handle (scope: ServerRequestScope) (posParams: Types.CompletionParams): AsyncLspResult<Types.CompletionList option> = async {
        let docMaybe = scope.GetUserDocumentForUri posParams.TextDocument.Uri
        match docMaybe with
        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let posInText = docText.Lines.GetPosition(LinePosition(posParams.Position.Line, posParams.Position.Character))

            let completionService = CompletionService.GetService(doc)
            if isNull completionService then
                return ()

            let! maybeCompletionResults =
                completionService.GetCompletionsAsync(doc, posInText) |> Async.AwaitTask

            match Option.ofObj maybeCompletionResults with
            | Some completionResults ->
                let makeLspCompletionItem (item: Microsoft.CodeAnalysis.Completion.CompletionItem) =
                    let baseCompletionItem = Types.CompletionItem.Create(item.DisplayText)

                    { baseCompletionItem with
                        Kind             = item.Tags |> Seq.tryHead |> Option.map roslynTagToLspCompletion
                        SortText         = item.SortText |> Option.ofObj
                        FilterText       = item.FilterText |> Option.ofObj
                        InsertTextFormat = Some InsertTextFormat.PlainText
                    }

                let completionItems =
                    completionResults.ItemsList
                    |> Seq.map makeLspCompletionItem
                    |> Array.ofSeq

                let completionList = {
                    IsIncomplete = false
                    ItemDefaults = None
                    Items        = completionItems
                }

                return LspResult.success (Some completionList)

            | None -> return LspResult.success None

        | None -> return LspResult.success None
    }
