namespace CSharpLanguageServer.Handlers

open System
open System.Reflection

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Completion =
    let private logger = LogProvider.getLoggerByName "Completion"

    let emptyRoslynOptionSet: Microsoft.CodeAnalysis.Options.OptionSet =
        let osType = typeof<Microsoft.CodeAnalysis.Options.OptionSet>
        let osEmptyOptionSetField = osType.GetField("Empty", BindingFlags.Static|||BindingFlags.NonPublic)
        osEmptyOptionSetField.GetValue(null) :?> Microsoft.CodeAnalysis.Options.OptionSet

    /// the type reflects on internal class Microsoft.CodeAnalysis.Completion.CompletionOptions
    /// see https://github.com/dotnet/roslyn/blob/main/src/Features/Core/Portable/Completion/CompletionOptions.cs
    type RoslynCompletionOptions =
        {
            Object: obj
            CompletionOptionsType: Type
        }
        with
            member rco.WithBool(optionName: string, optionValue: bool) =
                let cloneCompletionOptionsMI = rco.CompletionOptionsType.GetMethod("<Clone>$")
                let updatedCompletionOptions = cloneCompletionOptionsMI.Invoke(rco.Object, null)
                let newCo = rco.CompletionOptionsType.GetProperty(optionName)
                newCo.SetValue(updatedCompletionOptions, optionValue)
                { rco with Object = updatedCompletionOptions }

            static member Default() =
                let featuresAssembly = Assembly.Load("Microsoft.CodeAnalysis.Features")
                let coType = featuresAssembly.GetType("Microsoft.CodeAnalysis.Completion.CompletionOptions")
                let defaultCo: obj = coType.GetField("Default").GetValue()
                { Object = defaultCo; CompletionOptionsType = coType }

    type RoslynCompletionServiceWrapper(service: Microsoft.CodeAnalysis.Completion.CompletionService) =
        member __.GetCompletionsAsync(doc, position, completionOptions, completionTrigger, ct) : Async<Microsoft.CodeAnalysis.Completion.CompletionList> =
            let completionServiceType = service.GetType()

            let getCompletionsAsync7MI =
                completionServiceType.GetMethods(BindingFlags.Instance|||BindingFlags.NonPublic)
                |> Seq.filter (fun mi -> mi.Name = "GetCompletionsAsync" && mi.GetParameters().Length = 7)
                |> Seq.head

            let parameters: obj array = [| doc; position; completionOptions.Object; emptyRoslynOptionSet; completionTrigger; null; ct |]

            let result = getCompletionsAsync7MI.Invoke(service, parameters)

            (result :?> System.Threading.Tasks.Task<Microsoft.CodeAnalysis.Completion.CompletionList>)
            |> Async.AwaitTask

        member __.ShouldTriggerCompletion(sourceText, position, completionTrigger) =
            service.ShouldTriggerCompletion(sourceText, position, completionTrigger)

    let private dynamicRegistration (clientCapabilities: ClientCapabilities) =
        clientCapabilities.TextDocument
        |> Option.bind (fun x -> x.Completion)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities) : CompletionOptions option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false ->
            Some { ResolveProvider = None
                   TriggerCharacters = Some ([| "."; "'"; |])
                   AllCommitCharacters = None
                   WorkDoneProgress = None
                   CompletionItem = None }

    let registration (clientCapabilities: ClientCapabilities) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            let registerOptions: CompletionRegistrationOptions =
                    { DocumentSelector = Some defaultDocumentSelector
                      ResolveProvider = Some true
                      TriggerCharacters = Some ([| "."; "'"; |])
                      AllCommitCharacters = None
                      CompletionItem = None
                      WorkDoneProgress = None
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
        { Ionide.LanguageServerProtocol.Types.CompletionItem.Create(item.DisplayText) with
            Kind             = item.Tags |> Seq.tryHead |> Option.map roslynTagToLspCompletion
            SortText         = item.SortText |> Option.ofString
            FilterText       = item.FilterText |> Option.ofString
            Detail           = item.InlineDescription |> Option.ofString
            TextEditText     = item.DisplayTextPrefix |> Option.ofObj
            InsertTextFormat = Some InsertTextFormat.PlainText
            Data             = cacheKey |> serialize |> Some }

    let private cache = new LruCache<(Microsoft.CodeAnalysis.Document * Microsoft.CodeAnalysis.Completion.CompletionList)>(5)

    let handle (context: ServerRequestContext) (p: CompletionParams) : AsyncLspResult<Ionide.LanguageServerProtocol.Types.CompletionList option> = async {
        match context.GetDocument p.TextDocument.Uri with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let position = Position.toRoslynPosition sourceText.Lines p.Position

            let completionService =
                Microsoft.CodeAnalysis.Completion.CompletionService.GetService(doc)
                |> RoslynCompletionServiceWrapper

            let completionOptions =
                RoslynCompletionOptions.Default()
                |> _.WithBool("ShowItemsFromUnimportedNamespaces", false)
                |> _.WithBool("ShowNameSuggestions", false)

            let completionTrigger = CompletionContext.toCompletionTrigger p.Context
            let shouldTriggerCompletion =
                p.Context |> Option.exists (fun x -> x.TriggerKind = CompletionTriggerKind.TriggerForIncompleteCompletions) ||
                completionService.ShouldTriggerCompletion(sourceText, position, completionTrigger)

            let! completions =
                if shouldTriggerCompletion then
                    completionService.GetCompletionsAsync(doc, position, completionOptions, completionTrigger, ct)
                    |> Async.map Option.ofObj
                else
                    async.Return Option.None

            return
                completions
                |> Option.map (fun completions ->
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

    let resolve (_context: ServerRequestContext) (item: CompletionItem) : AsyncLspResult<CompletionItem> = async {
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
                |> Async.AwaitTask
                |> Async.map Option.ofObj
            // TODO: make the doc as a markdown string instead of a plain text
            let itemDocumentation = description |> Option.map Documentation.fromCompletionDescription
            return { item with Documentation = itemDocumentation |> Option.map U2.C2 } |> success
    }
