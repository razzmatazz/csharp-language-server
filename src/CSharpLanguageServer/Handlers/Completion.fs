namespace CSharpLanguageServer.Handlers

open System
open System.Reflection

open Microsoft.Extensions.Caching.Memory
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Lsp.Workspace

[<RequireQualifiedAccess>]
module Completion =
    let private _logger = Logging.getLoggerByName "Completion"

    let private completionItemMemoryCache = new MemoryCache(new MemoryCacheOptions())

    let private completionItemMemoryCacheSet (cacheItemId: string) roslynDoc roslynCompletionItem =
        completionItemMemoryCache.Set<Microsoft.CodeAnalysis.Document * Microsoft.CodeAnalysis.Completion.CompletionItem>(
            key = cacheItemId,
            value = (roslynDoc, roslynCompletionItem),
            absoluteExpiration = DateTimeOffset.Now.AddMinutes(5)
        )
        |> ignore

    let private completionItemMemoryCacheGet
        (cacheItemId: string)
        : option<Microsoft.CodeAnalysis.Document * Microsoft.CodeAnalysis.Completion.CompletionItem> =
        let mutable value = Unchecked.defaultof<obj>

        if completionItemMemoryCache.TryGetValue(cacheItemId, &value) then
            Some(value :?> Microsoft.CodeAnalysis.Document * Microsoft.CodeAnalysis.Completion.CompletionItem)
        else
            None

    let emptyRoslynOptionSet: Microsoft.CodeAnalysis.Options.OptionSet =
        let osEmptyOptionSetField =
            typeof<Microsoft.CodeAnalysis.Options.OptionSet>
            |> _.GetField("Empty", BindingFlags.Static ||| BindingFlags.NonPublic)
            |> nonNull "Microsoft.CodeAnalysis.Options.OptionSet.Empty"

        osEmptyOptionSetField.GetValue(null)
        |> nonNull "Microsoft.CodeAnalysis.Options.OptionSet.Empty"
        :?> Microsoft.CodeAnalysis.Options.OptionSet

    /// the type reflects on internal class Microsoft.CodeAnalysis.Completion.CompletionOptions
    /// see https://github.com/dotnet/roslyn/blob/main/src/Features/Core/Portable/Completion/CompletionOptions.cs
    type RoslynCompletionOptions =
        { Object: obj
          CompletionOptionsType: Type }

        member rco.WithBool(optionName: string, optionValue: bool) =
            let cloneCompletionOptionsMI =
                rco.CompletionOptionsType.GetMethod("<Clone>$")
                |> nonNull "rco.CompletionOptionsType.GetMethod('<Clone>$')"

            let updatedCompletionOptions = cloneCompletionOptionsMI.Invoke(rco.Object, null)

            let newCo =
                rco.CompletionOptionsType.GetProperty(optionName)
                |> nonNull (sprintf "rco.CompletionOptionsType.GetProperty('%s')" optionName)

            newCo.SetValue(updatedCompletionOptions, optionValue)

            { rco with
                Object = updatedCompletionOptions }

        static member Default() =
            let featuresAssembly = Assembly.Load("Microsoft.CodeAnalysis.Features")

            let coType =
                featuresAssembly.GetType("Microsoft.CodeAnalysis.Completion.CompletionOptions")
                |> nonNull "GetType('Microsoft.CodeAnalysis.Completion.CompletionOptions')"

            let defaultCo: obj =
                coType.GetField("Default")
                |> nonNull "Microsoft.CodeAnalysis.Completion.CompletionOptions.Default"
                |> _.GetValue(null)

            { Object = defaultCo
              CompletionOptionsType = coType }

    type RoslynCompletionServiceWrapper(service: Microsoft.CodeAnalysis.Completion.CompletionService) =
        member __.GetCompletionsAsync
            (doc, position, completionOptions, completionTrigger, ct)
            : Async<Microsoft.CodeAnalysis.Completion.CompletionList> =
            let completionServiceType = service.GetType()

            let getCompletionsAsync7MI =
                completionServiceType.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic)
                |> Seq.filter (fun mi -> mi.Name = "GetCompletionsAsync" && mi.GetParameters().Length = 7)
                |> Seq.head

            let parameters: obj array =
                [| doc
                   position
                   completionOptions.Object
                   emptyRoslynOptionSet
                   completionTrigger
                   null
                   ct |]

            let result =
                getCompletionsAsync7MI.Invoke(service, parameters)
                |> nonNull "result of getCompletionsAsync7MI"

            (result :?> System.Threading.Tasks.Task<Microsoft.CodeAnalysis.Completion.CompletionList>)
            |> Async.AwaitTask

        member __.ShouldTriggerCompletion(sourceText, position, completionTrigger) =
            service.ShouldTriggerCompletion(sourceText, position, completionTrigger)

        member __.GetDescriptionAsync(doc, item, ct) =
            service.GetDescriptionAsync(doc, item, ct)

    let provider (_cc: ClientCapabilities) : CompletionOptions option =
        Some
            { ResolveProvider = Some true
              TriggerCharacters = Some([| "."; "'" |])
              AllCommitCharacters = None
              CompletionItem = None
              WorkDoneProgress = None }

    let private roslynTagToLspCompletion tag =
        match tag with
        | "Class" -> CompletionItemKind.Class
        | "Delegate" -> CompletionItemKind.Function
        | "Enum" -> CompletionItemKind.Enum
        | "EnumMember" -> CompletionItemKind.EnumMember
        | "Interface" -> CompletionItemKind.Interface
        | "Structure" -> CompletionItemKind.Struct
        | "Local" -> CompletionItemKind.Variable
        | "Parameter" -> CompletionItemKind.Variable
        | "RangeVariable" -> CompletionItemKind.Variable
        | "Constant" -> CompletionItemKind.Constant
        | "Event" -> CompletionItemKind.Event
        | "Field" -> CompletionItemKind.Field
        | "Method" -> CompletionItemKind.Method
        | "Property" -> CompletionItemKind.Property
        | "Label" -> CompletionItemKind.Unit
        | "Keyword" -> CompletionItemKind.Keyword
        | "Namespace" -> CompletionItemKind.Module
        | "ExtensionMethod" -> CompletionItemKind.Method
        | "Assembly" -> CompletionItemKind.Module
        | "TypeParameter" -> CompletionItemKind.TypeParameter
        | _ -> CompletionItemKind.Property

    let optionOfString (value: string) =
        match String.IsNullOrWhiteSpace value with
        | true -> None
        | false -> Some value

    let parseAndFormatDocumentation
        (completionDescription: Microsoft.CodeAnalysis.Completion.CompletionDescription)
        : (string option * string option) =
        let codeBlockStartIndex =
            completionDescription.TaggedParts
            |> Seq.tryFindIndex (fun t -> t.Tag = "CodeBlockStart")

        let codeBlockEndIndex =
            completionDescription.TaggedParts
            |> Seq.tryFindIndex (fun t -> t.Tag = "CodeBlockEnd")

        match codeBlockStartIndex, codeBlockEndIndex with
        | Some 0, Some codeBlockEndIndex ->
            let synopsis =
                completionDescription.TaggedParts
                |> Seq.take codeBlockEndIndex
                |> Seq.map _.Text
                |> String.concat ""
                |> Some

            let documentationText =
                completionDescription.TaggedParts
                |> Seq.skip (codeBlockEndIndex + 1)
                |> Seq.skipWhile (fun t -> t.Tag = "LineBreak")
                |> Seq.map _.Text
                |> String.concat ""
                |> optionOfString

            synopsis, documentationText
        | _, _ -> None, None

    let handle
        (context: ServerRequestContext)
        (p: CompletionParams)
        : Async<LspResult<U2<CompletionItem array, CompletionList> option>> =
        async {
            let wf, docForUri =
                p.TextDocument.Uri |> workspaceDocument context.Workspace AnyDocument

            match docForUri with
            | None -> return None |> LspResult.success
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

                let completionTrigger =
                    p.Context
                    |> Option.bind (fun ctx ->
                        match ctx.TriggerKind with
                        | CompletionTriggerKind.Invoked
                        | CompletionTriggerKind.TriggerForIncompleteCompletions ->
                            Some Microsoft.CodeAnalysis.Completion.CompletionTrigger.Invoke
                        | CompletionTriggerKind.TriggerCharacter ->
                            ctx.TriggerCharacter
                            |> Option.map Seq.head
                            |> Option.map Microsoft.CodeAnalysis.Completion.CompletionTrigger.CreateInsertionTrigger
                        | _ -> None)
                    |> Option.defaultValue Microsoft.CodeAnalysis.Completion.CompletionTrigger.Invoke

                let shouldTriggerCompletion =
                    p.Context
                    |> Option.exists (fun x -> x.TriggerKind = CompletionTriggerKind.TriggerForIncompleteCompletions)
                    || completionService.ShouldTriggerCompletion(sourceText, position, completionTrigger)

                let! roslynCompletions =
                    if shouldTriggerCompletion then
                        completionService.GetCompletionsAsync(doc, position, completionOptions, completionTrigger, ct)
                        |> Async.map Option.ofObj
                    else
                        async.Return None

                let toLspCompletionItemsWithCacheInfo (completions: Microsoft.CodeAnalysis.Completion.CompletionList) =
                    completions.ItemsList
                    |> Seq.map (fun item -> (item, Guid.NewGuid() |> string))
                    |> Seq.map (fun (item, cacheItemId) ->
                        let lspCompletionItem =
                            { Ionide.LanguageServerProtocol.Types.CompletionItem.Create item.DisplayText with
                                Kind = item.Tags |> Seq.tryHead |> Option.map roslynTagToLspCompletion
                                SortText = item.SortText |> optionOfString
                                FilterText = item.FilterText |> optionOfString
                                InsertText = item.DisplayText |> optionOfString
                                Data = cacheItemId |> serialize |> Some }

                        (lspCompletionItem, cacheItemId, doc, item))
                    |> Array.ofSeq

                let lspCompletionItemsWithCacheInfo =
                    roslynCompletions |> Option.map toLspCompletionItemsWithCacheInfo

                // cache roslyn completion items
                for (_, cacheItemId, roslynDoc, roslynItem) in
                    (lspCompletionItemsWithCacheInfo |> Option.defaultValue Array.empty) do
                    completionItemMemoryCacheSet cacheItemId roslynDoc roslynItem

                return
                    lspCompletionItemsWithCacheInfo
                    |> Option.map (fun itemsWithCacheInfo ->
                        itemsWithCacheInfo |> Array.map (fun (item, _, _, _) -> item))
                    |> Option.map (fun items ->
                        { IsIncomplete = true
                          Items = items
                          ItemDefaults = None })
                    |> Option.map U2.C2
                    |> LspResult.success
        }

    let resolve (_context: ServerRequestContext) (item: CompletionItem) : AsyncLspResult<CompletionItem> = async {
        let roslynDocAndItemMaybe =
            item.Data
            |> Option.bind deserialize<string option>
            |> Option.bind completionItemMemoryCacheGet

        match roslynDocAndItemMaybe with
        | Some(doc, roslynCompletionItem) ->
            let completionService =
                Microsoft.CodeAnalysis.Completion.CompletionService.GetService(doc)
                |> nonNull "Microsoft.CodeAnalysis.Completion.CompletionService.GetService(doc)"

            let! ct = Async.CancellationToken

            let! description =
                completionService.GetDescriptionAsync(doc, roslynCompletionItem, ct)
                |> Async.AwaitTask
                |> Async.map Option.ofObj

            let synopsis, documentation =
                description
                |> Option.map parseAndFormatDocumentation
                |> Option.defaultValue (None, None)

            let updatedItemDocumentation =
                documentation
                |> Option.map (fun d ->
                    { Kind = MarkupKind.PlainText
                      Value = d }
                    |> U2.C2)

            return
                { item with
                    Detail = synopsis
                    Documentation = updatedItemDocumentation }
                |> LspResult.success

        | None -> return item |> LspResult.success
    }
