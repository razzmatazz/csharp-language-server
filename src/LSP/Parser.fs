module LSP.Parser

open LSP.Log
open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open LSP.Json.Ser
open Types

type RawMessage = {
    id: int option 
    method: string 
    ``params``: JsonValue option
}

let parseTextDocumentSaveReason(i: int): TextDocumentSaveReason = 
    match i with 
    | 1 -> TextDocumentSaveReason.Manual 
    | 2 -> TextDocumentSaveReason.AfterDelay 
    | 3 -> TextDocumentSaveReason.FocusOut
    | _ -> raise(Exception(sprintf "%d is not a known TextDocumentSaveReason" i))

let parseFileChangeType(i: int): FileChangeType = 
    match i with 
    | 1 -> FileChangeType.Created 
    | 2 -> FileChangeType.Changed 
    | 3 -> FileChangeType.Deleted
    | _ -> raise(Exception(sprintf "%d is not a known FileChangeType" i))

let parseTrace(text: string): Trace = 
    match text with 
    | "off" -> Trace.Off 
    | "messages" -> Trace.Messages 
    | "verbose" -> Trace.Verbose
    | _ -> raise(Exception(sprintf "Unexpected trace %s" text))

let parseCompletionItemKind(i: int): CompletionItemKind = 
    match i with 
    | 1 -> CompletionItemKind.Text
    | 2 -> CompletionItemKind.Method
    | 3 -> CompletionItemKind.Function
    | 4 -> CompletionItemKind.Constructor
    | 5 -> CompletionItemKind.Field
    | 6 -> CompletionItemKind.Variable
    | 7 -> CompletionItemKind.Class
    | 8 -> CompletionItemKind.Interface
    | 9 -> CompletionItemKind.Module
    | 10 -> CompletionItemKind.Property
    | 11 -> CompletionItemKind.Unit
    | 12 -> CompletionItemKind.Value
    | 13 -> CompletionItemKind.Enum
    | 14 -> CompletionItemKind.Keyword
    | 15 -> CompletionItemKind.Snippet
    | 16 -> CompletionItemKind.Color
    | 17 -> CompletionItemKind.File
    | 18 -> CompletionItemKind.Reference
    | 19 -> CompletionItemKind.Folder
    | 20 -> CompletionItemKind.EnumMember
    | 21 -> CompletionItemKind.Constant
    | 22 -> CompletionItemKind.Struct
    | 23 -> CompletionItemKind.Event
    | 24 -> CompletionItemKind.Operator
    | 25 -> CompletionItemKind.TypeParameter
    | _ -> raise(Exception(sprintf "%d is not a known CompletionItemKind" i))

let parseInsertTextFormat(i: int): InsertTextFormat = 
    match i with 
    | 1 -> InsertTextFormat.PlainText
    | 2 -> InsertTextFormat.Snippet
    | _ -> raise(Exception(sprintf "%d is not a known InsertTextFormat" i))

let parseDiagnosticSeverity(i: int): DiagnosticSeverity = 
    match i with 
    | 1 -> DiagnosticSeverity.Error 
    | 2 -> DiagnosticSeverity.Warning 
    | 3 -> DiagnosticSeverity.Information 
    | 4 -> DiagnosticSeverity.Hint
    | _ -> raise(Exception(sprintf "%d is not a known DiagnosticSeverity" i))

let parseMarkupKind(s: string): MarkupKind = 
    match s with 
    | "plaintext" -> MarkupKind.PlainText
    | "markdown" -> MarkupKind.Markdown 
    | _ -> raise(Exception(sprintf "%s is not a known MarkupKind" s))

let private readOptions = 
    { defaultJsonReadOptions 
        with customReaders = [  parseTextDocumentSaveReason
                                parseFileChangeType
                                parseTrace
                                parseCompletionItemKind
                                parseInsertTextFormat
                                parseDiagnosticSeverity
                                parseMarkupKind ] }

let private deserializeRawMessage = JsonValue.Parse >> deserializerFactory<RawMessage> readOptions

type Message = 
| RequestMessage of id: int * method: string * json: JsonValue
| NotificationMessage of method: string * json: JsonValue option

let parseMessage(jsonText: string): Message = 
    let raw = deserializeRawMessage jsonText
    match raw.id, raw.``params`` with
    | Some id, Some p -> RequestMessage (id, raw.method, p)
    | Some id, None -> RequestMessage (id, raw.method, JsonValue.Null)
    | None, _ -> NotificationMessage (raw.method, raw.``params``)

let parseDidChangeConfigurationParams = deserializerFactory<DidChangeConfigurationParams> readOptions

let parseDidOpenTextDocumentParams = deserializerFactory<DidOpenTextDocumentParams> readOptions

let parseDidChangeTextDocumentParams = deserializerFactory<DidChangeTextDocumentParams> readOptions

let parseWillSaveTextDocumentParams = deserializerFactory<WillSaveTextDocumentParams> readOptions

let parseDidSaveTextDocumentParams = deserializerFactory<DidSaveTextDocumentParams> readOptions

let parseDidCloseTextDocumentParams = deserializerFactory<DidCloseTextDocumentParams> readOptions

let parseDidChangeWatchedFilesParams = deserializerFactory<DidChangeWatchedFilesParams> readOptions

let parseNotification(method: string, maybeBody: JsonValue option): Notification = 
    match method, maybeBody with 
    | "initialized", _ -> Initialized
    | "exit", _ -> raise(Exception"exit message should terminated stream before reaching this point") 
    | "workspace/didChangeConfiguration", Some json -> DidChangeConfiguration (parseDidChangeConfigurationParams json)
    | "textDocument/didOpen", Some json -> DidOpenTextDocument (parseDidOpenTextDocumentParams json)
    | "textDocument/didChange", Some json -> DidChangeTextDocument (parseDidChangeTextDocumentParams json)
    | "textDocument/willSave", Some json -> WillSaveTextDocument (parseWillSaveTextDocumentParams json)
    | "textDocument/didSave", Some json -> DidSaveTextDocument (parseDidSaveTextDocumentParams json)
    | "textDocument/didClose", Some json -> DidCloseTextDocument (parseDidCloseTextDocumentParams json)
    | "workspace/didChangeWatchedFiles", Some json -> DidChangeWatchedFiles (parseDidChangeWatchedFilesParams json)
    | _, None -> 
        dprintfn "%s is not a known notification, or it is expected to contain a body" method
        OtherNotification method
    | _, _ -> 
        dprintfn "%s is not a known notification" method
        OtherNotification method

type InitializeParamsRaw = {
    processId: int option
    rootUri: Uri option
    initializationOptions: JsonValue option
    capabilities: JsonValue
    trace: Trace option
}

let private parseCapabilities(nested: JsonValue): Map<string, bool> =
    let rec flatten(path: string, node: JsonValue) = 
        seq {
            for (key, value) in node.Properties do 
                let newPath = path + "." + key
                match value with 
                | JsonValue.Boolean setting -> yield (newPath, setting)
                | _ -> yield! flatten(newPath, value)
        } 
    let kvs = seq {
        for (key, value) in nested.Properties do 
            if key <> "experimental" then 
                yield! flatten(key, value)
    }
    Map.ofSeq kvs

let private parseInitializeParams(raw: InitializeParamsRaw): InitializeParams = 
    { 
         processId = raw.processId
         rootUri = raw.rootUri
         initializationOptions = raw.initializationOptions
         capabilitiesMap = raw.capabilities |> parseCapabilities 
         trace = raw.trace
    }

let parseInitialize = deserializerFactory<InitializeParamsRaw> readOptions >> parseInitializeParams

let parseTextDocumentPositionParams = deserializerFactory<TextDocumentPositionParams> readOptions

let parseCompletionItem = deserializerFactory<CompletionItem> readOptions

let parseReferenceParams = deserializerFactory<ReferenceParams> readOptions

let parseDocumentSymbolParams = deserializerFactory<DocumentSymbolParams> readOptions

let parseWorkspaceSymbolParams = deserializerFactory<WorkspaceSymbolParams> readOptions

let parseDiagnostic = deserializerFactory<Diagnostic> readOptions

let parseCodeActionParams = deserializerFactory<CodeActionParams> readOptions

let parseCodeLensParams = deserializerFactory<CodeLensParams> readOptions

let parseCodeLens = deserializerFactory<CodeLens> readOptions

let parseDocumentLinkParams = deserializerFactory<DocumentLinkParams> readOptions

let parseDocumentLink = deserializerFactory<DocumentLink> readOptions

type DocumentFormattingParamsRaw = {
    textDocument: TextDocumentIdentifier
    options: JsonValue
}

type DocumentRangeFormattingParamsRaw = {
    textDocument: TextDocumentIdentifier
    options: JsonValue
    range: Range
}

type DocumentOnTypeFormattingParamsRaw = {
    textDocument: TextDocumentIdentifier
    options: JsonValue
    position: Position
    ch: char 
}

let private parseDocumentFormattingOptions(options: JsonValue) = 
    {
        tabSize = options?tabSize.AsInteger()
        insertSpaces = options?insertSpaces.AsBoolean()
    }

let private parseDocumentFormattingOptionsMap(options: JsonValue) = 
    options.Properties 
        |> Seq.map (fun (key, value) -> (key, value.AsString()))
        |> Map.ofSeq

let private parseDocumentFormattingParamsRaw(raw: DocumentFormattingParamsRaw): DocumentFormattingParams = 
    {
        textDocument = raw.textDocument 
        options = parseDocumentFormattingOptions raw.options
        optionsMap = parseDocumentFormattingOptionsMap raw.options
    }

let private parseDocumentRangeFormattingParamsRaw(raw: DocumentRangeFormattingParamsRaw): DocumentRangeFormattingParams = 
    {
        textDocument = raw.textDocument 
        options = parseDocumentFormattingOptions raw.options
        optionsMap = parseDocumentFormattingOptionsMap raw.options
        range = raw.range
    }

let private parseDocumentOnTypeFormattingParamsRaw(raw: DocumentOnTypeFormattingParamsRaw): DocumentOnTypeFormattingParams = 
    {
        textDocument = raw.textDocument 
        options = parseDocumentFormattingOptions raw.options
        optionsMap = parseDocumentFormattingOptionsMap raw.options
        position = raw.position
        ch = raw.ch
    }

let parseDocumentFormattingParams = deserializerFactory<DocumentFormattingParamsRaw> readOptions >> parseDocumentFormattingParamsRaw

let parseDocumentRangeFormattingParams = deserializerFactory<DocumentRangeFormattingParamsRaw> readOptions >> parseDocumentRangeFormattingParamsRaw

let parseDocumentOnTypeFormattingParams = deserializerFactory<DocumentOnTypeFormattingParamsRaw> readOptions >> parseDocumentOnTypeFormattingParamsRaw

let parseRenameParams = deserializerFactory<RenameParams> readOptions

let parseExecuteCommandParams = deserializerFactory<ExecuteCommandParams> readOptions

let parseDidChangeWorkspaceFoldersParams = deserializerFactory<DidChangeWorkspaceFoldersParams> readOptions

let parseRequest(method: string, json: JsonValue): Request = 
    match method with 
    | "initialize" -> Initialize(parseInitialize json)
    | "shutdown" -> Shutdown 
    | "textDocument/willSaveWaitUntil" -> WillSaveWaitUntilTextDocument(parseWillSaveTextDocumentParams json)
    | "textDocument/completion" -> Completion(parseTextDocumentPositionParams json)
    | "textDocument/hover" -> Hover(parseTextDocumentPositionParams json)
    | "completionItem/resolve" -> ResolveCompletionItem(parseCompletionItem json)
    | "textDocument/signatureHelp" -> SignatureHelp(parseTextDocumentPositionParams json)
    | "textDocument/definition" -> GotoDefinition(parseTextDocumentPositionParams json)
    | "textDocument/references" -> FindReferences(parseReferenceParams json)
    | "textDocument/documentHighlight" -> DocumentHighlight(parseTextDocumentPositionParams json)
    | "textDocument/documentSymbol" -> DocumentSymbols(parseDocumentSymbolParams json)
    | "workspace/symbol" -> WorkspaceSymbols(parseWorkspaceSymbolParams json)
    | "textDocument/codeAction" -> CodeActions(parseCodeActionParams json)
    | "textDocument/codeLens" -> CodeLens(parseCodeLensParams json)
    | "codeLens/resolve" -> ResolveCodeLens(parseCodeLens json)
    | "textDocument/documentLink" -> DocumentLink(parseDocumentLinkParams json)
    | "documentLink/resolve" -> ResolveDocumentLink(parseDocumentLink json)
    | "textDocument/formatting" -> DocumentFormatting(parseDocumentFormattingParams json)
    | "textDocument/rangeFormatting" -> DocumentRangeFormatting(parseDocumentRangeFormattingParams json)
    | "textDocument/onTypeFormatting" -> DocumentOnTypeFormatting(parseDocumentOnTypeFormattingParams json)
    | "textDocument/rename" -> Rename(parseRenameParams json)
    | "workspace/executeCommand" -> ExecuteCommand(parseExecuteCommandParams json)
    | "workspace/didChangeWorkspaceFolders" -> DidChangeWorkspaceFolders(parseDidChangeWorkspaceFoldersParams json)
    | _ -> raise(Exception(sprintf "Unexpected request method %s" method))
