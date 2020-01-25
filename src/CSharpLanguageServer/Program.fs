module CSharpLanguageServer.Program

open System
open System.IO
open System.Threading
open LSP
open LSP.Types
open LSP.Log

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

type Server(client: ILanguageClient) =
    let mutable deferredInitialize = async { () }

    let mutable currentFileSyntax = null

    let todo() = raise (Exception "TODO")

    interface ILanguageServer with
        member _.Initialize(_: InitializeParams) =
            async {
                return {
                   capabilities =
                       { defaultServerCapabilities with
                             hoverProvider = true
                             completionProvider = None
                             signatureHelpProvider = None
                             documentSymbolProvider = false
                             codeLensProvider = None
                             workspaceSymbolProvider = false
                             definitionProvider = false
                             referencesProvider = false
                             renameProvider = false
                             textDocumentSync = {
                                defaultTextDocumentSyncOptions with
                                      openClose = false
                                      save = Some({ includeText = false })
                                      change = TextDocumentSyncKind.Full
                             }
                         }
                   }
                }

        member __.Initialized(): Async<unit> =
            deferredInitialize

        member __.Shutdown(): Async<unit> = todo()
        member __.DidChangeConfiguration(_: DidChangeConfigurationParams): Async<unit> = todo()
        member __.DidOpenTextDocument(p: DidOpenTextDocumentParams): Async<unit> =
            async {
                let tree = CSharpSyntaxTree.ParseText(p.textDocument.text)
                let syntax = tree.GetCompilationUnitRoot(CancellationToken.None)
                currentFileSyntax <- syntax
                ()
            }

        member __.DidChangeTextDocument(_: DidChangeTextDocumentParams): Async<unit> = todo()
        member __.WillSaveTextDocument(_: WillSaveTextDocumentParams): Async<unit> = todo()
        member __.WillSaveWaitUntilTextDocument(_: WillSaveTextDocumentParams): Async<TextEdit list> = todo()
        member __.DidSaveTextDocument(_: DidSaveTextDocumentParams): Async<unit> = todo()
        member __.DidCloseTextDocument(_: DidCloseTextDocumentParams): Async<unit> = todo()
        member __.DidChangeWatchedFiles(_: DidChangeWatchedFilesParams): Async<unit> = todo()
        member __.Completion(_: TextDocumentPositionParams): Async<CompletionList option> = todo()

        member __.Hover(_: TextDocumentPositionParams): Async<Hover option> =
            async {
                return match currentFileSyntax with
                       | null -> None
                       | _ ->
                           let contents = [ HighlightedString("item description", "fsharp") ;
                                            PlainString("hey") ]

                           Some({ contents=contents; range=None })
            }

        member __.ResolveCompletionItem(_: CompletionItem): Async<CompletionItem> = todo()
        member __.SignatureHelp(_: TextDocumentPositionParams): Async<SignatureHelp option> = todo()
        member __.GotoDefinition(_: TextDocumentPositionParams): Async<LSP.Types.Location list> = todo()
        member __.FindReferences(_: ReferenceParams): Async<LSP.Types.Location list> = todo()
        member __.DocumentHighlight(_: TextDocumentPositionParams): Async<DocumentHighlight list> = todo()
        member __.DocumentSymbols(_: DocumentSymbolParams): Async<SymbolInformation list> = todo()
        member __.WorkspaceSymbols(_: WorkspaceSymbolParams): Async<SymbolInformation list> = todo()
        member __.CodeActions(_: CodeActionParams): Async<Command list> = todo()
        member __.CodeLens(_: CodeLensParams): Async<List<CodeLens>> = todo()
        member __.ResolveCodeLens(_: CodeLens): Async<CodeLens> = todo()
        member __.DocumentLink(_: DocumentLinkParams): Async<DocumentLink list> = todo()
        member __.ResolveDocumentLink(_: DocumentLink): Async<DocumentLink> = todo()
        member __.DocumentFormatting(_: DocumentFormattingParams): Async<TextEdit list> = todo()
        member __.DocumentRangeFormatting(_: DocumentRangeFormattingParams): Async<TextEdit list> = todo()
        member __.DocumentOnTypeFormatting(_: DocumentOnTypeFormattingParams): Async<TextEdit list> = todo()
        member __.Rename(_: RenameParams): Async<WorkspaceEdit> = todo()
        member __.ExecuteCommand(_: ExecuteCommandParams): Async<unit> = todo()
        member __.DidChangeWorkspaceFolders(_: DidChangeWorkspaceFoldersParams): Async<unit> = todo()

[<EntryPoint>]
let main(_: string array): int =
    let read = new BinaryReader(Console.OpenStandardInput())
    let write = new BinaryWriter(Console.OpenStandardOutput())
    let serverFactory(client) = Server(client) :> ILanguageServer
    dprintfn "Listening on stdin"
    try
        LanguageServer.connect(serverFactory, read, write)
        0 // return an integer exit code
    with e ->
        dprintfn "Exception in language server %O" e
        1
