namespace Ionide.LanguageServerProtocol.Types

[<AutoOpen>]
module Extensions =

  type SymbolKindCapabilities =

    static member DefaultValueSet = [|
      SymbolKind.File
      SymbolKind.Module
      SymbolKind.Namespace
      SymbolKind.Package
      SymbolKind.Class
      SymbolKind.Method
      SymbolKind.Property
      SymbolKind.Field
      SymbolKind.Constructor
      SymbolKind.Enum
      SymbolKind.Interface
      SymbolKind.Function
      SymbolKind.Variable
      SymbolKind.Constant
      SymbolKind.String
      SymbolKind.Number
      SymbolKind.Boolean
      SymbolKind.Array
    |]

  type CompletionItemKindCapabilities =
    static member DefaultValueSet = [|
      CompletionItemKind.Text
      CompletionItemKind.Method
      CompletionItemKind.Function
      CompletionItemKind.Constructor
      CompletionItemKind.Field
      CompletionItemKind.Variable
      CompletionItemKind.Class
      CompletionItemKind.Interface
      CompletionItemKind.Module
      CompletionItemKind.Property
      CompletionItemKind.Unit
      CompletionItemKind.Value
      CompletionItemKind.Enum
      CompletionItemKind.Keyword
      CompletionItemKind.Snippet
      CompletionItemKind.Color
      CompletionItemKind.File
      CompletionItemKind.Reference
    |]

  type TextDocumentSyncOptions with
    static member Default = {
      OpenClose = None
      Change = None
      WillSave = None
      WillSaveWaitUntil = None
      Save = None
    }

  type WorkspaceFoldersServerCapabilities with
    static member Default = { Supported = None; ChangeNotifications = None }

  type FileOperationPatternOptions with
    static member Default = { IgnoreCase = None }

  type FileOperationOptions with
    static member Default = {
      DidCreate = None
      WillCreate = None
      DidRename = None
      WillRename = None
      DidDelete = None
      WillDelete = None
    }


  type WorkspaceServerCapabilities =
    static member Default = {| WorkspaceFolders = None; FileOperations = None |}


  type ServerCapabilities with
    static member Default = {
      TextDocumentSync = None
      HoverProvider = None
      CompletionProvider = None
      SignatureHelpProvider = None
      DefinitionProvider = None
      TypeDefinitionProvider = None
      ImplementationProvider = None
      ReferencesProvider = None
      DocumentHighlightProvider = None
      DocumentSymbolProvider = None
      WorkspaceSymbolProvider = None
      CodeActionProvider = None
      CodeLensProvider = None
      DocumentFormattingProvider = None
      DocumentRangeFormattingProvider = None
      DocumentOnTypeFormattingProvider = None
      RenameProvider = None
      DocumentLinkProvider = None
      ExecuteCommandProvider = None
      Workspace = None
      Experimental = None
      PositionEncoding = None
      NotebookDocumentSync = None
      DeclarationProvider = None
      ColorProvider = None
      FoldingRangeProvider = None
      SelectionRangeProvider = None
      CallHierarchyProvider = None
      LinkedEditingRangeProvider = None
      SemanticTokensProvider = None
      MonikerProvider = None
      TypeHierarchyProvider = None
      InlineValueProvider = None
      InlayHintProvider = None
      DiagnosticProvider = None
    }


  type InitializeResult with
    static member Default = { Capabilities = ServerCapabilities.Default; ServerInfo = None }

  type CompletionItem with
    static member Create(label: string) = {
      Label = label
      LabelDetails = None
      Kind = None
      Tags = None
      Detail = None
      Documentation = None
      Deprecated = None
      Preselect = None
      SortText = None
      FilterText = None
      InsertText = None
      InsertTextFormat = None
      InsertTextMode = None
      TextEdit = None
      TextEditText = None
      AdditionalTextEdits = None
      CommitCharacters = None
      Command = None
      Data = None
    }

  type WorkDoneProgressKind =
    | Begin
    | Report
    | End

    override x.ToString() =
      match x with
      | Begin -> "begin"
      | Report -> "report"
      | End -> "end"

  type WorkDoneProgressEnd with
    static member Create(?message) = { Kind = WorkDoneProgressKind.End.ToString(); Message = message }

  type WorkDoneProgressBegin with
    static member Create(title, ?cancellable, ?message, ?percentage) = {
      Kind = WorkDoneProgressKind.Begin.ToString()
      Title = title
      Cancellable = cancellable
      Message = message
      Percentage = percentage
    }

  type WorkDoneProgressReport with
    static member Create(?cancellable, ?message, ?percentage) = {
      Kind = WorkDoneProgressKind.Report.ToString()
      Cancellable = cancellable
      Message = message
      Percentage = percentage
    }


  type WorkspaceEdit with
    static member DocumentChangesToChanges(edits: TextDocumentEdit[]) =
      edits
      |> Array.map (fun edit ->
        let edits =
          edit.Edits
          |> Array.choose (
            function
            | U2.C1 x -> Some x
            | _ -> None
          )

        edit.TextDocument.Uri.ToString(), edits
      )
      |> Map.ofArray

    static member CanUseDocumentChanges(capabilities: ClientCapabilities) =
      (capabilities.Workspace
       |> Option.bind (fun x -> x.WorkspaceEdit)
       |> Option.bind (fun x -> x.DocumentChanges)) = Some true

    static member Create(edits: TextDocumentEdit[], capabilities: ClientCapabilities) =
      if WorkspaceEdit.CanUseDocumentChanges(capabilities) then
        let edits =
          edits
          |> Array.map U4.C1

        { Changes = None; DocumentChanges = Some edits; ChangeAnnotations = None }
      else
        {
          Changes = Some(WorkspaceEdit.DocumentChangesToChanges edits)
          DocumentChanges = None
          ChangeAnnotations = None
        }

  type TextDocumentCodeActionResult = U2<Command, CodeAction>[]