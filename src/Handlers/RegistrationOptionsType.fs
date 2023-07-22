namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types


type TextDocumentRegistrationOptions = { DocumentSelector: DocumentSelector option }

type CodeActionRegistrationOptions =
    { CodeActionKinds: CodeActionKind [] option
      ResolveProvider: bool option
      DocumentSelector: DocumentSelector option }

type CodeLensRegistrationOptions =
    { ResolveProvider: bool option
      DocumentSelector: DocumentSelector option}

type CompletionRegistrationOptions =
    { TriggerCharacters: char [] option
      AllCommitCharacters: char [] option
      ResolveProvider: bool option
      CompletionItem: CompletionItemOptions option
      DocumentSelector: DocumentSelector option }

type DocumentOnTypeFormattingRegistrationOptions =
    { FirstTriggerCharacter: char
      MoreTriggerCharacter: char [] option
      DocumentSelector: DocumentSelector option }

type DocumentSymbolRegistrationOptions =
    { Label: string option
      DocumentSelector: DocumentSelector option }

type InlayHintRegistrationOptions =
    { ResolveProvider: bool option
      DocumentSelector: DocumentSelector option }

type RenameRegistrationOptions =
    { PrepareProvider: bool option
      DocumentSelector: DocumentSelector option }

type SemanticTokensRegistrationOptions =
    { Legend: SemanticTokensLegend
      Range: bool option
      Full: U2<bool, SemanticTokenFullOptions> option
      DocumentSelector: DocumentSelector option }

type SignatureHelpRegistrationOptions =
    { TriggerCharacters: char [] option
      RetriggerCharacters: char [] option
      DocumentSelector: DocumentSelector option }
