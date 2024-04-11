module CSharpLanguageServer.Types

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

type ServerSettings =
    { SolutionPath: string option
      LogLevel: string }

    static member Default: ServerSettings =
        { SolutionPath = None
          LogLevel = "log" }

type CSharpMetadataInformation =
    { ProjectName: string
      AssemblyName: string
      SymbolName: string
      Source: string }

type CSharpMetadataParams =
    { TextDocument: TextDocumentIdentifier }

type CSharpMetadataResponse = CSharpMetadataInformation

[<Interface>]
type ICSharpLspServer =
    inherit ILspServer
    abstract CSharpMetadata: CSharpMetadataParams -> AsyncLspResult<CSharpMetadataResponse option>

[<Interface>]
type ICSharpLspClient =
    inherit ILspClient
    // Use a ClientCapabilitiesDTO instead of ClientCapabilities to avoid Option.map & Option.bind?
    // But ClientCapabilities is a complex type, write it again will be a huge work.
    abstract member Capabilities: ClientCapabilities option with get, set

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
      // CompletionItem: CompletionItemOption option
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

type DocumentFilter with
    static member Default: DocumentFilter =
        { Language = None
          Scheme = Some "file"
          Pattern = Some "**/*.cs" }

// Type abbreviations cannot have augmentations, extensions
let defaultDocumentSelector: DocumentSelector = [| DocumentFilter.Default |]
