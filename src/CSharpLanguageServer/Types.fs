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

/// TODO: to be moved to Ionide.LanguageServerProtocol
type CallHierarchyRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type ReferenceRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type TypeDefinitionRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type DocumentFormattingRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type TypeHierarchyRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type DefinitionRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type ImplementationRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type DocumentRangeFormattingRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type DocumentHighlightRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type HoverRegistrationOptions =
    { DocumentSelector: DocumentSelector option }

/// TODO: to be moved to Ionide.LanguageServerProtocol
type WorkspaceSymbolRegistrationOptions =
    { ResolveProvider: bool option }

type DocumentFilter with
    static member Default: DocumentFilter =
        { Language = None
          Scheme = Some "file"
          Pattern = Some "**/*.cs" }

// Type abbreviations cannot have augmentations, extensions
let defaultDocumentSelector: DocumentSelector = [| DocumentFilter.Default |]
