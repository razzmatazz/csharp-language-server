module CSharpLanguageServer.Types

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

type ServerSettings =
    { SolutionPath: string option
      LogLevel: LogLevel
      ApplyFormattingOptions: bool
      UseMetadataUris: bool
      DebugMode: bool }

    member this.GetEffectiveFormattingOptions options =
        if this.ApplyFormattingOptions then Some options else None

    static member Default: ServerSettings =
        { SolutionPath = None
          LogLevel = LogLevel.Information
          ApplyFormattingOptions = false
          UseMetadataUris = false
          DebugMode = false }

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

let csharpDocumentFilter: TextDocumentFilter =
    { Language = Some "csharp"
      Scheme = Some "file"
      Pattern = Some "**/*.cs" }

let razorCsharpDocumentFilter: TextDocumentFilter =
    { Language = Some "razor"
      Scheme = Some "file"
      Pattern = Some "**/*.cshtml" }

let defaultDocumentSelector: DocumentSelector = [| csharpDocumentFilter |> U2.C1 |]

let emptyClientCapabilities: ClientCapabilities =
    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = None
      General = None
      Experimental = None }
