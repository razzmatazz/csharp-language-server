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
      RazorSupport: bool
      DebugMode: bool }

    member this.GetEffectiveFormattingOptions options =
        if this.ApplyFormattingOptions then Some options else None

    static member Default: ServerSettings =
        { SolutionPath = None
          LogLevel = LogLevel.Information
          ApplyFormattingOptions = false
          UseMetadataUris = false
          RazorSupport = false
          DebugMode = false }

type CSharpSectionConfiguration =
    { solution: string option
      applyFormattingOptions: bool option }

    static member Default =
        { solution = None
          applyFormattingOptions = None }

let applyCSharpSectionConfigurationOnSettings oldSettings csharpSectionConfig =
    { oldSettings with
        SolutionPath = csharpSectionConfig.solution |> Option.orElse oldSettings.SolutionPath
        ApplyFormattingOptions =
            csharpSectionConfig.applyFormattingOptions
            |> Option.defaultValue oldSettings.ApplyFormattingOptions }

type DidChangeConfigurationSettingsDto =
    { csharp: CSharpSectionConfiguration option }

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

let documentSelectorForCSharpDocuments: DocumentSelector =
    [| csharpDocumentFilter |> U2.C1 |]

let documentSelectorForCSharpAndRazorDocuments (settings: ServerSettings) : DocumentSelector =
    match settings.RazorSupport with
    | true -> [| csharpDocumentFilter |> U2.C1; razorCsharpDocumentFilter |> U2.C1 |]
    | false -> [| csharpDocumentFilter |> U2.C1 |]

let emptyClientCapabilities: ClientCapabilities =
    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = None
      General = None
      Experimental = None }
