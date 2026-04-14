module CSharpLanguageServer.Types

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

type CSharpDebugConfiguration =
    { solutionLoadDelay: int option
      debugMode: bool option }

    static member Default =
        { solutionLoadDelay = None
          debugMode = None }

type CSharpConfiguration =
    { logLevel: string option
      applyFormattingOptions: bool option
      analyzersEnabled: bool option
      useMetadataUris: bool option
      razorSupport: bool option
      locale: string option
      debug: CSharpDebugConfiguration option
      solutionPathOverride: string option }

    member this.GetEffectiveFormattingOptions options =
        if this.applyFormattingOptions |> Option.defaultValue false then
            Some options
        else
            None

    static member Default =
        { logLevel = None
          applyFormattingOptions = None
          analyzersEnabled = None
          useMetadataUris = None
          razorSupport = None
          locale = None
          debug = None
          solutionPathOverride = None }

let mergeCSharpConfiguration (oldConfig: CSharpConfiguration) (newConfig: CSharpConfiguration) =
    { logLevel = newConfig.logLevel |> Option.orElse oldConfig.logLevel
      applyFormattingOptions =
        newConfig.applyFormattingOptions
        |> Option.orElse oldConfig.applyFormattingOptions
      analyzersEnabled = newConfig.analyzersEnabled |> Option.orElse oldConfig.analyzersEnabled
      useMetadataUris = newConfig.useMetadataUris |> Option.orElse oldConfig.useMetadataUris
      razorSupport = newConfig.razorSupport |> Option.orElse oldConfig.razorSupport
      locale = newConfig.locale |> Option.orElse oldConfig.locale
      debug =
        match newConfig.debug with
        | Some newDebug ->
            Some
                { solutionLoadDelay =
                    newDebug.solutionLoadDelay
                    |> Option.orElse (oldConfig.debug |> Option.bind _.solutionLoadDelay)
                  debugMode = newDebug.debugMode |> Option.orElse (oldConfig.debug |> Option.bind _.debugMode) }
        | None -> oldConfig.debug
      solutionPathOverride = newConfig.solutionPathOverride |> Option.orElse oldConfig.solutionPathOverride }

type DidChangeConfigurationSettingsDto = { csharp: CSharpConfiguration option }

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

let documentSelectorForCSharpAndRazorDocuments (config: CSharpConfiguration) : DocumentSelector =
    match config.razorSupport |> Option.defaultValue false with
    | true -> [| csharpDocumentFilter |> U2.C1; razorCsharpDocumentFilter |> U2.C1 |]
    | false -> [| csharpDocumentFilter |> U2.C1 |]

let emptyClientCapabilities: ClientCapabilities =
    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = None
      General = None
      Experimental = None }
