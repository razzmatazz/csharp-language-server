namespace CSharpLanguageServer.Runtime

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.WorkspaceFolder

type RequestMode =
    | ReadOnly
    | ReadWrite
    | ReadOnlyBackground

type RequestEffects =
    { ClientInitializeEmitted: bool
      ClientShutdownEmitted: bool
      ClientCapabilityChange: ClientCapabilities option
      DocumentClosed: string list
      DocumentOpened: (string * int * DateTime) list
      DocumentTouched: (string * DateTime) list
      SettingsChange: CSharpConfiguration option
      TraceLevelChange: TraceValues option
      WorkspaceConfigurationChanged: WorkspaceFolder list option
      WorkspaceFolderChange: LspWorkspaceFolder list
      WorkspaceReloadRequested: TimeSpan list }

    static member Empty =
        { ClientInitializeEmitted = false
          ClientShutdownEmitted = false
          ClientCapabilityChange = None
          DocumentClosed = []
          DocumentOpened = []
          DocumentTouched = []
          SettingsChange = None
          TraceLevelChange = None
          WorkspaceConfigurationChanged = None
          WorkspaceFolderChange = []
          WorkspaceReloadRequested = [] }

    member this.WithClientInitialize() =
        { this with
            ClientInitializeEmitted = true }

    member this.WithClientShutdown() =
        { this with
            ClientShutdownEmitted = true }

    member this.WithClientCapabilityChange(capabilities) =
        { this with
            ClientCapabilityChange = Some capabilities }

    member this.WithDocumentClosed(uri) =
        { this with
            DocumentClosed = this.DocumentClosed @ [ uri ] }

    member this.WithDocumentOpened(uri, version, timestamp) =
        { this with
            DocumentOpened = this.DocumentOpened @ [ (uri, version, timestamp) ] }

    member this.WithDocumentTouched(uri, timestamp) =
        { this with
            DocumentTouched = this.DocumentTouched @ [ (uri, timestamp) ] }

    member this.WithSettingsChange(config) =
        { this with
            SettingsChange = Some config }

    member this.WithTraceLevelChange(traceLevel) =
        { this with
            TraceLevelChange = Some traceLevel }

    member this.WithWorkspaceConfigurationChanged(folders) =
        { this with
            WorkspaceConfigurationChanged = Some folders }

    member this.WithWorkspaceFolderChange(folder) =
        { this with
            WorkspaceFolderChange = this.WorkspaceFolderChange @ [ folder ] }

    member this.WithWorkspaceReloadRequested(delay) =
        { this with
            WorkspaceReloadRequested = this.WorkspaceReloadRequested @ [ delay ] }

type RequestContext
    (
        requestMode: RequestMode,
        lspClient: ILspClient,
        config: CSharpConfiguration,
        getWorkspaceFolder: DocumentUri -> bool -> Async<LspWorkspaceFolder option>,
        getWorkspaceFolderUriList: unit -> Async<string list>,
        clientCapabilities: ClientCapabilities,
        shutdownReceived: bool
    ) =
    let mutable effects = RequestEffects.Empty

    member _.LspClient = lspClient
    member _.Config = config
    member _.ClientCapabilities = clientCapabilities
    member _.ShutdownReceived = shutdownReceived
    member _.Effects = effects

    member _.GetWorkspaceFolder(uri) = getWorkspaceFolder

    member _.GetWorkspaceFolderReadySolution(uri) = async {
        let! wf = getWorkspaceFolder uri true

        match wf with
        | None -> return None, None
        | Some wf ->
            match wf.Solution with
            | Ready(_, solution) -> return Some wf, Some solution
            | _ -> return Some wf, None
    }

    member _.GetWorkspaceFolderList(withSolutionReady: bool) = async {
        let! wfUris = getWorkspaceFolderUriList ()
        let mutable wfs = []

        for uri in wfUris do
            let! wf = getWorkspaceFolder uri withSolutionReady

            match wf with
            | None -> failwithf "no LspWorkspaceFolder resolved for URI \"%s\"!" uri
            | Some wf -> wfs <- wf :: wfs

        return wfs
    }

    member _.UpdateEffects(update: RequestEffects -> RequestEffects) =
        match requestMode.IsReadOnlyBackground with
        | false -> effects <- update effects
        | true -> invalidOp "Effects are not allowed for this request"
