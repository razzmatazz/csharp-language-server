namespace CSharpLanguageServer.Handlers

open System
open System.IO
open System.Reflection

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Util

[<RequireQualifiedAccess>]
module LifeCycle =
    let private logger = Logging.getLoggerByName "LifeCycle"

    let handleInitialize
        (getServerCapabilities: CSharpConfiguration -> InitializeParams -> ServerCapabilities)
        (context: RequestContext)
        (p: InitializeParams)
        : Async<LspResult<InitializeResult>> =
        async {
            context.UpdateEffects(_.WithClientInitialize())

            // Set trace level immediately so logging during initialization is forwarded
            // via $/logTrace (and console stderr is suppressed). The Emit also updates
            // ServerState when the buffered event replays after this handler returns.
            let initialTraceLevel = p.Trace |> Option.defaultValue TraceValues.Off
            Logging.setLspTraceLevel initialTraceLevel
            context.UpdateEffects(_.WithTraceLevelChange(initialTraceLevel))

            let windowShowMessage m =
                context.LspClient.WindowLogMessage({ Type = MessageType.Info; Message = m })

            let serverName = "csharp-ls"
            let serverVersion = Assembly.GetExecutingAssembly().GetName().Version |> string
            logger.LogInformation("initializing, {name} version {version}", serverName, serverVersion)
            logger.LogInformation("initial csharp config: {config}", context.Config |> string)

            do! windowShowMessage (sprintf "csharp-ls: initializing, version %s" serverVersion)

            logger.LogInformation(
                "{serverName} is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server",
                serverName
            )

            do!
                windowShowMessage (
                    sprintf
                        "csharp-ls: %s is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server"
                        serverName
                )

            initializeMSBuild ()

            logger.LogDebug("handleInitialize: p.ClientInfo: {clientInfo}", p.ClientInfo |> Option.map serialize)

            logger.LogDebug("handleInitialize: p.Capabilities: {caps}", serialize p.Capabilities)
            context.UpdateEffects(_.WithClientCapabilityChange(p.Capabilities))

            logger.LogDebug(
                "handleInitialize: p.RootPath={rootPath}, p.RootUri={rootUri}, p.WorkspaceFolders={wf}",
                p.RootPath,
                p.RootUri,
                p.WorkspaceFolders
            )

            let workspaceFoldersFallbackUri: DocumentUri =
                p.RootUri
                |> Option.orElseWith (fun () -> p.RootPath |> Option.map (Uri >> string))
                |> Option.defaultWith (fun () -> Directory.GetCurrentDirectory() |> (Uri >> string))

            let workspaceFolders =
                match p.WorkspaceFolders with
                | Some wfs -> wfs |> List.ofArray
                | None ->
                    [ { Uri = workspaceFoldersFallbackUri
                        Name = "root" } ]

            logger.LogInformation("handleInitialize: using workspaceFolders: {folders}", serialize workspaceFolders)

            context.UpdateEffects(_.WithWorkspaceConfigurationChanged(workspaceFolders))

            let initializeResult =
                let serverCapabilities = getServerCapabilities context.Config p

                let assemblyVersion =
                    Assembly.GetExecutingAssembly().GetName().Version
                    |> Option.ofObj
                    |> Option.map string

                { InitializeResult.Default with
                    Capabilities = serverCapabilities
                    ServerInfo =
                        Some
                            { Name = "csharp-ls"
                              Version = assemblyVersion } }

            return initializeResult |> LspResult.success
        }

    let handleInitialized
        (lspClient: ILspClient)
        (getDynamicRegistrations: CSharpConfiguration -> ClientCapabilities -> Registration list)
        (context: RequestContext)
        (_p: unit)
        : Async<LspResult<unit>> =
        async {
            logger.LogDebug("handleInitialized: \"initialized\" notification received from client")

            logger.LogDebug("handleInitialized: registrationParams..")

            let registrationParams =
                { Registrations =
                    getDynamicRegistrations context.Config context.ClientCapabilities
                    |> List.toArray }

            logger.LogDebug("handleInitialized: ClientRegisterCapability..")
            // TODO: Retry on error?
            try
                match! lspClient.ClientRegisterCapability registrationParams with
                | Ok _ -> ()
                | Error error ->
                    logger.LogWarning("handleInitialized: dynamic cap registration has failed with {error}", error)
            with ex ->
                logger.LogWarning("handleInitialized: dynamic cap registration has failed with {error}", string ex)

            logger.LogDebug("handleInitialized: retrieve csharp settings..")

            //
            // retrieve csharp settings
            //
            try
                let! workspaceCSharpConfig =
                    lspClient.WorkspaceConfiguration(
                        { Items =
                            [| { Section = Some "csharp"
                                 ScopeUri = None } |] }
                    )

                let csharpConfig =
                    workspaceCSharpConfig
                    |> Option.fromResult
                    |> Option.bind Seq.tryHead
                    |> Option.bind deserialize<CSharpConfiguration option>

                match csharpConfig with
                | None -> ()
                | Some csharpConfig ->
                    let newConfig = mergeCSharpConfiguration context.Config csharpConfig
                    context.UpdateEffects(_.WithSettingsChange(newConfig))

            with ex ->
                logger.LogWarning(
                    "handleInitialized: could not retrieve `csharp` workspace configuration section: {error}",
                    ex |> string
                )

            return Ok()
        }

    let handleShutdown (context: RequestContext) (_: unit) : Async<LspResult<unit>> = async {
        context.UpdateEffects(_.WithClientCapabilityChange(emptyClientCapabilities))
        context.UpdateEffects(_.WithClientShutdown())
        return Ok()
    }

    let handleExit (context: RequestContext) (_: unit) : Async<LspResult<unit>> = async {
        // Per LSP spec: exit with code 0 if shutdown was received first, 1 otherwise.
        let exitCode = if context.ShutdownReceived then 0 else 1

        logger.LogInformation(
            "handleExit: exiting with code {exitCode} (shutdownReceived={shutdownReceived})",
            exitCode,
            context.ShutdownReceived
        )

        Environment.Exit(exitCode)
        return Ok()
    }
