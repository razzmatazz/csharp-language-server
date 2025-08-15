namespace CSharpLanguageServer.Handlers

open System
open System.IO
open System.Reflection

open Microsoft.Build.Locator
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.State
open CSharpLanguageServer.State.ServerState
open CSharpLanguageServer.Types
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Initialization =
    let private logger = Logging.getLoggerByName "Initialization"

    let handleInitialize (lspClient: ILspClient)
                         (setupTimer: unit -> unit)
                         (serverCapabilities: ServerCapabilities)
                         (context: ServerRequestContext)
                         (p: InitializeParams)
            : Async<LspResult<InitializeResult>> = async {
        // context.State.LspClient has not been initialized yet thus context.WindowShowMessage will not work
        let windowShowMessage m = lspClient.WindowLogMessage({ Type = MessageType.Info; Message = m })

        context.Emit(ClientChange (Some lspClient))

        let serverName = "csharp-ls"
        let serverVersion = Assembly.GetExecutingAssembly().GetName().Version |> string
        logger.LogInformation("initializing, {name} version {version}", serverName, serverVersion)

        do! windowShowMessage(
            sprintf "csharp-ls: initializing, version %s" serverVersion)

        logger.LogInformation(
            "{serverName} is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server",
            serverName)

        do! windowShowMessage(
            sprintf "csharp-ls: %s is released under MIT license and is not affiliated with Microsoft Corp.; see https://github.com/razzmatazz/csharp-language-server" serverName)

        let vsInstanceQueryOpt = VisualStudioInstanceQueryOptions.Default
        let vsInstanceList = MSBuildLocator.QueryVisualStudioInstances(vsInstanceQueryOpt)
        if Seq.isEmpty vsInstanceList then
            raise (InvalidOperationException("No instances of MSBuild could be detected." + Environment.NewLine + "Try calling RegisterInstance or RegisterMSBuildPath to manually register one."))

        // do! infoMessage "MSBuildLocator instances found:"
        //
        // for vsInstance in vsInstanceList do
        //     do! infoMessage (sprintf "- SDK=\"%s\", Version=%s, MSBuildPath=\"%s\", DiscoveryType=%s"
        //                              vsInstance.Name
        //                              (string vsInstance.Version)
        //                              vsInstance.MSBuildPath
        //                              (string vsInstance.DiscoveryType))

        let vsInstance = vsInstanceList |> Seq.head

        logger.LogInformation(
            "MSBuildLocator: will register \"{vsInstanceName}\", Version={vsInstanceVersion} as default instance",
            vsInstance.Name,
            (string vsInstance.Version))

        MSBuildLocator.RegisterInstance(vsInstance)

(*
        logger.LogTrace("handleInitialize: p.Capabilities={caps}", serialize p.Capabilities))
*)
        context.Emit(ClientCapabilityChange p.Capabilities)

        // TODO use p.RootUri
        let rootPath = Directory.GetCurrentDirectory()
        context.Emit(RootPathChange rootPath)

        // setup timer so actors get period ticks
        setupTimer()

        let initializeResult =
            let assemblyVersion =
                Assembly.GetExecutingAssembly().GetName().Version
                |> Option.ofObj
                |> Option.map string

            { InitializeResult.Default with
                    Capabilities = serverCapabilities
                    ServerInfo =
                      Some
                        { Name = "csharp-ls"
                          Version = assemblyVersion }}

        return initializeResult |> LspResult.success
    }

    let handleInitialized (lspClient: ILspClient)
                          (stateActor: MailboxProcessor<ServerStateEvent>)
                          (getRegistrations: ClientCapabilities -> Registration list)
                          (context: ServerRequestContext)
                          (_p: unit)
            : Async<LspResult<unit>> =
        async {
            logger.LogTrace("handleInitialized: \"initialized\" notification received from client")

            let registrationParams = { Registrations = getRegistrations context.ClientCapabilities |> List.toArray }

            // TODO: Retry on error?
            try
                match! lspClient.ClientRegisterCapability registrationParams with
                | Ok _ -> ()
                | Error error ->
                    logger.LogWarning("handleInitialized: didChangeWatchedFiles registration has failed with {error}", error)
            with
            | ex ->
                logger.LogWarning("handleInitialized: didChangeWatchedFiles registration has failed with {error}", string ex)

            //
            // retrieve csharp settings
            //
            try
                let! workspaceCSharpConfig =
                    lspClient.WorkspaceConfiguration(
                        { Items=[| { Section=Some "csharp"; ScopeUri=None } |] })

                let csharpConfigTokensMaybe =
                    match workspaceCSharpConfig with
                    | Ok ts -> Some ts
                    | _ -> None

                let newSettingsMaybe =
                  match csharpConfigTokensMaybe with
                  | Some [| t |] ->
                      let csharpSettingsMaybe = t |> deserialize<ServerSettingsCSharpDto option>

                      match csharpSettingsMaybe with
                      | Some csharpSettings ->

                          match csharpSettings.solution with
                          | Some solutionPath-> Some { context.State.Settings with SolutionPath = Some solutionPath }
                          | _ -> None

                      | _ -> None
                  | _ -> None

                // do! logMessage (sprintf "handleInitialized: newSettingsMaybe=%s" (string newSettingsMaybe))

                match newSettingsMaybe with
                | Some newSettings ->
                    context.Emit(SettingsChange newSettings)
                | _ -> ()
            with
            | ex ->
                logger.LogWarning(
                    "handleInitialized: could not retrieve `csharp` workspace configuration section: {error}",
                    ex |> string
                )

            //
            // start loading the solution
            //
            stateActor.Post(SolutionReloadRequest (TimeSpan.FromMilliseconds(100)))

            logger.LogTrace("handleInitialized: OK")

            return Ok()
        }

    let handleShutdown (context: ServerRequestContext) (_: unit) : Async<LspResult<unit>> = async {
        context.Emit(ClientCapabilityChange emptyClientCapabilities)
        context.Emit(ClientChange None)
        return Ok()
    }
