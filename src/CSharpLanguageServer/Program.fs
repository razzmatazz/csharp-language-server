module CSharpLanguageServer.Program

open Argu
open System
open System.Reflection
open Serilog
open Serilog.Core
open Serilog.Events
open State

open CSharpLanguageServer.Logging
open CSharpLanguageServer.MSBuildWorkspacePatcher

[<EntryPoint>]
let entry args =
    MSBuildWorkspacePatcher.Patch()

    try
        let argParser = ArgumentParser.Create<Options.CLIArguments>(programName = "csharp-ls")
        let serverArgs = argParser.Parse args

        serverArgs.TryGetResult(<@ Options.CLIArguments.Version @>)
            |> Option.iter (fun _ -> printfn "csharp-ls, %s"
                                             (Assembly.GetExecutingAssembly().GetName().Version |> string)
                                     exit 0)
        let legacy =
            serverArgs.TryGetResult(<@ Options.CLIArguments.Legacy @>)
            |> Option.isSome

        if legacy then
            let parseLogLevel (s: string) =
                match s.ToLowerInvariant() with
                | "error" -> Ionide.LanguageServerProtocol.Types.MessageType.Error
                | "warning" -> Ionide.LanguageServerProtocol.Types.MessageType.Warning
                | "info" -> Ionide.LanguageServerProtocol.Types.MessageType.Info
                | "log" -> Ionide.LanguageServerProtocol.Types.MessageType.Log
                | _ -> Ionide.LanguageServerProtocol.Types.MessageType.Log

            // default the verbosity to warning
            let settings: ServerSettings = {
                SolutionPath = serverArgs.TryGetResult(<@ Options.CLIArguments.Solution @>)
                LogLevel = serverArgs.TryGetResult(<@ Options.CLIArguments.LogLevel @>)
                           |> Option.defaultValue "log"
                           |> parseLogLevel
            }

            Server.start settings
        else
            let logLevel =
                match serverArgs.TryGetResult(<@ Options.CLIArguments.LogLevel @>) |> Option.defaultValue "log" with
                | "error" -> LogEventLevel.Error
                | "warning" -> LogEventLevel.Warning
                | "info" -> LogEventLevel.Information
                | "log" -> LogEventLevel.Verbose
                | _ -> LogEventLevel.Information
            let logConfig =
                LoggerConfiguration()
                    .MinimumLevel.ControlledBy(LoggingLevelSwitch(logLevel))
                    .Enrich.FromLogContext()
                    .WriteTo.Async(fun conf ->
                        conf.Console(
                            outputTemplate =
                                "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}",
                            // Redirect all logs to stderr since stdout is used to communicate with client.
                            standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose),
                            theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
                        )
                        |> ignore)

            Log.Logger <- logConfig.CreateLogger()

            CSharpLanguageServer.Lsp.Server.start CSharpLanguageServer.Lsp.CSharpLspClient CSharpLanguageServer.Workspace.WorkspaceManager
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message

        match ex.ErrorCode with
        | ErrorCode.HelpText -> 0
        | _ -> 1  // Unrecognised arguments

    | e ->
        printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
        3
