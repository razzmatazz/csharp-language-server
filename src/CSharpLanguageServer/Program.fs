module CSharpLanguageServer.Program

open System
open System.Reflection

open Argu
open Serilog
open Serilog.Core
open Serilog.Events

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp

[<EntryPoint>]
let entry args =
    try
        let argParser = ArgumentParser.Create<Options.CLIArguments>(programName = "csharp-ls")
        let serverArgs = argParser.Parse args

        serverArgs.TryGetResult(<@ Options.CLIArguments.Version @>)
            |> Option.iter (fun _ -> printfn "csharp-ls, %s"
                                             (Assembly.GetExecutingAssembly().GetName().Version |> string)
                                     exit 0)

        let logLevelArg =
            serverArgs.TryGetResult(<@ Options.CLIArguments.LogLevel @>)
            |> Option.defaultValue "log"

        let logLevel =
            match logLevelArg with
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

        let settings = {
            ServerSettings.Default with
                SolutionPath = serverArgs.TryGetResult(<@ Options.CLIArguments.Solution @>)
                LogLevel = logLevelArg
        }

        Server.start settings
    with
    | :? ArguParseException as ex ->
        eprintfn "%s" ex.Message

        match ex.ErrorCode with
        | ErrorCode.HelpText -> 0
        | _ -> 1  // Unrecognised arguments

    | e ->
        eprintfn "Server crashing error - %s \n %s" e.Message e.StackTrace
        3
