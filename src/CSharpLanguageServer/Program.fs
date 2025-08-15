module CSharpLanguageServer.Program

open System
open System.Reflection

open Argu
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging

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

        let logLevel =
            match logLevelArg with
            | Some "error" -> LogLevel.Error
            | Some "warning" -> LogLevel.Warning
            | Some "info" -> LogLevel.Information
            | Some "debug" -> LogLevel.Debug
            | Some "trace" -> LogLevel.Trace
            | _ -> LogLevel.Information

        Logging.setupLogging logLevel

        let settings = {
            ServerSettings.Default with
                SolutionPath = serverArgs.TryGetResult(<@ Options.CLIArguments.Solution @>)
                LogLevel = logLevel
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
