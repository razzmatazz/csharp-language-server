module CSharpLanguageServer.Program

open System
open System.Reflection

open Argu
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Diagnostics


type CLIArguments =
    | [<AltCommandLine("-v")>] Version
    | [<AltCommandLine("-l")>] LogLevel of level:string
    | [<AltCommandLine("-s")>] Solution of solution:string
    | Diagnose
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Version -> "Display versioning information"
                | Solution _ -> "Specify .sln file to load (relative to CWD)"
                | LogLevel _ -> "Set log level, <trace|debug|info|warning|error>; default is `info`"
                | Diagnose -> "Run diagnostics"

[<EntryPoint>]
let entry args =
    let argParser = ArgumentParser.Create<CLIArguments>(programName = "csharp-ls")
    let serverArgs = argParser.Parse args

    let parseLogLevel logLevelArg =
        match logLevelArg with
        | "error" -> LogLevel.Error
        | "warning" -> LogLevel.Warning
        | "info" -> LogLevel.Information
        | "debug" -> LogLevel.Debug
        | "trace" -> LogLevel.Trace
        | _ -> LogLevel.Information

    let settings = {
        ServerSettings.Default with
            SolutionPath = serverArgs.TryGetResult <@ Solution @>
            LogLevel =
                serverArgs.TryGetResult(<@ LogLevel @>)
                |> Option.map parseLogLevel
                |> Option.defaultValue LogLevel.Information
    }

    try
        serverArgs.TryGetResult <@ Version @>
            |> Option.iter (fun _ -> printfn "csharp-ls, %s"
                                             (Assembly.GetExecutingAssembly().GetName().Version |> string)
                                     exit 0)

        let logLevelArg =
            serverArgs.TryGetResult(<@ CLIArguments.LogLevel @>)

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
                SolutionPath = serverArgs.TryGetResult(<@ CLIArguments.Solution @>)
                LogLevel = logLevel
        }

        match serverArgs.TryGetResult <@ Diagnose @> with
        | Some _ ->
            Logging.setupLogging LogLevel.Trace
            let exitCode = diagnoseSolution settings |> Async.RunSynchronously
            exitCode

        | _ ->
            Logging.setupLogging settings.LogLevel
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
