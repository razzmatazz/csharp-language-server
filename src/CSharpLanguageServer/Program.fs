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
    | [<AltCommandLine("-l")>] LogLevel of level: string
    | [<AltCommandLine("-s")>] Solution of solution: string
    | Debug
    | Diagnose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "Display versioning information"
            | Solution _ -> "Specify .sln file to load (relative to CWD)"
            | LogLevel _ -> "Set log level, <trace|debug|info|warning|error>; default is `info`"
            | Debug -> "Enable debug mode"
            | Diagnose -> "Run diagnostics"


let parseLogLevel (debugMode: bool) (logLevelArg: string option) =
    match logLevelArg with
    | Some "error" -> LogLevel.Error
    | Some "warning" -> LogLevel.Warning
    | Some "info" -> LogLevel.Information
    | Some "debug" -> LogLevel.Debug
    | Some "trace" -> LogLevel.Trace
    | _ -> if debugMode then LogLevel.Debug else LogLevel.Information


[<EntryPoint>]
let entry args =
    try
        let argParser = ArgumentParser.Create<CLIArguments>(programName = "csharp-ls")
        let serverArgs = argParser.Parse args

        let printVersion () =
            printfn "csharp-ls, %s" (Assembly.GetExecutingAssembly().GetName().Version |> string)

        serverArgs.TryGetResult <@ Version @>
        |> Option.iter (fun _ ->
            printVersion ()
            exit 0)

        let debugMode: bool = serverArgs.Contains Debug
        let logLevel = serverArgs.TryGetResult(<@ LogLevel @>) |> parseLogLevel debugMode

        let settings =
            { ServerSettings.Default with
                DebugMode = debugMode
                SolutionPath = serverArgs.TryGetResult <@ Solution @>
                LogLevel = logLevel }

        let exitCode =
            match serverArgs.TryGetResult <@ Diagnose @> with
            | Some _ ->
                Logging.setupLogging LogLevel.Trace
                diagnose settings |> Async.RunSynchronously
            | _ ->
                Logging.setupLogging settings.LogLevel
                Server.start settings

        exitCode
    with
    | :? ArguParseException as ex ->
        eprintfn "%s" ex.Message

        match ex.ErrorCode with
        | ErrorCode.HelpText -> 0
        | _ -> 1 // Unrecognised arguments

    | e ->
        eprintfn "Server crashing error - %s \n %s" e.Message e.StackTrace
        3
