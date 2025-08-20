module CSharpLanguageServer.Program

open System
open System.Reflection

open Argu
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging

type CLIArguments =
    | [<AltCommandLine("-v")>] Version
    | [<AltCommandLine("-l")>] LogLevel of level:string
    | [<AltCommandLine("-s")>] Solution of solution:string
    | Debug
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Version -> "display versioning information"
                | Solution _ -> ".sln file to load (relative to CWD)"
                | LogLevel _ -> "log level, <trace|debug|info|warning|error>; default is `info`"
                | Debug -> "enable debug mode"


[<EntryPoint>]
let entry args =
    try
        let argParser = ArgumentParser.Create<CLIArguments>(programName = "csharp-ls")
        let serverArgs = argParser.Parse args

        let printVersion () =
            printfn "csharp-ls, %s"
                    (Assembly.GetExecutingAssembly().GetName().Version |> string)

        serverArgs.TryGetResult(<@ CLIArguments.Version @>)
            |> Option.iter (fun _ -> printVersion ();  exit 0)

        let debugMode: bool =  serverArgs.Contains Debug

        let logLevelArg = serverArgs.TryGetResult(<@ CLIArguments.LogLevel @>)

        let logLevel =
            match logLevelArg with
            | Some "error" -> LogLevel.Error
            | Some "warning" -> LogLevel.Warning
            | Some "info" -> LogLevel.Information
            | Some "debug" -> LogLevel.Debug
            | Some "trace" -> LogLevel.Trace
            | _ -> if debugMode then LogLevel.Debug else LogLevel.Information

        let settings = {
            ServerSettings.Default with
                SolutionPath = serverArgs.TryGetResult(<@ CLIArguments.Solution @>)
                LogLevel = logLevel
        }

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
