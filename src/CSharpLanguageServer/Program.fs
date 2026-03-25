module CSharpLanguageServer.Program

open System
open System.Reflection

open Argu
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Diagnostics
open CSharpLanguageServer.Runtime.JsonRpcServer

type CLIArguments =
    | [<AltCommandLine("-v")>] Version
    | [<AltCommandLine("-l")>] LogLevel of level: string
    | [<AltCommandLine("-s")>] Solution of solution: string
    | Debug
    | Diagnose
    | [<AltCommandLine("-f")>] Features of features: string
    | RpcLog of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "display versioning information"
            | Solution _ -> "specify .sln file to load (relative to CWD)"
            | LogLevel _ -> "set log level, <trace|debug|info|warning|error>; default is `info`"
            | Debug -> "enable debug mode"
            | Diagnose -> "run diagnostics"
            | Features _ -> "enable optional features, comma-separated: [metadata-uris, razor-support]"
            | RpcLog _ -> "write RPC wire log to file (READ/WRITE/ERROR/...)"

let parseLogLevel (logLevelArg: string option) =
    match logLevelArg with
    | Some "error" -> LogLevel.Error
    | Some "warning" -> LogLevel.Warning
    | Some "info" -> LogLevel.Information
    | Some "debug" -> LogLevel.Debug
    | Some "trace" -> LogLevel.Trace
    | _ -> LogLevel.Information

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
        let logLevel = serverArgs.TryGetResult <@ LogLevel @> |> parseLogLevel

        let features: Set<string> =
            serverArgs.TryGetResult <@ Features @>
            |> Option.defaultValue ""
            |> _.Split(",")
            |> Seq.map _.Trim()
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Set.ofSeq

        let settings =
            { ServerSettings.Default with
                DebugMode = debugMode
                SolutionPath = serverArgs.TryGetResult <@ Solution @>
                UseMetadataUris = features.Contains "metadata-uris"
                RazorSupport = features.Contains "razor-support"
                LogLevel = logLevel }

        let makeRpcLogCallback (path: string) : RpcLogEntry -> unit =
            let writer = new System.IO.StreamWriter(path, append = false)

            fun entry ->
                let line =
                    match entry with
                    | RpcRead msg -> sprintf "READ  %s" (string msg)
                    | RpcWrite msg -> sprintf "WRITE %s" (string msg)
                    | RpcError text -> sprintf "ERROR %s" text
                    | RpcWarn text -> sprintf "WARN  %s" text
                    | RpcDebug text -> sprintf "DEBUG %s" text

                writer.WriteLine(line)
                writer.Flush()

        let rpcLogCallback: (RpcLogEntry -> unit) option =
            serverArgs.TryGetResult <@ RpcLog @> |> Option.map makeRpcLogCallback

        let exitCode =
            match serverArgs.TryGetResult <@ Diagnose @> with
            | Some _ ->
                Logging.setupLogging LogLevel.Trace
                diagnose settings |> Async.RunSynchronously
            | _ ->
                Logging.setupLogging settings.LogLevel

                let jsonRpcLogger = Logging.getLoggerByName "JsonRpcServer"

                let jsonRpcLogCallback (entry: RpcLogEntry) =
                    match entry with
                    | RpcError text -> jsonRpcLogger.LogError("RPC error: {Message}", text)
                    | RpcWarn text -> jsonRpcLogger.LogWarning("RPC warning: {Message}", text)
                    | RpcDebug text -> jsonRpcLogger.LogDebug("RPC debug: {Message}", text)
                    | RpcRead _ -> ()
                    | RpcWrite _ -> ()

                let combinedRpcLogCallback =
                    match rpcLogCallback with
                    | Some fileCallback ->
                        Some(fun entry ->
                            fileCallback entry
                            jsonRpcLogCallback entry)
                    | None -> Some jsonRpcLogCallback

                Server.start settings combinedRpcLogCallback

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
