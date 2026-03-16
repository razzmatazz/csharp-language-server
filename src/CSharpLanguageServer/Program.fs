module CSharpLanguageServer.Program

open System
open System.IO
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
    | [<AltCommandLine("-f")>] Features of features: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "display versioning information"
            | Solution _ -> "specify .sln file to load (relative to CWD)"
            | LogLevel _ -> "set log level, <trace|debug|info|warning|error>; default is `info`"
            | Debug -> "enable debug mode"
            | Diagnose -> "run diagnostics"
            | Features _ -> "enable optional features, comma-separated: [metadata-uris, razor-support]"

let parseLogLevel (debugMode: bool) (logLevelArg: string option) =
    match logLevelArg with
    | Some "error" -> LogLevel.Error
    | Some "warning" -> LogLevel.Warning
    | Some "info" -> LogLevel.Information
    | Some "debug" -> LogLevel.Debug
    | Some "trace" -> LogLevel.Trace
    | _ -> if debugMode then LogLevel.Debug else LogLevel.Information

let getTopLevelFiles (dir: string) (patterns: string list) =
    patterns
    |> List.collect (fun pattern ->
        Directory.GetFiles(dir, pattern, SearchOption.TopDirectoryOnly)
        |> Array.toList)

let validateSolutionsOrTopLevelProjects
    (solutionPath: string option) : string option =
        match solutionPath with
        | Some solution ->
            let fullPath = Path.GetFullPath solution
            if not (File.Exists fullPath) then
                invalidArg "Solution" $"Solution file not found: %s{fullPath}"

            match Path.GetExtension(fullPath).ToLowerInvariant() with
                | ".sln" | ".slnx" -> Some fullPath
                | _ ->
                    invalidArg "Solution" $"Invalid solution file extension (expected .sln or .slnx): %s{fullPath}"

        | None ->
            let dir = Directory.GetCurrentDirectory()

            let slnxFiles = getTopLevelFiles dir [ "*.slnx" ]
            let slnFiles  = getTopLevelFiles dir [ "*.sln"  ]
            let projFiles = getTopLevelFiles dir [ "*.csproj"; ]

            match slnxFiles, slnFiles with
                // Prefer .slnx if exactly one
                | [ single ], _ ->
                    Some single

                // Multiple .slnx → must choose
                | xs, _ when List.length xs > 1 ->
                    invalidArg "Solution"
                        (sprintf
                            "Multiple .slnx files found in '%s'. Please specify one with -s/--solution.\n%s"
                            dir
                            (String.concat "\n" xs))

                // No .slnx, but exactly one .sln
                | [], [ single ] ->
                    Some single

                // Multiple .sln → must choose
                | [], xs when List.length xs > 1 ->
                    invalidArg "Solution"
                        (sprintf
                            "Multiple .sln files found in '%s'. Please specify one with -s/--solution.\n%s"
                            dir
                            (String.concat "\n" xs))

                // No solutions at all
                | [], [] ->
                    if List.isEmpty projFiles then
                        invalidArg "Solution"
                            $"No -s/--solution argument provided, and no .sln/.slnx or project files were found in '%s{dir}' (top-level)."
                    else
                        None
                | _ -> invalidArg "Solution" "Unable to process target directory"

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
        let logLevel = serverArgs.TryGetResult <@ LogLevel @> |> parseLogLevel debugMode

        let features: Set<string> =
            serverArgs.TryGetResult <@ Features @>
            |> Option.defaultValue ""
            |> _.Split(",")
            |> Seq.map _.Trim()
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Set.ofSeq

        let solutionsProvided = serverArgs.TryGetResult <@ Solution @>
        let solutionOrValidCurrentDir = validateSolutionsOrTopLevelProjects solutionsProvided

        let settings: ServerSettings =
            { ServerSettings.Default with
                DebugMode = debugMode
                SolutionPath = solutionOrValidCurrentDir
                UseMetadataUris = features.Contains "metadata-uris"
                RazorSupport = features.Contains "razor-support"
                LogLevel = logLevel }

        let exitCode: int =
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
