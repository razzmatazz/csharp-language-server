module CSharpLanguageServer.Program

open Argu
open System.Reflection
open State

[<EntryPoint>]
let entry args =
    try
        let argParser = ArgumentParser.Create<Options.CLIArguments>(programName = "csharp-ls")
        let serverArgs = argParser.Parse args

        serverArgs.TryGetResult(<@ Options.CLIArguments.Version @>)
            |> Option.iter (fun _ -> printfn "csharp-ls, %s"
                                             (Assembly.GetExecutingAssembly().GetName().Version |> string)
                                     exit 0)

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
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message

        match ex.ErrorCode with
        | ErrorCode.HelpText -> 0
        | _ -> 1  // Unrecognised arguments

    | e ->
        printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
        3
