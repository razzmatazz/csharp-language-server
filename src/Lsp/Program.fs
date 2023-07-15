// TODO: Move the entrypoint of program to another project and add a argument parser
module CSharpLanguageServer.Program

open System
open Serilog
open Serilog.Events

open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Workspace
open CSharpLanguageServer.MSBuildWorkspacePatcher

[<EntryPoint>]
let entry args =
    MSBuildWorkspacePatcher.Patch()

    let logConfig =
        LoggerConfiguration()
            .MinimumLevel.Verbose()
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

    Server.start CSharpLspClient WorkspaceManager
