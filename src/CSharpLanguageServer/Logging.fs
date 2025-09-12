namespace CSharpLanguageServer.Logging

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console

module Logging =
    let mutable private loggerFactory: option<ILoggerFactory> = None

    let setupLogging (minimumLevel: LogLevel) =

        let createConsoleLogger (builder: ILoggingBuilder) =
            let configureSimpleConsole (opts: SimpleConsoleFormatterOptions) =
                opts.SingleLine <- true
                opts.TimestampFormat <- "H:mm:ss.fff "
                opts.IncludeScopes <- true

            let configureConsoleLogger (opts: ConsoleLoggerOptions) =
                opts.LogToStandardErrorThreshold <- LogLevel.Trace // send everything to stderr

            builder
                .AddSimpleConsole(configureSimpleConsole)
                .AddConsole(configureConsoleLogger)
                .SetMinimumLevel(minimumLevel)
                |> ignore

        loggerFactory <- LoggerFactory.Create(createConsoleLogger) |> Some

    let getLoggerByName name = loggerFactory.Value.CreateLogger name
