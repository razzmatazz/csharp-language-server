namespace CSharpLanguageServer.Logging

open System
open Microsoft.Extensions.Logging

module Logging =
    let mutable private loggerFactory: option<ILoggerFactory> = None

    let setupLogging (minimumLevel: LogLevel) =
        let factory = LoggerFactory.Create(fun builder ->
            builder
                .AddConsole(fun opts ->
                    opts.LogToStandardErrorThreshold <- LogLevel.Trace // send everything to stderr
                )
                .SetMinimumLevel(minimumLevel) |> ignore
        )

        loggerFactory <- Some factory

    let getLoggerByName name = loggerFactory.Value.CreateLogger name
