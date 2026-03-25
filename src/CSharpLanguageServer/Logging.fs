namespace CSharpLanguageServer.Logging

open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

/// An ILogger that forwards log messages to the LSP client as $/logTrace notifications.
/// Messages are sent fire-and-forget; a thread-static reentrant guard prevents infinite loops
/// when the JSON-RPC layer itself logs while sending the notification.
type LspTraceLogger(categoryName: string, provider: LspTraceLoggerProvider) =

    [<ThreadStatic; DefaultValue>]
    static val mutable private reentrant: bool

    interface ILogger with
        member _.IsEnabled(logLevel) =
            match provider.TraceLevel with
            | TraceValues.Off -> false
            | TraceValues.Messages -> logLevel >= LogLevel.Information
            | TraceValues.Verbose -> logLevel >= LogLevel.Trace
            | _ -> false

        member _.BeginScope(_state) =
            { new IDisposable with
                member _.Dispose() = () }

        member _.Log(logLevel, _eventId, state, exn, formatter) =
            if LspTraceLogger.reentrant then
                ()
            else

                let client = provider.Client

                match provider.TraceLevel, client with
                | TraceValues.Off, _ -> ()
                | _, None -> ()
                | traceLevel, Some(client: ILspClient) ->
                    let shouldSend =
                        match traceLevel with
                        | TraceValues.Messages -> logLevel >= LogLevel.Information
                        | TraceValues.Verbose -> logLevel >= LogLevel.Trace
                        | _ -> false

                    if shouldSend then
                        let rawMessage = formatter.Invoke(state, exn)

                        let message =
                            match traceLevel with
                            | TraceValues.Verbose -> sprintf "[%O] %s: %s" logLevel categoryName rawMessage
                            | _ -> rawMessage

                        let verbose = if not (isNull exn) then Some(sprintf "%O" exn) else None

                        let logParams = { Message = message; Verbose = verbose }

                        try
                            LspTraceLogger.reentrant <- true
                            client.LogTrace(logParams) |> Async.Start
                        finally
                            LspTraceLogger.reentrant <- false

/// An ILoggerProvider that creates LspTraceLogger instances. Holds mutable references to the
/// current LSP client and trace level, which are updated over the server lifecycle.
and LspTraceLoggerProvider() =
    let mutable client: ILspClient option = None
    let mutable traceLevel: TraceValues = TraceValues.Off
    let syncRoot = obj ()

    member _.Client = lock syncRoot (fun () -> client)

    member _.TraceLevel = lock syncRoot (fun () -> traceLevel)

    member _.SetClient(newClient: ILspClient option) =
        lock syncRoot (fun () -> client <- newClient)

    member _.SetTraceLevel(newLevel: TraceValues) =
        lock syncRoot (fun () -> traceLevel <- newLevel)

    interface ILoggerProvider with
        member this.CreateLogger(categoryName) = LspTraceLogger(categoryName, this)

        member _.Dispose() = ()

module Logging =
    let mutable private loggerFactory: option<ILoggerFactory> = None
    let mutable private traceLoggerProvider: LspTraceLoggerProvider option = None

    let setupLogging (minimumLevel: LogLevel) =

        let provider = new LspTraceLoggerProvider()
        traceLoggerProvider <- Some provider

        let configureLogging (builder: ILoggingBuilder) =
            let configureSimpleConsole (opts: SimpleConsoleFormatterOptions) =
                opts.TimestampFormat <- "H:mm:ss.fff "
                opts.IncludeScopes <- true

            let configureConsoleLogger (opts: ConsoleLoggerOptions) =
                opts.LogToStandardErrorThreshold <- LogLevel.Trace // send everything to stderr

            // Set the global minimum to Trace so that all messages reach our providers.
            // The LSP trace provider does its own filtering based on the current TraceValues.
            // The console provider is filtered to the user-requested level, but suppressed
            // when LSP tracing is active to avoid duplicate output.
            builder
                .SetMinimumLevel(LogLevel.Trace)
                .AddSimpleConsole(configureSimpleConsole)
                .AddConsole(configureConsoleLogger)
                .AddFilter<ConsoleLoggerProvider>(fun level ->
                    level >= minimumLevel && provider.TraceLevel = TraceValues.Off)
                .AddProvider(provider)
            |> ignore

        loggerFactory <- LoggerFactory.Create configureLogging |> Some

    let getLoggerByName name = loggerFactory.Value.CreateLogger name

    let setLspTraceClient (newClient: ILspClient option) =
        traceLoggerProvider |> Option.iter (fun p -> p.SetClient(newClient))

    let setLspTraceLevel (newLevel: TraceValues) =
        traceLoggerProvider |> Option.iter (fun p -> p.SetTraceLevel(newLevel))
