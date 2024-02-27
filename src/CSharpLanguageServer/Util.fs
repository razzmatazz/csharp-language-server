module CSharpLanguageServer.Util

open System
open System.Runtime.InteropServices
open System.IO

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis.Classification
open Serilog
open Serilog.Core
open Serilog.Sinks
open Serilog.Events
open Serilog.Configuration

let parseFileUri s: string =
    Uri(s).LocalPath

let tryParseFileUri s: string option =
    try
        let uri = Uri(s)
        Some uri.LocalPath
    with _ex ->
        None

let makeFileUri (path: string): string =
    let fullPath = Path.GetFullPath(path)

    match RuntimeInformation.IsOSPlatform(OSPlatform.Windows) with
    | true -> "file:///" + fullPath
    | false -> "file://" + fullPath

type AsyncLogFn = string -> Async<unit>

let unwindProtect cleanupFn op =
    async {
        try
            return! op
        finally
            cleanupFn ()
    }

// TPL Task's wrap exceptions in AggregateException, -- this fn unpacks them
let rec unpackException (exn : Exception) =
    match exn with
    | :? AggregateException as agg ->
        match Seq.tryExactlyOne agg.InnerExceptions with
        | Some x -> unpackException x
        | None -> exn
    | _ -> exn

// flip f takes its (first) two arguments in the reverse order of f, just like
// the function with the same name in Haskell.
let flip f x y = f y x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


type LspClientLogEventSink(formatProvider: IFormatProvider) =
    let mutable lspClientMaybe: ILspClient option = None

    let mapLogEventLevel lel =
        match lel with
        | LogEventLevel.Verbose -> MessageType.Log
        | LogEventLevel.Debug -> MessageType.Log
        | LogEventLevel.Information -> MessageType.Info
        | LogEventLevel.Warning -> MessageType.Warning
        | LogEventLevel.Error -> MessageType.Error
        | LogEventLevel.Fatal -> MessageType.Error
        | _ -> MessageType.Info

    let shouldEmitLogEvent (logEvent: LogEvent) =
        match logEvent.Level with
        | LogEventLevel.Information -> true
        | LogEventLevel.Warning -> true
        | LogEventLevel.Error -> true
        | _ -> false

    member __.SetLspClient(newLspClient: ILspClient option) =
        lspClientMaybe <- newLspClient

    interface ILogEventSink with
        member __.Emit(logEvent: LogEvent) =
            let shouldEmit = shouldEmitLogEvent logEvent

            match lspClientMaybe, shouldEmit with
            | Some lspClient, true ->
                let messageParams: LogMessageParams =
                    { Type = mapLogEventLevel logEvent.Level
                      Message = logEvent.RenderMessage(formatProvider) }

                lspClient.WindowLogMessage(messageParams) |> Async.StartAsTask |> ignore

            | _, _ -> ()
