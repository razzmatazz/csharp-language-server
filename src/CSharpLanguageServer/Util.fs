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

let ClassificationTypeMap = Map [
    (ClassificationTypeNames.ClassName,             "class");
    (ClassificationTypeNames.Comment,               "comment");
    (ClassificationTypeNames.ConstantName,          "property");
    (ClassificationTypeNames.ControlKeyword,        "keyword");
    (ClassificationTypeNames.DelegateName,          "class");
    (ClassificationTypeNames.EnumMemberName,        "enumMember");
    (ClassificationTypeNames.EnumName,              "enum");
    (ClassificationTypeNames.EventName,             "event");
    (ClassificationTypeNames.ExtensionMethodName,   "method");
    (ClassificationTypeNames.FieldName,             "property");
    (ClassificationTypeNames.Identifier,            "variable");
    (ClassificationTypeNames.InterfaceName,         "interface");
    (ClassificationTypeNames.LabelName,             "variable");
    (ClassificationTypeNames.LocalName,             "variable");
    (ClassificationTypeNames.Keyword,               "keyword");
    (ClassificationTypeNames.MethodName,            "method");
    (ClassificationTypeNames.NamespaceName,         "namespace");
    (ClassificationTypeNames.NumericLiteral,        "number");
    (ClassificationTypeNames.Operator,              "operator");
    (ClassificationTypeNames.OperatorOverloaded,    "operator");
    (ClassificationTypeNames.ParameterName,         "parameter");
    (ClassificationTypeNames.PropertyName,          "property");
    (ClassificationTypeNames.RecordClassName,       "class");
    (ClassificationTypeNames.RecordStructName,      "struct");
    (ClassificationTypeNames.RegexText,             "regex");
    (ClassificationTypeNames.StringLiteral,         "string");
    (ClassificationTypeNames.StructName,            "struct");
    (ClassificationTypeNames.TypeParameterName,     "typeParameter");
    (ClassificationTypeNames.VerbatimStringLiteral, "string")
]

let ClassificationModifierMap = Map [
    (ClassificationTypeNames.StaticSymbol, "static")
]

// flip f takes its (first) two arguments in the reverse order of f, just like
// the function with the same name in Haskell.
let flip f x y = f y x

let SemanticTokenTypeMap =
    ClassificationTypeMap
    |> Map.values
    |> Seq.distinct
    |> flip Seq.zip (Seq.initInfinite uint32)
    |> Map.ofSeq

let SemanticTokenModifierMap =
    ClassificationModifierMap
    |> Map.values
    |> Seq.distinct
    |> flip Seq.zip (Seq.initInfinite uint32)
    |> Map.ofSeq

let SemanticTokenTypes =
    SemanticTokenTypeMap
    |> Seq.sortBy (fun kvp -> kvp.Value)
    |> Seq.map (fun kvp -> kvp.Key)

let SemanticTokenModifiers =
    SemanticTokenModifierMap
    |> Seq.sortBy (fun kvp -> kvp.Value)
    |> Seq.map (fun kvp -> kvp.Key)

let GetSemanticTokenIdFromClassification (classification: string) =
    ClassificationTypeMap
    |> Map.tryFind classification
    |> Option.bind (flip Map.tryFind SemanticTokenTypeMap)

let GetSemanticTokenModifierFlagFromClassification (classification: string) =
    ClassificationModifierMap
    |> Map.tryFind classification
    |> Option.bind (flip Map.tryFind SemanticTokenModifierMap)
    |> Option.defaultValue 0u
    |> int32
    |> (<<<) 1u


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

    member __.SetLspClient(newLspClient: ILspClient option) =
        lspClientMaybe <- newLspClient

    interface ILogEventSink with
        member __.Emit(logEvent: LogEvent) =
            match lspClientMaybe with
            | Some lspClient ->
                let messageParams: LogMessageParams =
                    { Type = mapLogEventLevel logEvent.Level
                      Message = logEvent.RenderMessage(formatProvider) }

                lspClient.WindowLogMessage(messageParams) |> Async.StartAsTask |> ignore

            | None -> ()
