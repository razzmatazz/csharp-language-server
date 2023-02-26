module CSharpLanguageServer.Util

open System
open System.Runtime.InteropServices
open System.IO
open Microsoft.CodeAnalysis.Classification;

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

let ClassificationTypeMap = Map [
    (ClassificationTypeNames.ClassName,           "class");
    (ClassificationTypeNames.Comment,             "comment");
    (ClassificationTypeNames.ControlKeyword,      "keyword");
    (ClassificationTypeNames.EnumMemberName,      "enumMember");
    (ClassificationTypeNames.EnumName,            "enum");
    (ClassificationTypeNames.EventName,           "event");
    (ClassificationTypeNames.ExtensionMethodName, "method");
    (ClassificationTypeNames.FieldName,           "property");
    (ClassificationTypeNames.InterfaceName,       "interface");
    (ClassificationTypeNames.LabelName,           "label");
    (ClassificationTypeNames.LocalName,           "variable");
    (ClassificationTypeNames.Keyword,             "keyword");
    (ClassificationTypeNames.MethodName,          "method");
    (ClassificationTypeNames.NamespaceName,       "namespace");
    (ClassificationTypeNames.NumericLiteral,      "number");
    (ClassificationTypeNames.Operator,            "operator");
    (ClassificationTypeNames.OperatorOverloaded,  "operator");
    (ClassificationTypeNames.ParameterName,       "parameter");
    (ClassificationTypeNames.PropertyName,        "property");
    (ClassificationTypeNames.RecordClassName,     "class");
    (ClassificationTypeNames.RecordStructName,    "struct");
    (ClassificationTypeNames.RegexText,           "regex");
    (ClassificationTypeNames.StringLiteral,       "string");
    (ClassificationTypeNames.StructName,          "struct");
    (ClassificationTypeNames.TypeParameterName,   "typeParameter")
]

let ClassificationModifierMap = Map [
    (ClassificationTypeNames.StaticSymbol, "static")
]

let SemanticTokenTypeMap =
    ClassificationTypeMap
    |> Map.values
    |> Seq.distinct
    |> fun types -> Seq.zip types (Seq.initInfinite (id >> uint32)) // There is no `flip` in F#, sadly
    |> Map.ofSeq

let SemanticTokenModifierMap =
    ClassificationModifierMap
    |> Map.values
    |> Seq.distinct
    |> fun modifiers -> Seq.zip modifiers (Seq.initInfinite (id >> uint32))
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
    |> Option.bind (fun t -> Map.tryFind t SemanticTokenTypeMap)

let GetSemanticTokenModifierFlagFromClassification (classification: string) =
    ClassificationModifierMap
    |> Map.tryFind classification
    |> Option.bind (fun m -> Map.tryFind m SemanticTokenModifierMap)
    |> Option.defaultValue 0u
    |> int32
    |> (<<<) 1u
