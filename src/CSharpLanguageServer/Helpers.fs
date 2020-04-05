module CSharpLanguageServer.Helpers
open LSP.Types


// mappings taken from omnisharp-roslyn

let roslynTagToLspCompletion tag =
    match tag with
    | "Class"         -> CompletionItemKind.Class
    | "Delegate"      -> CompletionItemKind.Class
    | "Enum"          -> CompletionItemKind.Enum
    | "Interface"     -> CompletionItemKind.Interface
    | "Struct"        -> CompletionItemKind.Class
    | "Local"         -> CompletionItemKind.Variable
    | "Parameter"     -> CompletionItemKind.Variable
    | "RangeVariable" -> CompletionItemKind.Variable
    | "Const"         -> CompletionItemKind.Value
    | "EnumMember"    -> CompletionItemKind.Enum
    | "Event"         -> CompletionItemKind.Function
    | "Field"         -> CompletionItemKind.Field
    | "Method"        -> CompletionItemKind.Method
    | "Property"      -> CompletionItemKind.Property
    | "Label"         -> CompletionItemKind.Unit
    | "Keyword"       -> CompletionItemKind.Keyword
    | "Namespace"     -> CompletionItemKind.Module
    | _ -> CompletionItemKind.Property
