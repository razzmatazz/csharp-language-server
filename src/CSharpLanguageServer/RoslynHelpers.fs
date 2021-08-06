module CSharpLanguageServer.RoslynHelpers

open Microsoft.CodeAnalysis.CSharp
open LanguageServerProtocol
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols


let roslynTagToLspCompletion tag =
    match tag with
    | "Class"         -> Types.CompletionItemKind.Class
    | "Delegate"      -> Types.CompletionItemKind.Class
    | "Enum"          -> Types.CompletionItemKind.Enum
    | "Interface"     -> Types.CompletionItemKind.Interface
    | "Struct"        -> Types.CompletionItemKind.Class
    | "Local"         -> Types.CompletionItemKind.Variable
    | "Parameter"     -> Types.CompletionItemKind.Variable
    | "RangeVariable" -> Types.CompletionItemKind.Variable
    | "Const"         -> Types.CompletionItemKind.Value
    | "EnumMember"    -> Types.CompletionItemKind.Enum
    | "Event"         -> Types.CompletionItemKind.Function
    | "Field"         -> Types.CompletionItemKind.Field
    | "Method"        -> Types.CompletionItemKind.Method
    | "Property"      -> Types.CompletionItemKind.Property
    | "Label"         -> Types.CompletionItemKind.Unit
    | "Keyword"       -> Types.CompletionItemKind.Keyword
    | "Namespace"     -> Types.CompletionItemKind.Module
    | _ -> Types.CompletionItemKind.Property


let makeRangeForLinePosSpan (pos: LinePositionSpan): Types.Range =
    { Start = { Line = pos.Start.Line ; Character = pos.Start.Character }
      End = { Line = pos.End.Line ; Character = pos.End.Character } }


type DocumentSymbolCollector(documentUri) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols: Types.SymbolInformation list = []

    let collect (identifier: SyntaxToken) kind =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span
                      |> makeRangeForLinePosSpan
            }

        let symbol: Types.SymbolInformation =
            { Name = identifier.ToString()
              Kind = kind
              Location = location
              ContainerName = None
            }

        collectedSymbols <- symbol :: collectedSymbols

    member __.GetSymbols() = collectedSymbols |> List.rev |> Array.ofList

    override __.VisitClassDeclaration(node) =
        collect node.Identifier Types.SymbolKind.Class

        base.VisitClassDeclaration(node)

    override __.VisitMethodDeclaration(node) =
        collect node.Identifier Types.SymbolKind.Method

        base.VisitMethodDeclaration(node)


let symbolToLspSymbolInformation (symbol: ISymbol): Types.SymbolInformation =
    let documentUri = "file:///xxx"

    let symbolLocation = symbol.Locations |> Seq.head

    let location: Types.Location =
        { Uri = documentUri
          Range = symbolLocation.GetLineSpan().Span |> makeRangeForLinePosSpan
        }

    { Name = symbol.Name
      Kind = Types.SymbolKind.File
      Location = location
      ContainerName = None }


let findSymbols (solution: Solution) pattern (limit: int option): Async<Types.SymbolInformation list> = async {
    let mutable symbolsFound = []

    for project in solution.Projects do
        let! symbols = SymbolFinder.FindSourceDeclarationsWithPatternAsync(
                           project, pattern, SymbolFilter.TypeAndMember)
                       |> Async.AwaitTask
        symbolsFound <- (List.ofSeq symbols) @ symbolsFound

    return Seq.map symbolToLspSymbolInformation symbolsFound |> List.ofSeq
}
