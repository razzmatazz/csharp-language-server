module CSharpLanguageServer.RoslynHelpers

open Microsoft.CodeAnalysis.CSharp
open LanguageServerProtocol
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open System
open Microsoft.CodeAnalysis.CodeRefactorings
open System.Reflection


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

let lspPositionForRoslynLinePosition (pos: LinePosition): Types.Position =
    { Line = pos.Line ; Character = pos.Character }

let roslynLinePositionForLspPosition (pos: Types.Position) =
    LinePosition(pos.Line, pos.Character)

let roslynLinePositionSpanForLspRange (range: Types.Range) =
    LinePositionSpan(
        roslynLinePositionForLspPosition range.Start,
        roslynLinePositionForLspPosition range.End)

let lspRangeForRoslynLinePosSpan (pos: LinePositionSpan): Types.Range =
    { Start = lspPositionForRoslynLinePosition pos.Start
      End = lspPositionForRoslynLinePosition pos.End }

let roslynCodeActionToLspCodeAction (ca: CodeActions.CodeAction): Types.CodeAction =
    let edit: Types.WorkspaceEdit = {
        Changes = None
        DocumentChanges = None
    }

    { Title = ca.Title
      Kind = None
      Diagnostics = None
      Edit = edit
      Command = None
    }

type DocumentSymbolCollector(documentUri) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols: Types.SymbolInformation list = []

    let collect (identifier: SyntaxToken) kind =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span
                      |> lspRangeForRoslynLinePosSpan
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
          Range = symbolLocation.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan
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

let refactoringProviderInstances =
    let assemblies =
        [ "Microsoft.CodeAnalysis.Features"
          "Microsoft.CodeAnalysis.CSharp.Features"
          "Microsoft.CodeAnalysis.Workspaces"
        ]
        |> Seq.map Assembly.Load
        |> Array.ofSeq

    let validType (t: Type) =
        (not (t.GetTypeInfo().IsInterface))
        && (not (t.GetTypeInfo().IsAbstract))
        && (not (t.GetTypeInfo().ContainsGenericParameters))

    let types =
        assemblies
        |> Seq.collect (fun a -> a.GetTypes())
        |> Seq.filter validType
        |> Seq.toArray

    let isCodeRefactoringProvider (t: Type) = t.IsAssignableTo(typeof<CodeRefactoringProvider>)

    let hasParameterlessConstructor (t: Type) = t.GetConstructor([| |]) <> null

    types
        |> Seq.filter isCodeRefactoringProvider
        |> Seq.filter hasParameterlessConstructor
        |> Seq.map Activator.CreateInstance
        |> Seq.filter (fun i -> i <> null)
        |> Seq.map (fun i -> i :?> CodeRefactoringProvider)
        |> Seq.toArray
