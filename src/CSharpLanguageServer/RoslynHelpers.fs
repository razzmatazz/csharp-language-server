module CSharpLanguageServer.RoslynHelpers

open Microsoft.CodeAnalysis.CSharp
open LanguageServerProtocol
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis

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
