module CSharpLanguageServer.RoslynHelpers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open LanguageServerProtocol
open Microsoft.CodeAnalysis.Text

let makeRangeForLinePosSpan (pos: LinePositionSpan): Types.Range =
    { Start = { Line = pos.Start.Line ; Character = pos.Start.Character }
      End = { Line = pos.End.Line ; Character = pos.End.Character } }

type DocumentSymbolCollector2(documentUri) =
    inherit CSharpSyntaxWalker()

    let mutable classSymbols: Types.SymbolInformation list = []

    member __.GetSymbols() =
        classSymbols
        |> Array.ofList

    override __.VisitClassDeclaration(node) =
        let location: Types.Location =
            { Uri = documentUri
              Range = node.Identifier.GetLocation().GetLineSpan().Span
                      |> makeRangeForLinePosSpan
            }

        let symbol: Types.SymbolInformation =
            { Name = node.Identifier.ToString()
              Kind = Types.SymbolKind.Class
              Location = location
              ContainerName = None
            }

        classSymbols <- symbol :: classSymbols
        ()
