namespace MetaModelGenerator

module Formatting =
  open Fantomas.Core

  let formatConfig = {
    FormatConfig.Default with
        IndentSize = 4
        MaxRecordWidth = 80
        MaxValueBindingWidth = 120
        MaxFunctionBindingWidth = 120
        MaxDotGetExpressionWidth = 120
        MaxInfixOperatorExpression = 10
        ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems
        MultilineBracketStyle = MultilineBracketStyle.Aligned
        MultiLineLambdaClosingNewline = true
        InsertFinalNewline = false
  }


module FileWriters =
  open System.IO

  let writeIfChanged outputPath text =
    async {
      let writeToFile (path: string) (contents: string) = File.WriteAllTextAsync(path, contents)

      let! existingFile =
        async {
          if File.Exists(outputPath) then
            let! file =
              File.ReadAllTextAsync(outputPath)
              |> Async.AwaitTask

            return Some file
          else
            return None
        }

      printfn "Writing to %s" outputPath

      match existingFile with
      | Some existingFile when existingFile = text -> printfn "No changes"
      | _ ->
        do!
          text
          |> writeToFile outputPath
          |> Async.AwaitTask
    }


module Widgets =
  [<Literal>]
  let UriString = "URI"

  [<Literal>]
  let DocumentUriString = "DocumentUri"

  [<Literal>]
  let RegExpString = "RegExp"


[<AutoOpen>]
module TypeAnonBuilders =
  open Fabulous.AST
  open Fantomas.Core.SyntaxOak

  let pipe (right: WidgetBuilder<Expr>) (left: WidgetBuilder<Expr>) = Ast.InfixAppExpr(left, "|>", right)


  type Ast with

    static member LspUri() = Ast.LongIdent Widgets.DocumentUriString
    static member DocumentUri() = Ast.LongIdent Widgets.DocumentUriString
    static member LspRegExp() = Ast.LongIdent Widgets.DocumentUriString

    static member AsyncPrefix(t: WidgetBuilder<Type>) = Ast.AppPrefix(Ast.LongIdent "Async", [ t ])

    static member AsyncLspResultPrefix(t: WidgetBuilder<Type>) = Ast.AppPrefix(Ast.LongIdent "AsyncLspResult", [ t ])