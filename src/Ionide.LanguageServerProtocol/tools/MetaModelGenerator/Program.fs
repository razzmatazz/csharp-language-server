namespace MetaModelGenerator

module Main =
  open Argu
  open System
  open System.Text.Json
  open System.IO

  type TypeArgs =
    | MetaModelPath of string
    | OutputFilePath of string

    interface IArgParserTemplate with
      member this.Usage: string =
        match this with
        | MetaModelPath _ ->
          "The path to metaModel.json. See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#metaModel"
        | OutputFilePath _ -> "The path to the output file. Should end with .fs"

  type ClientServerArgs =
    | MetaModelPath of string
    | OutputFilePath of string

    interface IArgParserTemplate with
      member this.Usage: string =
        match this with
        | MetaModelPath _ ->
          "The path to metaModel.json. See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#metaModel"
        | OutputFilePath _ -> "The path to the output file. Should end with .fs"

  type CommandArgs =
    | [<CliPrefix(CliPrefix.None)>] Types of ParseResults<TypeArgs>
    | [<CliPrefix(CliPrefix.None)>] ClientServer of ParseResults<ClientServerArgs>

    interface IArgParserTemplate with
      member this.Usage =
        match this with
        | Types _ -> "Generates Types from metaModel.json."
        | ClientServer _ -> "Generates Client/Server"

  let readMetaModel metamodelPath =
    async {

      printfn "Reading in %s" metamodelPath

      let! metaModel =
        File.ReadAllTextAsync(metamodelPath)
        |> Async.AwaitTask

      printfn "Deserializing metaModel"

      let parsedMetaModel =
        JsonSerializer.Deserialize<MetaModel.MetaModel>(metaModel, MetaModel.metaModelSerializerOptions)

      return parsedMetaModel
    }


  [<EntryPoint>]
  let main argv =

    let errorHandler =
      ProcessExiter(
        colorizer =
          function
          | ErrorCode.HelpText -> None
          | _ -> Some ConsoleColor.Red
      )

    let parser = ArgumentParser.Create<CommandArgs>(programName = "MetaModelGenerator", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    match results.GetSubCommand() with
    | Types r ->
      let metaModelPath = r.GetResult <@ TypeArgs.MetaModelPath @>
      let OutputFilePath = r.GetResult <@ TypeArgs.OutputFilePath @>

      let metaModel =
        readMetaModel metaModelPath
        |> Async.RunSynchronously

      GenerateTypes.generateType metaModel OutputFilePath
      |> Async.RunSynchronously

    | ClientServer r ->

      let metaModelPath = r.GetResult <@ ClientServerArgs.MetaModelPath @>
      let OutputFilePath = r.GetResult <@ ClientServerArgs.OutputFilePath @>

      let metaModel =
        readMetaModel metaModelPath
        |> Async.RunSynchronously

      GenerateClientServer.generateClientServer metaModel OutputFilePath
      |> Async.RunSynchronously

    0