namespace CSharpLanguageServer

module Options =
  open Argu

  type CLIArguments =
      | Version
      with
          interface IArgParserTemplate with
              member s.Usage =
                  match s with
                  | Version -> "display versioning information"
