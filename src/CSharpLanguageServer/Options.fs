namespace CSharpLanguageServer

module Options =
  open Argu

  type CLIArguments =
      | [<AltCommandLine("-v")>] Version
      | [<AltCommandLine("-l")>] LogLevel of level:string
      | [<AltCommandLine("-s")>] Solution of solution:string
      with
          interface IArgParserTemplate with
              member s.Usage =
                  match s with
                  | Version -> "display versioning information"
                  | Solution _ -> ".sln file to load (relative to CWD)"
                  | LogLevel _ -> "log level, <log|info|warning|error>; default is `log`"
