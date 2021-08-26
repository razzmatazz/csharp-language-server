module CSharpLanguageServer.Program

open Microsoft.Build.Locator

[<EntryPoint>]
let entry args =

    MSBuildLocator.RegisterDefaults() |> ignore

    try
      // default the verbosity to warning
      Server.start ()
    with
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
