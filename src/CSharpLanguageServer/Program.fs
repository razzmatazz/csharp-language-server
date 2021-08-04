module CSharpLanguageServer.Program

open Microsoft.Build.Locator

[<EntryPoint>]
let entry args =

    MSBuildLocator.RegisterDefaults() |> ignore

    try
      System.Threading.ThreadPool.SetMinThreads(16, 16) |> ignore

      // default the verbosity to warning
      Server.start ()
    with
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
