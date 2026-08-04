#I "../CSharpLanguageServer.Tests/bin/Debug/net10.0"
#r "Ionide.LanguageServerProtocol.dll"
#r "Newtonsoft.Json.dll"
#r "nunit.framework.dll"
#r "CSharpLanguageServer.dll"
#r "CSharpLanguageServer.Tests.dll"

open System
open System.Diagnostics
open System.IO
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Tests.Tooling

let profile =
    { defaultClientProfile with
        ServerConfig =
            { defaultClientProfile.ServerConfig with
                analyzersEnabled = Some true } }

let documentCount = 12

let addDocuments solutionDir =
    let projectDir = Path.Combine(solutionDir, "Project")

    for index in 1..documentCount do
        File.WriteAllText(Path.Combine(projectDir, $"Benchmark{index}.cs"), $"class Benchmark{index} {{ int unused; }}")

    let exitCode, stdout, stderr = runDotnetBuild projectDir

    if exitCode <> 0 && exitCode <> 1 then
        failwith $"Fixture build failed ({exitCode}):\n{stdout}\n{stderr}"

let requestDiagnostics (client: LspTestClient) uri =
    let parameters: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = uri }
          Identifier = None
          PreviousResultId = None }

    match
        client.Request<DocumentDiagnosticParams, DocumentDiagnosticReport option>("textDocument/diagnostic", parameters)
    with
    | Some(U2.C1 report) -> report.Items.Length
    | _ -> failwith $"Expected full diagnostics for {uri}"

let run () =
    let outputDirectory =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../CSharpLanguageServer.Tests/bin/Debug/net10.0"))

    Directory.SetCurrentDirectory(outputDirectory)

    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" profile addDocuments id

    use openDocument = client.Open("Project/Class.cs")
    requestDiagnostics client openDocument.Uri |> ignore

    let uris =
        [| for index in 1..documentCount -> fileUriForProjectDir client.SolutionDir $"Project/Benchmark{index}.cs" |]

    let serverProcess = client.GetState().ServerProcess.Value
    serverProcess.Refresh()
    let cpuBefore = serverProcess.TotalProcessorTime
    let total = Stopwatch.StartNew()

    let results =
        [| for uri in uris do
               let request = Stopwatch.StartNew()
               let diagnosticCount = requestDiagnostics client uri
               request.Stop()
               yield int64 request.Elapsed.TotalMilliseconds, diagnosticCount |]

    total.Stop()
    serverProcess.Refresh()

    printfn "request times (ms): %A" (results |> Array.map fst)
    printfn "diagnostics per request: %A" (results |> Array.map snd)
    printfn "wall time: %d ms" (int64 total.Elapsed.TotalMilliseconds)
    printfn "server CPU time: %d ms" (int64 (serverProcess.TotalProcessorTime - cpuBefore).TotalMilliseconds)

run ()
