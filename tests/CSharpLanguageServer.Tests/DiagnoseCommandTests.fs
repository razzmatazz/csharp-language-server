module CSharpLanguageServer.Tests.DiagnoseCommandTests

open System.IO
open System.Reflection
open System.Diagnostics
open System.Threading.Tasks

open NUnit.Framework

open CSharpLanguageServer.Tests.Tooling


[<Test>]
let testDiagnoseCommandWorks () =
    let fixtureDir = "genericProject"

    let testAssemblyLocationDir =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        |> nonNull "Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)"

    let actualFixtureDir =
        DirectoryInfo(Path.Combine(testAssemblyLocationDir, "..", "..", "..", "Fixtures", fixtureDir))
        |> _.FullName

    let processStartInfo = makeServerProcessInfo actualFixtureDir
    processStartInfo.Arguments <- "--diagnose"

    let p = new Process()
    p.StartInfo <- processStartInfo

    let startResult = p.Start()

    if not startResult then
        failwith "Failed to start server process."

    let stdoutTask = p.StandardOutput.ReadToEndAsync()
    let stderrTask = p.StandardError.ReadToEndAsync()

    p.WaitForExit(1000 * 10) |> ignore

    if not p.HasExited then
        p.Kill()
        p.WaitForExit()

    Task.WaitAll(stdoutTask, stderrTask)

    let stdout: string = stdoutTask.Result
    Assert.That(stdout, Is.Empty)

    let stderr: string = stderrTask.Result
    Assert.That(stderr, Does.Contain("diagnose: loading solution.."))
    Assert.That(stderr, Does.Contain("csharp-ls: Loading solution"))
    Assert.That(stderr, Does.Contain("diagnose: done"))
