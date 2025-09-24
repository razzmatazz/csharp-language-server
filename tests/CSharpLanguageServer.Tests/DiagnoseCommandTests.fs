module CSharpLanguageServer.Tests.DiagnoseCommandTests

open System.IO
open System.Reflection
open System.Diagnostics
open System.Threading.Tasks

open NUnit.Framework

open CSharpLanguageServer.Tests.Tooling


[<TestCase>]
let testDiagnoseCommandWorks () =
    let testDataDirName = "testDiagnoseCommandWorks"

    let testAssemblyLocationDir =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let actualTestDataDir =
        DirectoryInfo(Path.Combine(testAssemblyLocationDir, "..", "..", "..", "TestData", testDataDirName))
        |> _.FullName

    let processStartInfo = makeServerProcessInfo actualTestDataDir
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
    Assert.IsEmpty(stdout)

    let stderr: string = stderrTask.Result
    Assert.IsTrue(stderr.Contains("diagnose: loading solution.."))
    Assert.IsTrue(stderr.Contains("csharp-ls: loading project"))
    Assert.IsTrue(stderr.Contains("diagnose: done"))
