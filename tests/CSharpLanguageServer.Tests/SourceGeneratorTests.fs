module CSharpLanguageServer.Tests.SourceGeneratorTests

open System
open System.Diagnostics
open System.IO

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Tests.Tooling

let private runDotnetBuild (dir: string) =
    let psi = ProcessStartInfo("dotnet", "build")
    psi.WorkingDirectory <- dir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    let proc =
        match Process.Start(psi) with
        | null -> failwith "Failed to start dotnet build process"
        | p -> p

    use _ = proc
    proc.WaitForExit(60_000) |> ignore
    proc.ExitCode, proc.StandardOutput.ReadToEnd(), proc.StandardError.ReadToEnd()

/// Regression guard for issue #312 (generator DLL locking on Windows).
///
/// Older Roslyn versions (≤4.13) locked analyzer/generator DLLs via
/// DefaultAnalyzerAssemblyLoader, preventing rebuilds while the server was running.
/// Roslyn 5.3.0 (used by this project) shadow-copies by default via
/// ShadowCopyAnalyzerPathResolver, so the bug no longer manifests — this test
/// passes today and serves as a guard against future regressions.
///
/// Test sequence:
///   1. Pre-build the generator so the DLL exists on disk
///   2. Start the server (loads the DLL as an analyzer reference)
///   3. Hover on a generated symbol to force GetCompilationAsync (loads the DLL)
///   4. Rebuild the generator — succeeds only if the DLL is not locked
[<Test>]
let ``rebuilding a loaded source generator does not fail due to DLL locking`` () =
    // On Linux/macOS file replacement works even with memory-mapped files, so the
    // locking issue only ever manifested on Windows.
    Assume.That(Environment.OSVersion.Platform = PlatformID.Win32NT, "DLL locking only occurs on Windows")

    // Pre-build the generator in the temp fixture dir before the server starts, so
    // the DLL exists and Roslyn will actually load it when the workspace is opened.
    let prebuildGenerator (solutionDir: string) =
        let generatorDir = Path.Combine(solutionDir, "Generator")
        let exitCode, stdout, stderr = runDotnetBuild generatorDir

        if exitCode <> 0 then
            failwithf "Pre-build of generator failed (exit %d):\nstdout:\n%s\nstderr:\n%s" exitCode stdout stderr

    use client =
        activateFixtureExt "projectWithSourceGenerator" defaultClientProfile prebuildGenerator id

    use classFile = client.Open "Project/Class.cs"

    // Force the server to call GetCompilationAsync by issuing a hover request.
    // This loads the generator DLL into the server process.
    let hoverParams: HoverParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = { Line = 5u; Character = 52u } // "Hello" in Hello.World
          WorkDoneToken = None }

    let _: Hover option = client.Request("textDocument/hover", hoverParams)

    // Attempt to rebuild the generator. Without shadow-copying this would fail on
    // Windows with an IOException (file in use).
    let generatorDir = Path.Combine(client.SolutionDir, "Generator")

    let exitCode, stdout, stderr = runDotnetBuild generatorDir

    Assert.AreEqual(
        0,
        exitCode,
        sprintf
            "dotnet build failed (exit %d) — generator DLL is likely locked:\nstdout:\n%s\nstderr:\n%s"
            exitCode
            stdout
            stderr
    )

// Class.cs line 5 (0-based): "    static void Main() => System.Console.WriteLine(Hello.World);"
// "Hello" starts at character 52.
let private helloPosition: Position = { Line = 5u; Character = 52u }

[<Test>]
let ``go-to-definition on a generated symbol returns a csharp:/generated/ URI`` () =
    use client = activateFixture "projectWithSourceGenerator"
    use classFile = client.Open "Project/Class.cs"

    let definitionParams: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = helloPosition
          WorkDoneToken = None
          PartialResultToken = None }

    let definition: Declaration option = client.Request("textDocument/definition", definitionParams)

    match definition with
    | Some(U2.C2 locations) ->
        Assert.AreEqual(1, locations.Length)
        Assert.IsTrue(
            locations.[0].Uri.StartsWith "csharp:/generated/",
            sprintf "expected csharp:/generated/ URI, got: %s" locations.[0].Uri
        )
    | _ -> Assert.Fail(sprintf "expected Some Location[], got: %A" definition)

[<Test>]
let ``csharp/metadata returns source text for a generated document`` () =
    use client = activateFixture "projectWithSourceGenerator"
    use classFile = client.Open "Project/Class.cs"

    // Resolve the generated URI via go-to-definition
    let definitionParams: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = helloPosition
          WorkDoneToken = None
          PartialResultToken = None }

    let definition: Declaration option = client.Request("textDocument/definition", definitionParams)

    let generatedUri =
        match definition with
        | Some(U2.C2 [| loc |]) -> loc.Uri
        | _ -> failwithf "expected single Location from go-to-definition, got: %A" definition

    Assert.IsTrue(
        generatedUri.StartsWith "csharp:/generated/",
        sprintf "expected csharp:/generated/ URI, got: %s" generatedUri
    )

    let metadataParams: CSharpMetadataParams = { TextDocument = { Uri = generatedUri } }
    let metadata: CSharpMetadataResponse option = client.Request("csharp/metadata", metadataParams)

    Assert.IsTrue(metadata.IsSome, "expected Some response from csharp/metadata")

    Assert.IsTrue(
        metadata.Value.Source.Contains "public static class Hello",
        sprintf "expected generated source to contain 'public static class Hello', got:\n%s" metadata.Value.Source
    )

[<Test>]
let ``go-to-definition on generated symbol works without obj directory`` () =
    // The fixture temp dir is copied without bin/ or obj/, so the generated .g.cs
    // file does not exist on disk. This test validates that the virtual URI path
    // works regardless of whether obj/ files are present.
    use client = activateFixture "projectWithSourceGenerator"
    use classFile = client.Open "Project/Class.cs"

    let definitionParams: DefinitionParams =
        { TextDocument = { Uri = classFile.Uri }
          Position = helloPosition
          WorkDoneToken = None
          PartialResultToken = None }

    let definition: Declaration option = client.Request("textDocument/definition", definitionParams)

    match definition with
    | Some(U2.C2 locations) when locations.Length > 0 ->
        Assert.IsTrue(
            locations.[0].Uri.StartsWith "csharp:/generated/",
            sprintf "expected csharp:/generated/ URI, got: %s" locations.[0].Uri
        )
    | _ ->
        Assert.Fail(
            sprintf
                "expected Some Location[] with csharp:/generated/ URI even without obj/, got: %A"
                definition
        )
