# Source Generator Support

Tracking issues: #25 (expose generated code to editor), #312 (generator DLL locking)

Two independent problems. Both stem from source generators already running inside the
server process via MSBuildWorkspace/Roslyn, but they can be implemented separately.

---

## 1 — DLL Locking (#312)

### Problem

MSBuildWorkspace loads all `<Analyzer>` / source-generator DLLs into the server process.
Roslyn's `DefaultAnalyzerAssemblyLoader` keeps a file handle open on each DLL for the
lifetime of the process. On Windows, `dotnet build` cannot overwrite a locked DLL, so
rebuilding a user-authored generator fails while the server is running.

(The Razor generator ships inside the SDK and is never rebuilt by the user — it is not
affected.)

### Fix: shadow-copy on load

`IAnalyzerAssemblyLoader` is a **public** Roslyn interface (`Microsoft.CodeAnalysis.dll`)
with two methods: `AddDependencyLocation(string)` and `LoadFromPath(string) : Assembly`.
Implement it directly in F# — no reflection needed for the loader itself:

```fsharp
// src/CSharpLanguageServer/Roslyn/WorkspaceServices.fs
type ShadowCopyAnalyzerAssemblyLoader(shadowDir: string) =
    let loaded = ConcurrentDictionary<string, Assembly>()

    interface IAnalyzerAssemblyLoader with
        member _.AddDependencyLocation(_path) = ()
        member _.LoadFromPath(path) =
            loaded.GetOrAdd(path, fun p ->
                let dir = Path.Combine(shadowDir, Guid.NewGuid().ToString("N"))
                Directory.CreateDirectory(dir) |> ignore
                let dest = Path.Combine(dir, Path.GetFileName(p))
                File.Copy(p, dest)
                Assembly.LoadFrom(dest))
```

`Assembly.LoadFrom` loads into the default `AssemblyLoadContext`. If isolation or
unload/reload support is needed later, switch to a custom `AssemblyLoadContext`.

### Wiring via reflection — `DefaultAnalyzerAssemblyLoaderProvider`

`WorkspaceServicesInterceptor` already intercepts every `GetService` call on the
workspace's `HostWorkspaceServices` (via Castle DynamicProxy), using the same
`Assembly.Load("…").GetType("…")` → `Activator.CreateInstance` pattern established
throughout `WorkspaceServices.fs` for other internal Roslyn types.

The injection point is `IAnalyzerAssemblyLoaderProvider`
(`Microsoft.CodeAnalysis.Workspaces.dll`, `internal`). Its concrete implementation
`DefaultAnalyzerAssemblyLoaderProvider` (`internal sealed`) has a **public constructor**:

```
ctor(IEnumerable<IAnalyzerAssemblyResolver> assemblyResolvers,
     IEnumerable<IAnalyzerPathResolver> assemblyPathResolvers)
```

This means we can instantiate it reflectively without needing `BindingFlags.NonPublic`:

```fsharp
// inside WorkspaceServicesInterceptor.Intercept, in the GetService branch:
| "Microsoft.CodeAnalysis.Host.IAnalyzerAssemblyLoaderProvider" ->
    let wkspAsm = Assembly.Load "Microsoft.CodeAnalysis.Workspaces"
    let providerType =
        wkspAsm.GetType "Microsoft.CodeAnalysis.Host.DefaultAnalyzerAssemblyLoaderProvider"
        |> nonNull "DefaultAnalyzerAssemblyLoaderProvider"

    // ShadowCopyAnalyzerPathResolver is internal in Microsoft.CodeAnalysis.dll
    // but has a public ctor(string baseDirectory) — same pattern as above
    let coreAsm = Assembly.Load "Microsoft.CodeAnalysis"
    let resolverType =
        coreAsm.GetType "Microsoft.CodeAnalysis.ShadowCopyAnalyzerPathResolver"
        |> nonNull "ShadowCopyAnalyzerPathResolver"
    let shadowResolver = Activator.CreateInstance(resolverType, shadowDir)

    // IAnalyzerPathResolver is internal — wrap in an IEnumerable<obj> via Array
    let resolvers = Array.CreateInstance(resolverType, 1)
    resolvers.SetValue(shadowResolver, 0)

    Activator.CreateInstance(providerType, [||], resolvers)  // no assembly resolvers
```

`ShadowCopyAnalyzerPathResolver` (in `Microsoft.CodeAnalysis.dll`, `internal sealed`)
intercepts `GetResolvedAnalyzerPath` to return a shadow-copy path, so the underlying
`AnalyzerAssemblyLoader` never holds a handle on the original DLL. This is exactly how
Roslyn's own tooling achieves shadow-copying — the path resolver is the extension point.

Note: `ShadowCopyAnalyzerAssemblyLoader` no longer exists in Roslyn 5.x; the
functionality was split into the `ShadowCopyAnalyzerPathResolver` + `AnalyzerAssemblyLoader`
components in this version. The hand-rolled loader shown above remains a valid fallback
if the provider injection proves unstable.

### Cleanup

Register a `Shutdown` handler to delete the shadow directory. Stale copies from unclean
exits are acceptable — they live in the system temp directory.

### Files to change

| File | Change |
|---|---|
| `Roslyn/WorkspaceServices.fs` | Add `ShadowCopyAnalyzerAssemblyLoader` (fallback loader); add `IAnalyzerAssemblyLoaderProvider` intercept branch in `WorkspaceServicesInterceptor` that instantiates `DefaultAnalyzerAssemblyLoaderProvider` with a `ShadowCopyAnalyzerPathResolver` |
| `Roslyn/Solution.fs` | Create and pass the shadow temp dir path into `CSharpLspHostServices` at workspace construction; register a `Shutdown` cleanup handler |

---

## 2 — Expose Generated Documents to the Editor (#25)

### Problem

Source-generated `.g.cs` files exist inside the Roslyn `Compilation` as
`SourceGeneratedDocument` objects. They are not at a stable disk path — they live under
`obj/` and may not exist after `dotnet clean`. The editor cannot open them, and
go-to-definition into generated code silently returns nothing because
`Location.fromRoslynLocation` (in `Conversions.fs`) checks `File.Exists` on the mapped
path and returns `None` when the file is absent.

### Overview

Three things are needed:

1. A **virtual URI scheme** for generated documents
2. A **content-serving path** through the existing `csharp/metadata` endpoint
3. **Location translation** so handlers emit virtual URIs instead of broken `file://` paths

No new LSP methods, no new handler registrations, no changes to `Lsp/Server.fs`,
`DocumentSync.fs`, `Types.fs`, or `Program.fs`. Always-on — no feature flag.

### 2.1 Virtual URI scheme

Follow the existing `csharp:/` convention (single-slash, matching
`workspaceFolderMetadataSymbolSourceViewUri` which produces
`csharp:/<projectPath>/decompiled/<symbol>.cs`). Define:

```
csharp:/generated/<projectFilePath>/<generatorAssemblyName>/<generatorTypeName>/<hintName>
```

All components URI-encoded. This tuple maps 1:1 to a `SourceGeneratedDocumentIdentity`:

```fsharp
doc.Identity.Generator.AssemblyName   // e.g. "MyGenerators"
doc.Identity.Generator.TypeName       // e.g. "MyGenerators.FooGenerator"
doc.Identity.HintName                 // e.g. "Foo.g.cs"
```

Add `encodeGeneratedDocumentUri` / `parseGeneratedDocumentUri` helpers alongside
`workspaceFolderMetadataSymbolSourceViewUri` / `workspaceFolderParseMetadataSymbolSourceViewUri`
in `Lsp/WorkspaceFolder.fs`.

### 2.2 Serve content via `csharp/metadata`

The existing flow already does what we need: the client receives a `csharp:/…` URI from
`textDocument/definition`, calls `csharp/metadata` with that URI, and opens the returned
text as a read-only buffer.

`CSharpMetadata.handle` currently parses the URI via
`workspaceFolderParseMetadataSymbolSourceViewUri`, finds the project, gets the
compilation, looks up the type by metadata name, and decompiles it. Add a parallel path:
if the URI matches the generated-document scheme, parse it via
`parseGeneratedDocumentUri`, call `project.GetSourceGeneratedDocumentsAsync()`, find the
matching document by identity, and return `doc.GetTextAsync()` directly — no
decompilation needed.

`CSharpMetadataResponse` already has the right shape
(`{ ProjectName; AssemblyName; SymbolName; Source }`) — populate from `doc.Identity`.

### 2.3 Translate locations

The key dispatch point is `workspaceFolderResolveSymbolLocation`, which matches on
`(l.IsInMetadata, l.IsInSource)`:

```fsharp
| true, _ ->   // metadata → decompile (if useMetadataUris enabled)
| false, true -> // source → Location.fromRoslynLocation
| _, _ ->      // catch-all → empty
```

Source-generated files land in the `false, true` arm but then fail the `File.Exists`
check inside `Location.fromRoslynLocation`. Add a check before (or at the top of) that
arm: resolve the location's `SourceTree` via `solution.GetDocument(tree)` — if it
returns a `SourceGeneratedDocument` (a public subclass of `Document` since Roslyn 4.0),
emit a virtual URI instead. This is the single most surgical change; all
definition/references/etc. handlers flow through this function.

The proposed helper:

```fsharp
// Roslyn/Conversions.fs, in module Location
let fromSourceGeneratedDocument
    (encodeUri: SourceGeneratedDocumentIdentity -> string -> string)
    (doc: SourceGeneratedDocument)
    (span: LinePositionSpan)
    : Location =
    { Uri = encodeUri doc.Identity doc.Project.FilePath
      Range = Range.fromLinePositionSpan span }
```

**Handlers that produce locations** (and will benefit without per-handler changes, since
they go through `workspaceFolderResolveSymbolLocation` or `Location.fromRoslynLocation`):

- Definition, References, Implementation, TypeDefinition, CallHierarchy, DocumentHighlight

Declaration is currently a stub (`notImplemented`).

### 2.4 `LspWorkspaceFolderDocumentType` extension

Add a `GeneratedDocument` case alongside the existing `UserDocument | DecompiledDocument
| AnyDocument`. Extend `workspaceFolderDocumentDetails` with a branch that parses the
generated-document URI, calls `project.GetSourceGeneratedDocumentsAsync()`, and matches
by identity.

### 2.5 Semantic model and symbol resolution

`workspaceFolderDocumentSemanticModel` and `workspaceFolderDocumentSymbol` both have
Razor-specific branches guarded by `uri.EndsWith ".cshtml"` that obtain a
`(compilation, SyntaxTree)` pair instead of working from a regular `Document`. Source-
generated URIs need the same treatment, but unlike Razor (which reverse-engineers the
generator's naming convention via `solutionGetRazorDocumentForPath`) source generators
expose identity directly via `GetSourceGeneratedDocumentsAsync()`.

These two functions are good candidates to generalise: a shared helper
`workspaceFolderVirtualDocumentSyntaxTree uri wf` returning
`(Compilation * SyntaxTree) option` could serve both `.cshtml` and `csharp:/generated/…`
cases, replacing the duplicated `EndsWith ".cshtml"` guards.

### 2.6 Diagnostics (optional follow-up)

`Diagnostic.handleWorkspaceDiagnostic` obtains diagnostics via
`project.GetCompilationAsync()` → `compilation.GetDiagnostics()`, grouped by
`d.Location.GetMappedLineSpan().Path`. Diagnostics in source-generated files will have
`obj/` paths that may not exist. These could be mapped to virtual URIs. Lower priority —
generated code is rarely where the user acts on a diagnostic.

### Files to change

| File | Change |
|---|---|
| `Lsp/WorkspaceFolder.fs` | Add `encodeGeneratedDocumentUri` / `parseGeneratedDocumentUri`; add `GeneratedDocument` to `LspWorkspaceFolderDocumentType`; extend `workspaceFolderDocumentDetails`; add source-generated check in `workspaceFolderResolveSymbolLocation`; generalise `workspaceFolderDocumentSemanticModel` and `workspaceFolderDocumentSymbol` |
| `Handlers/CSharpMetadata.fs` | Add branch to serve generated-document URIs |
| `Roslyn/Conversions.fs` | Add `Location.fromSourceGeneratedDocument` helper |

---

## What does NOT change

| File / area | Why |
|---|---|
| `DocumentSync.fs` | Generated documents are read-only — no `didOpen`/`didChange`/`didClose` |
| `Types.fs` / document selectors | No new language ID or filter |
| `Program.fs` / feature flags | Always-on |
| `Lsp/Server.fs` | No new handlers or endpoints |
| Razor path (`solutionGetRazorDocumentForPath`, `solutionFindSymbolForRazorDocumentPath`) | Purely additive; Razor stays on its own code path |

---

## Razor comparison

Razor support solved several of the same sub-problems. The two features are mirror
images: Razor maps *editor positions → generated tree* (`.cshtml` is the real file,
`.g.cs` is internal); source generators map *generated tree → virtual URI* (`.g.cs` is
what the editor opens).

**Reused / extended:**

| Existing code | How generators extend it |
|---|---|
| `workspaceFolderResolveSymbolLocation` (three match arms) | Add source-generated check in `IsInSource` arm |
| `LspWorkspaceFolderDocumentType` (`DecompiledDocument` case) | Add `GeneratedDocument` case |
| `workspaceFolderDocumentSemanticModel` (Razor `.cshtml` branch) | Generalise into shared virtual-doc helper |
| `workspaceFolderDocumentSymbol` (Razor `.cshtml` branch) | Same generalisation |
| `CSharpMetadata.handle` (decompiled branch) | Add generated-doc branch |

**Not reused:**

| Razor mechanism | Why generators don't need it |
|---|---|
| `solutionGetRazorDocumentForPath` (reverse-engineers naming convention) | `GetSourceGeneratedDocumentsAsync()` exposes identity directly |
| `AdditionalDocument` sync in `DocumentSync.fs` | Generated documents are read-only |
| `GetMappedLineSpan()` position translation | Generated files use their own coordinate space |
| `documentSelectorForCSharpAndRazorDocuments` | No new language ID |
| `--features razor-support` flag | Always-on |

---

## 3 — Test Fixture

### Approach

All integration tests follow one pattern: a static fixture directory under
`tests/CSharpLanguageServer.Tests/Fixtures/` is copied to a fresh temp directory at
test startup, a real server process is launched against it, and assertions are made over
the live JSON-RPC wire (`Tooling.fs`). The same pattern works for source generators.

No existing fixture includes a source generator. This will be the first.

### Fixture structure

```
Fixtures/
  projectWithSourceGenerator/
    Generator/
      Generator.csproj      ← builds the generator; compiled by MSBuild during load
      Generator.cs          ← IIncrementalGenerator implementation
    Project/
      Project.csproj        ← references Generator as an Analyzer
      Class.cs              ← uses a symbol produced by the generator
```

Using a `<ProjectReference OutputItemType="Analyzer">` inside the same fixture tree
means the fixture is self-contained and offline — no NuGet package download required at
test runtime. The fixture copy filter already preserves `.csproj` and `.cs` files;
`bin/` and `obj/` are excluded, so MSBuild inside the server process will restore and
build the generator fresh.

#### `Generator/Generator.csproj`

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>  <!-- required for analyzers -->
    <LangVersion>latest</LangVersion>
    <Nullable>enable</Nullable>
    <EnforceExtendedAnalyzerRules>true</EnforceExtendedAnalyzerRules>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Microsoft.CodeAnalysis.CSharp" Version="4.0.1" PrivateAssets="all" />
  </ItemGroup>
</Project>
```

Targeting `netstandard2.0` is the conventional requirement for Roslyn source generators.

#### `Generator/Generator.cs`

A minimal incremental generator that emits one class so there is exactly one generated
symbol to navigate to:

```csharp
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using System.Text;

[assembly: System.Reflection.AssemblyVersion("1.0.0")]

namespace TestGenerator;

[Generator]
public class HelloGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        context.RegisterPostInitializationOutput(ctx =>
        {
            ctx.AddSource("Generated.g.cs", SourceText.From("""
                namespace Generated;
                public static class Hello
                {
                    public static string World => "hello";
                }
                """, Encoding.UTF8));
        });
    }
}
```

Using `RegisterPostInitializationOutput` (no trigger — always fires) keeps the fixture
simple and deterministic. The generated class is `Generated.Hello`.

#### `Project/Project.csproj`

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\Generator\Generator.csproj"
                      OutputItemType="Analyzer"
                      ReferenceOutputAssembly="false" />
  </ItemGroup>
</Project>
```

#### `Project/Class.cs`

```csharp
// line 0: comment
using Generated;        // line 1
                        // line 2
class Program           // line 3
{                       // line 4
    static void Main() => System.Console.WriteLine(Hello.World);  // line 5
}
```

The `Hello` reference is on line 5 (0-based), character 52 (start of `Hello` after the
`(` at char 51). This is the navigation target for go-to-definition tests.

### Test file: `SourceGeneratorTests.fs`

```fsharp
module CSharpLanguageServer.Tests.SourceGeneratorTests

open System.Diagnostics
open System.IO
open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Tests.Tooling

[<TestFixture>]
type SourceGeneratorTests() =

    [<Test; Platform("Win")>]
    member _.``rebuilding a loaded source generator does not fail due to DLL locking``() =
        use client = activateFixture "projectWithSourceGenerator"
        use classFile = client.Open("Project/Class.cs")

        let generatorDir = Path.Combine(client.SolutionDir, "Generator")
        let psi = ProcessStartInfo("dotnet", "build")
        psi.WorkingDirectory <- generatorDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false

        use proc = Process.Start(psi)
        proc.WaitForExit(60_000) |> ignore

        Assert.AreEqual(
            0, proc.ExitCode,
            sprintf "dotnet build failed (exit %d) — generator DLL is likely locked:\n%s"
                proc.ExitCode (proc.StandardError.ReadToEnd()))

    [<Test>]
    member _.``go-to-definition on a generated symbol returns a csharp:/generated/ URI``() =
        use client = activateFixture "projectWithSourceGenerator"
        use classFile = client.Open("Project/Class.cs")

        let def: Declaration option =
            client.Request(
                "textDocument/definition",
                { TextDocument = { Uri = classFile.Uri }
                  Position = { Line = 5u; Character = 52u }  // "Hello" in Hello.World
                  WorkDoneToken = None
                  PartialResultToken = None })

        match def with
        | Some(U2.C2 locations) ->
            Assert.AreEqual(1, locations.Length)
            Assert.IsTrue(
                locations.[0].Uri.StartsWith("csharp:/generated/"),
                sprintf "expected csharp:/generated/ URI, got: %s" locations.[0].Uri)
        | _ -> Assert.Fail("expected Location[]")

    [<Test>]
    member _.``csharp/metadata returns source text for a generated document``() =
        use client = activateFixture "projectWithSourceGenerator"
        use classFile = client.Open("Project/Class.cs")

        // First resolve the generated URI via go-to-definition
        let def: Declaration option =
            client.Request(
                "textDocument/definition",
                { TextDocument = { Uri = classFile.Uri }
                  Position = { Line = 5u; Character = 52u }
                  WorkDoneToken = None
                  PartialResultToken = None })

        let generatedUri =
            match def with
            | Some(U2.C2 [| loc |]) -> loc.Uri
            | _ -> failwith "expected single Location"

        let metadata: CSharpMetadataResponse option =
            client.Request(
                "csharp/metadata",
                { TextDocument = { Uri = generatedUri } })

        Assert.IsTrue(metadata.IsSome)
        Assert.IsTrue(
            metadata.Value.Source.Contains("public static class Hello"),
            "expected generated source in response")

    [<Test>]
    member _.``go-to-definition on generated symbol works after dotnet clean``() =
        // Validates that the virtual URI path does not depend on obj/ files existing.
        // The fixture temp dir has no obj/ (excluded by copy filter), so this is
        // already the case without any extra setup.
        use client = activateFixture "projectWithSourceGenerator"
        use classFile = client.Open("Project/Class.cs")

        let def: Declaration option =
            client.Request(
                "textDocument/definition",
                { TextDocument = { Uri = classFile.Uri }
                  Position = { Line = 5u; Character = 52u }
                  WorkDoneToken = None
                  PartialResultToken = None })

        Assert.IsTrue(def.IsSome, "definition should resolve even without obj/")
```

### Fixture registration

Add to `CSharpLanguageServer.Tests.fsproj` after the last existing `<Compile>` entry:

```xml
<Compile Include="SourceGeneratorTests.fs" />
```

### DLL locking regression test (issue #312)

The same fixture can reproduce the locking bug. The test rebuilds the generator project
while the server holds the loaded DLL, which fails on Windows without shadow-copying.

`client.SolutionDir` exposes the temp directory path. After the server finishes loading
(the generator DLL is now locked in the server process), run `dotnet build` on the
generator subproject and assert on the exit code. The test code is included inline in the
`SourceGeneratorTests.fs` listing above.

**Platform gating:** `[<Platform("Win")>]` is an NUnit attribute that skips the test on
non-Windows runners. On Linux/macOS file replacement works even when the file is memory-
mapped, so the bug only manifests on Windows. CI already runs on `windows-latest` (in
`test.yaml` matrix: `[windows-latest, ubuntu-24.04]`).

**Why this is a failing test today:** The server loads the generator DLL via Roslyn's
default `AnalyzerAssemblyLoader`, which holds an open file handle on Windows. `dotnet
build` tries to write the recompiled DLL to the same `bin/` path and fails with an
`IOException` (file in use). After the shadow-copy fix, the server loads from a temp
copy, leaving the original path unlocked — `dotnet build` succeeds and the test passes.

Note: `LspTestClient.Dispose` already has special handling for Windows CI — it skips
temp directory cleanup when `GITHUB_ACTIONS` is set, specifically because of file-locking
issues. This test exercises the same class of problem at the application level.

### Coverage

| What is tested | Test |
|---|---|
| go-to-definition emits `csharp:/generated/` URI | `go-to-definition on a generated symbol…` |
| `csharp/metadata` serves generated source text | `csharp/metadata returns source text…` |
| virtual URI doesn't require `obj/` to exist on disk | `go-to-definition…works after dotnet clean` |
| **generator DLL not locked after load (Windows)** | **`rebuilding a loaded source generator…`** |
| (follow-up) references, hover, semantic tokens work over generated URI | add to existing test classes once URI scheme lands |

---

## Validation notes

_Last validated: 2025-04-11 against codebase at Roslyn 5.3.0 / net10.0_

**Source code** — all referenced file paths, functions, and types confirmed in the
codebase. Key references: `WorkspaceServicesInterceptor` (WorkspaceServices.fs:299),
`CSharpLspHostServices` (WorkspaceServices.fs:350),
`workspaceFolderResolveSymbolLocation` (WorkspaceFolder.fs:278),
`LspWorkspaceFolderDocumentType` (WorkspaceFolder.fs:73),
`Location.fromRoslynLocation` (Conversions.fs:41),
`CSharpMetadataResponse` (Types.fs:69, alias for `CSharpMetadataInformation`).

**Roslyn APIs** — `SourceGeneratedDocument`, `GetSourceGeneratedDocumentsAsync`,
`IAnalyzerAssemblyLoader`, `AnalyzerFileReference` are all public in 5.3.0.
`SourceGeneratedDocument.Identity` is public since 4.4.
`ShadowCopyAnalyzerAssemblyLoader` does not exist in Roslyn 5.x — the type was split
into `ShadowCopyAnalyzerPathResolver` (`internal sealed`, public ctor, `Microsoft.CodeAnalysis.dll`)
and `AnalyzerAssemblyLoader` (`internal` abstract, `Microsoft.CodeAnalysis.dll`).
`IAnalyzerAssemblyLoaderProvider` is `internal`; its concrete implementation
`DefaultAnalyzerAssemblyLoaderProvider` (`Microsoft.CodeAnalysis.Workspaces.dll`) has a
public constructor and is instantiable via `Activator.CreateInstance` using the same
`Assembly.Load("…").GetType("…")` pattern already in `WorkspaceServices.fs`.

**Docs to update post-implementation:**

- `docs/features.md` — add source-generator section (also: existing URI example
  `csharp:/metadata/projects/…` is outdated vs actual
  `csharp:/<projectPath>/decompiled/<symbol>.cs`)
- `docs/codebase-architecture.md` — note analyzer/generator loading in
  `Roslyn/WorkspaceServices.fs` section
- `README.md` — no changes unless a config toggle is added
