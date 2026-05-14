# Razor Test Failures: .NET SDK 10.0.200 → 10.0.300

Investigation into why cshtml/Razor integration tests broke after upgrading the .NET SDK
from 10.0.200 to 10.0.300.

---

## Symptom

14 integration tests fail with empty/null results. All failures are in tests that exercise
cshtml (Razor view) files: hover, completion, diagnostics, references, document-highlight,
document-sync, and workspace-diagnostics. Regular `.cs` tests are unaffected.

Representative failure:

```
Failed testPullDiagnosticsWorkForRazorFiles
  Expected: 1
  But was:  0
```

---

## Root Cause — Layer 1: Razor is now a true Roslyn source generator

### How Razor code generation worked in SDK ≤ 10.0.201

The Razor toolchain compiled `.cshtml` files into intermediate `.g.cs` files **before**
invoking the C# compiler. These files were written to `obj/` and passed to CSC as
ordinary source inputs. When `MSBuildWorkspace` loaded the project, those files appeared
as regular `Document` objects. Their `SyntaxTree.FilePath` matched paths like:

```
<projectDir>/obj/Debug/net10.0/Views/Test/ViewFileWithErrors_cshtml.g.cs
```

or (older flat format):

```
<projectDir>/obj/Debug/net10.0/Views_Test_ViewFileWithErrors_cshtml.g.cs
```

Both variants are matched by `solutionGetRazorDocumentForPath` in `Solution.fs` via
`cshtmlPathTranslated` and `altCshtmlPathTranslated`. The existing TODO comment
acknowledges this duality:

```fsharp
// TODO: drop this later, this is for compat with .NET SDK < "10.201"
let altCshtmlPathTranslated =
    cshtmlPathTranslated |> _.Replace(Path.DirectorySeparatorChar, '_')
```

The generated tree contained `#line` directives pointing back to the original `.cshtml`
path, so `GetMappedLineSpan()` translated positions between C# and cshtml coordinates.

### What changed in SDK 10.0.300

The Razor toolchain was refactored to use a true Roslyn `IIncrementalGenerator`
(`Microsoft.NET.Sdk.Razor.SourceGenerators.RazorSourceGenerator`, inside
`Microsoft.CodeAnalysis.Razor.Compiler`).

Consequences:

1. **No `.g.cs` files are written to disk.** The generator output is entirely in-memory.
2. **`compilation.SyntaxTrees` no longer contains the generated tree.** Those files were
   formerly static CSC inputs; now they are source-generator outputs — a separate Roslyn
   concept.
3. **`project.AdditionalDocuments` contains the `.cshtml` files**, but only as raw
   markup, with no C# or `#line` directives.

Net result: `solutionGetRazorDocumentForPath` searches `compilation.SyntaxTrees`, finds
no matching `.g.cs` path, and returns `None`. Every handler that calls it returns empty
results.

### Confirmation from instrumentation

Added `eprintfn` to `solutionGetRazorDocumentForPath` and ran the test with
`LoggingEnabled = true`:

```
looking for cshtmlPathTranslated=Views/Test/ViewFileWithErrors_cshtml.g.cs
looking for altCshtmlPathTranslated=Views_Test_ViewFileWithErrors_cshtml.g.cs
compilation.SyntaxTrees count=8
tree.FilePath=…/Project/Class.cs
tree.FilePath=…/Controllers/TestController.cs
… (6 more regular .cs and obj/ .cs files, no .g.cs)
cshtmlTree found=false
sourceGeneratedDocuments count=0
```

---

## Root Cause — Layer 2: `solutionGetRazorDocumentForPath` uses the wrong lookup

### The existing source generator support

The codebase already supports regular source generators via `GetSourceGeneratedDocumentsAsync`
— `SourceGeneratorTests.fs` passes and `WorkspaceFolder.fs` uses `d.HintName` on
`SourceGeneratedDocument`. However, those tests use a `prebuildGenerator` fixture patch:

```fsharp
let private prebuildGenerator (solutionDir: string) =
    let generatorDir = Path.Combine(solutionDir, "Generator")
    let exitCode, stdout, stderr = runDotnetBuild generatorDir
    ...
```

This runs `dotnet build` on the generator project before the server starts, producing the
generator DLL that Roslyn then loads as an `AnalyzerReference`.

### The Razor generator and its editorconfig — no special treatment needed

> **Correction to earlier investigation:** a brief detour suspected the
> `Project.GeneratedMSBuildEditorConfig.editorconfig` was missing and needed to be
> synthesised manually. **This is wrong.** The file is produced naturally by
> MSBuildWorkspace's own design-time build and no extra code is required. See the
> "Dropped approach" section below for details.

MSBuildWorkspace's `ProjectBuildManager.BuildProjectInstanceAsync` runs the `Compile`
and `CoreCompile` MSBuild targets with `SkipCompilerExecution=true`. As part of that
target chain, `GenerateMSBuildEditorConfigFile` (with `BeforeTargets="CoreCompile"`) is
triggered automatically. The Razor SDK (`Microsoft.NET.Sdk.Razor`, which is part of the
.NET SDK itself — not a NuGet package) registers the necessary `CompilerVisibleProperty`
and `CompilerVisibleItemMetadata` items without requiring a prior `dotnet restore`, so
the target satisfies its `_GeneratedEditorConfigHasItems=true` condition and writes:

```
obj/Debug/net10.0/Project.GeneratedMSBuildEditorConfig.editorconfig
```

Roslyn then reads that file via `ProjectInstanceReader.GetEditorConfigFiles()` (which
calls `executedProject.GetItems("EditorConfigFiles")` on the post-build project
instance) and populates `ProjectFileInfo.AnalyzerConfigDocuments`. These become
`project.AnalyzerConfigDocuments` in the Roslyn `Solution`. When
`GetSourceGeneratedDocumentsAsync()` is subsequently called, the `GeneratorDriver`
already has the correct `AnalyzerConfigOptions` and the Razor generator produces output.

This was verified empirically:
- Running `dotnet msbuild -t:Compile -p:DesignTimeBuild=true -p:SkipCompilerExecution=true`
  on the raw (non-restored) `aspnetProject` fixture **does produce** the editorconfig,
  with `_GeneratedEditorConfigShouldRun=true` confirmed in the verbose log.
- A subsequent `dotnet build --no-restore` confirms the source generator writes
  `.g.cs` files under
  `obj/Debug/net10.0/Microsoft.CodeAnalysis.Razor.Compiler/Microsoft.NET.Sdk.Razor.SourceGenerators.RazorSourceGenerator/`.

### Why `GetSourceGeneratedDocumentsAsync` returns 0 (actual root cause)

The editorconfig **is** present in `project.AnalyzerConfigDocuments` and the generator
**does** produce documents. The reason `sourceGeneratedDocuments count=0` was observed
is that `solutionGetRazorDocumentForPath` **never calls `GetSourceGeneratedDocumentsAsync`**
— it only searches `compilation.SyntaxTrees`. Under SDK ≤ 10.0.201 the Razor-generated
`.g.cs` files were ordinary `Document` objects included in `SyntaxTrees`; under SDK
10.0.300 they are `SourceGeneratedDocument` objects and are invisible to
`SyntaxTrees`. The function simply finds no match and returns `None`.

---

## Relevant APIs (Roslyn 5.3.0)

### Source generator execution (all `internal` in `Microsoft.CodeAnalysis.Workspaces.dll`)

```
namespace Microsoft.CodeAnalysis.Host

interface IWorkspaceConfigurationService
  property Options : WorkspaceConfigurationOptions   // read-only

record WorkspaceConfigurationOptions
  ctor(SourceGeneratorExecution: SourceGeneratorExecutionPreference,
       ValidateCompilationTrackerStates: bool)
  // Default = Automatic

enum SourceGeneratorExecutionPreference
  Automatic = 0   // generators run on every GetSourceGeneratedDocumentsAsync call
  Balanced  = 1   // generators only re-run when version map is bumped

sealed class DefaultWorkspaceConfigurationService : IWorkspaceConfigurationService
  ctor()
  property Options : WorkspaceConfigurationOptions  // getter-only, no setter, class sealed
```

`CanSkipRunningGeneratorsAsync` (internal, called by `AddExistingOrComputeNewGeneratorInfoAsync`)
is the actual gate. It consults `CreationPolicy`:
- `Create` → always run
- `CreateOnlyRequired` → only run generators where `IsRequiredGenerator` returns true
- `DoNotCreate` → never run

`IsRequiredGenerator` is hard-coded to return `true` only for
`"Microsoft.NET.Sdk.Razor.SourceGenerators.RazorSourceGenerator"`. So the Razor
generator will run in `CreateOnlyRequired` mode too — but only if the editorconfig is
present and the generator can configure itself.

### `SourceGeneratedDocument` (public since Roslyn 4.0)

```
class SourceGeneratedDocument : Document
  property HintName  : string   // e.g. "Views/Test/ViewFileWithErrors_cshtml.g.cs"
  property FilePath  : string   // full generated path (not on disk)
  method GetSyntaxTreeAsync(CancellationToken) : Task<SyntaxTree>
  method GetSemanticModelAsync(CancellationToken) : Task<SemanticModel>
```

The generated `SyntaxTree` contains `#line` directives mapping back to the `.cshtml`
file. Once the tree is available, the existing `GetMappedLineSpan()` position logic in
`solutionFindSymbolForRazorDocumentPath` works unchanged.

### `Solution.AddAnalyzerConfigDocument` (public)

```csharp
// Microsoft.CodeAnalysis.Solution
public Solution AddAnalyzerConfigDocument(
    DocumentId documentId,
    string name,
    SourceText text,
    IEnumerable<string>? folders = null,
    string? filePath = null)
```

Returns a **new immutable `Solution` snapshot** with the extra analyzer config document
added to the specified project. The `CompilationTracker` for that project is invalidated —
on the next `GetSourceGeneratedDocumentsAsync()` call the tracker rebuilds from scratch,
passing the new config to the `GeneratorDriver`. This is the intended public mechanism for
injecting analyzer configuration without touching the `Workspace` object itself.

Also available: `WithAnalyzerConfigDocumentText(DocumentId, SourceText)` — updates an
**existing** analyzer config document's text. Both trigger generator re-runs on the next
call; `AddAnalyzerConfigDocument` causes a full fork (no cached compilation state
reused), while `WithAnalyzerConfigDocumentText` can reuse the syntax phase.

### `SymbolFinder.FindReferencesAsync` — known gap with `SourceGeneratedDocument`

**`SymbolFinder.FindReferencesAsync(symbol, solution)` does NOT automatically search
`SourceGeneratedDocument` objects.** This is a confirmed Roslyn bug (#63375), present in
5.3.0. Internally the engine iterates `project.Documents` (regular documents only); it
does not call `GetSourceGeneratedDocumentsAsync()`.

The `IImmutableSet<Document>` overload does accept `SourceGeneratedDocument` (it is a
`Document` subclass), so references in generated documents **can** be found if callers
retrieve them explicitly and pass them in. **However**, this overload is a *restriction*
set: when non-null, only the given documents are searched. Regular project documents must
therefore be explicitly included alongside the source-generated ones:

```csharp
var allDocs = solution.Projects
    .SelectMany(p => p.Documents)                        // regular docs
    .Concat(await GetAllSourceGeneratedDocsAsync(...))   // generated docs
    .ToImmutableHashSet();

SymbolFinder.FindReferencesAsync(symbol, solution, allDocs, ct);
```

**Impact on csharp-ls:** `References.fs` calls the two-argument overload
`(symbol, solution)` — generated Razor documents are not searched. This was not a
problem before SDK 10.0.300 because generated files were regular `Document` objects.
The fix must explicitly retrieve the `SourceGeneratedDocument` objects and include them
alongside regular documents in the document set (see Part C).

### `Location.fromRoslynLocation` — `GetMappedLineSpan` fallback chain

The existing conversion uses `GetMappedLineSpan()` **first**, falling back to
`GetLineSpan()` only when the mapped path does not exist on disk:

```fsharp
// PRIMARY: GetMappedLineSpan() — follows #line directives → .cshtml path + position
if mappedLoc.IsValid && File.Exists mappedLoc.Path then Some mappedLoc
// FALLBACK: GetLineSpan() — raw position inside the .g.cs virtual path
```

For a `SourceGeneratedDocument` reference location: `GetMappedLineSpan()` returns the
`.cshtml` file path (from `#line` directives). Since the `.cshtml` file exists on disk,
`File.Exists` is true and the mapped location wins. Result: reference locations in
generated code are correctly reported as `.cshtml` positions **without any extra code**,
as long as `SymbolFinder` is given the generated documents to search.

---

## Fix Plan

The fix has two parts.

---

### ~~Part A — Inject the editorconfig~~ — DROPPED

> **Dropped.** Earlier investigation incorrectly concluded that
> `Project.GeneratedMSBuildEditorConfig.editorconfig` was absent from
> `project.AnalyzerConfigDocuments` and needed to be synthesised. Empirical testing and
> Roslyn source inspection both confirm this is wrong: MSBuildWorkspace's design-time
> build (`Compile`+`CoreCompile` with `SkipCompilerExecution=true`) triggers
> `GenerateMSBuildEditorConfigFile` automatically, the result lands in the `EditorConfigFiles`
> MSBuild item group, and `ProjectInstanceReader.GetEditorConfigFiles()` reads it into
> `ProjectFileInfo.AnalyzerConfigDocuments`. No `AddAnalyzerConfigDocument` injection is
> needed. Source generators work naturally once the code uses the right Roslyn API to
> retrieve them (Part B below).

---

### Part B — Replace `solutionGetRazorDocumentForPath` to use `GetSourceGeneratedDocumentsAsync`

Replace the body of `solutionGetRazorDocumentForPath` entirely. Remove the
`compilation.SyntaxTrees` loop.

#### HintName format

The Razor generator derives each document's `HintName` from the `TargetPath` metadata
in the editorconfig (the base64-decoded relative path of the `.cshtml` file), replacing
**both** directory separators **and** dots with `_`:

```
Views/Test/ViewFileWithErrors.cshtml → Views_Test_ViewFileWithErrors_cshtml.g.cs
Views/_ViewImports.cshtml            → Views__ViewImports_cshtml.g.cs
```

This is the same flat-underscore format that was previously called `altCshtmlPathTranslated`
and was thought to be SDK < 10.0.201 only. In practice it is the stable `HintName`
format across all SDK versions (the `SyntaxTree.FilePath` for on-disk files used
directory separators, but `SourceGeneratedDocument.HintName` always uses underscores).

The `cshtmlPathTranslated` pattern used in the old `SyntaxTrees` search (which kept
directory separators) was never the right format for `HintName` matching — it was
specific to how Roslyn constructed the virtual file path for the on-disk `.g.cs` files.

#### New implementation

```fsharp
// Razor HintName format: replace both '.' and directory separators with '_', append '.g.cs'
// e.g. Views/Test/ViewFileWithErrors.cshtml → Views_Test_ViewFileWithErrors_cshtml.g.cs
let cshtmlHintName =
    Path.GetRelativePath(projectBaseDir, cshtmlPath)
    |> _.Replace(".", "_")
    |> _.Replace(Path.DirectorySeparatorChar, '_')
    |> fun s -> s + ".g.cs"

let! ct = Async.CancellationToken
let! generatedDocs =
    project.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask

let razorGenDoc =
    generatedDocs
    |> Seq.tryFind (fun d -> d.HintName.EndsWith cshtmlHintName)

match razorGenDoc with
| None -> return None
| Some doc ->
    let! tree = doc.GetSyntaxTreeAsync(ct) |> Async.AwaitTask
    match tree |> Option.ofObj with
    | None -> return None
    | Some tree ->
        let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
        return compilation |> Option.ofObj |> Option.map (fun c -> (project, c, tree))
```

`project.GetCompilationAsync()` at this point already includes the generated trees
(the `CompilationTracker` ran the generator when `GetSourceGeneratedDocumentsAsync` was
called). `GetSemanticModel(tree)` on that compilation works correctly for symbol resolution.

### Part C — Fix `SymbolFinder` to include `SourceGeneratedDocument` objects

`SymbolFinder.FindReferencesAsync(symbol, solution)` does **not** search
`SourceGeneratedDocument` objects automatically (Roslyn bug #63375). The
`References.fs` handler calls the two-argument overload and therefore misses Razor
references entirely.

The four-argument overload `(symbol, solution, documents, ct)` accepts
`SourceGeneratedDocument` (it is a `Document` subclass), but it is a **restriction** set:
when non-null, only the provided documents are searched. Regular documents must therefore
be included alongside the source-generated ones:

```fsharp
let! sourceGenDocArrays =
    solution.Projects
    |> Seq.map (fun p ->
        p.GetSourceGeneratedDocumentsAsync(ct).AsTask() |> Async.AwaitTask)
    |> Async.Parallel

let allDocs =
    let regularDocs =
        solution.Projects |> Seq.collect _.Documents |> Seq.cast<Document>
    sourceGenDocArrays
    |> Seq.concat
    |> Seq.cast<Document>
    |> Seq.append regularDocs
    |> ImmutableHashSet.CreateRange

let! refs =
    SymbolFinder.FindReferencesAsync(symbol, solution, allDocs, ct)
    |> Async.AwaitTask
```

**`Location.fromRoslynLocation` already handles this correctly:** when `SymbolFinder`
returns a reference location inside a generated tree, `GetMappedLineSpan()` follows the
`#line` directives back to the `.cshtml` file path. Since the `.cshtml` file exists on
disk, `File.Exists` is true, and the mapped location wins — the LSP client receives a
`.cshtml` URI and position without any additional code.

### Drop compat with older SDK Razor paths

The new `solutionGetRazorDocumentForPath` drops two legacy paths:

1. **`compilation.SyntaxTrees` search** — the pre-10.0.300 path that found Razor output
   as ordinary `Document` objects written to `obj/` as `.g.cs` files.
2. **`altCshtmlPathTranslated`** — the flat-path alias (`Views_Test_Index_cshtml.g.cs`)
   used by SDK < 10.0.201, already flagged with a `// TODO: drop this later` comment.

The new implementation uses only `GetSourceGeneratedDocumentsAsync` + `HintName`.

### The `WorkspaceConfigurationServiceInterceptor` — already absent

During investigation a `WorkspaceConfigurationServiceInterceptor` was added to force
`SourceGeneratorExecution = Automatic`. It turned out unnecessary (MSBuildWorkspace
already defaults to `Automatic`) and was never committed.

### Files changed (complete list)

| File | Change |
|---|---|
| `src/…/Roslyn/Solution.fs` | (Part B) Replace `solutionGetRazorDocumentForPath` — use `GetSourceGeneratedDocumentsAsync` + `HintName` (underscore pattern) |
| `src/…/Handlers/References.fs` | (Part C) Collect regular + source-generated docs; pass explicit set to `FindReferencesAsync` |
| `tests/…/Fixtures/aspnetProject/global.json` | **New** — pins project SDK resolver to `10.0.1xx` band so MSBuild loads the compatible Razor DLL |

---

## Root Cause — Layer 3: Roslyn / SDK version coupling

### How source generator loading works in MSBuildWorkspace

`MSBuildWorkspace.OpenProjectAsync` runs a design-time build (`Compile` + `CoreCompile`
with `SkipCompilerExecution=true`). `ProjectInstanceReader` reads the resulting
`Analyzer` MSBuild items and stores them as `AnalyzerFileReference` objects in
`project.AnalyzerReferences`. Each `AnalyzerFileReference` points to a DLL path on disk
— for the Razor generator that path lives inside the installed .NET SDK:

```
/usr/local/share/dotnet/sdk/<SDK>/Sdks/Microsoft.NET.Sdk.Razor/source-generators/
    Microsoft.CodeAnalysis.Razor.Compiler.dll
```

When `GetSourceGeneratedDocumentsAsync()` is first called, Roslyn's
`SolutionCompilationState` calls `AnalyzerFileReference.GetGenerators(LanguageNames.CSharp)`.
This loads the DLL into an isolated `AssemblyLoadContext` and reflects over its types to
find classes annotated with `[Generator(LanguageNames.CSharp)]`. If the DLL cannot
resolve one of its own dependencies (e.g. a `Microsoft.CodeAnalysis` version that is not
present), `GetTypes()` throws a `ReflectionTypeLoadException`; Roslyn's loader catches
it silently and returns an empty generator list. The result:

- `GetGenerators` returns `[]`
- `ComputeHasSourceGeneratorsAsync` → `NoSourceGenerators`
- `HasRequiredGeneratorsAsync` → `false`
- `CanSkipRunningGeneratorsAsync` → `true` (skip)
- `GetSourceGeneratedDocumentsAsync` returns **0 documents**

No error is reported anywhere.

### The concrete version table

Each .NET SDK band ships a `Microsoft.CodeAnalysis.Razor.Compiler.dll` built against a
specific Roslyn version. The Roslyn packages that `csharp-ls` references via NuGet must
be **≥** that version (same or higher minor within the same major — .NET Core allows
loading a higher version when the same public key is present):

| SDK | Razor compiler needs Roslyn | csharp-ls Roslyn (NuGet) | Compatible? |
|---|---|---|---|
| 10.0.100 / 10.0.101 (GA) | `5.0.0.0` (pre-release `5.0.0-2.25461.22`) | `5.3.0` | **✓ yes** |
| 10.0.201 | `5.0.0.0` (same band) | `5.3.0` | **✓ yes** |
| 10.0.300 (preview) | `5.5.0.0` (pre-release `5.5.0-2.26118.1`) | `5.3.0` | **✗ no** |

### Which SDK MSBuildLocator actually picks

`initializeMSBuild` calls `MSBuildLocator.QueryVisualStudioInstances(Default)` and
takes `Seq.head`. On this machine `QueryVisualStudioInstances` returns SDKs in
descending version order; the head is **10.0.101** (not 10.0.300), because the process
itself runs under the SDK band selected by `global.json` (`"version": "10.0.100",
"rollForward": "minor"` → the latest patch in the 10.0.1xx feature band, i.e. 10.0.101).

Confirmed by `csharp-ls --diagnose` output:

```
MSBuildLocator: will register ".NET Core SDK", Version=10.0.101 as default instance
```

With SDK 10.0.101, the Razor DLL requires only Roslyn `5.0.0.0`, which is satisfied by
the `5.3.0` NuGet packages. `GetGenerators` returns the Razor generator successfully.

### The MSBuild engine SDK vs. project SDK — a critical distinction

`MSBuildLocator.RegisterInstance(vsInstance)` configures which **MSBuild engine
assemblies** are loaded into the process. It does **not** control how `Sdk="..."` entries
in project files are resolved.

When MSBuild evaluates a project that begins with `<Project Sdk="Microsoft.NET.Sdk.Web">`,
it invokes the `.NET SDK resolver` (`Microsoft.NET.Sdk.Resolver.dll`) to locate
`Microsoft.NET.Sdk.Web`. That resolver calls out to `dotnet` (or consults the runtime)
and selects the SDK by walking up the project's directory tree looking for a `global.json`.
If none is found, it falls back to the **latest installed SDK** regardless of which SDK
was registered via MSBuildLocator.

Consequence for the test suite:

| What | Value |
|---|---|
| MSBuildLocator registered engine | SDK 10.0.101 |
| Fixture temp dir parent chain | no `global.json` anywhere |
| SDK resolver falls back to | latest installed = **10.0.300** |
| Razor DLL added as Analyzer item | `/sdk/10.0.300/…/Microsoft.CodeAnalysis.Razor.Compiler.dll` |
| That DLL requires | Roslyn **5.5.0** |
| csharp-ls provides | Roslyn **5.3.0** |
| `GetGenerators()` result | **0** (ReflectionTypeLoadException swallowed) |

Confirmed by adding `eprintfn` with the `FullPath` property of each `AnalyzerReference`
and running with `activateFixtureWithLoggingEnabled`:

```
ANALYZER 'Microsoft.CodeAnalysis.Razor.Compiler'
  path=/usr/local/share/dotnet/sdk/10.0.300/Sdks/Microsoft.NET.Sdk.Razor/
        source-generators/Microsoft.CodeAnalysis.Razor.Compiler.dll
  -> 0 generators
```

### Fix: add `global.json` to the `aspnetProject` fixture

The `prepareTempTestDirFrom` copy filter already passes `global.json` files through.
Adding one to `tests/…/Fixtures/aspnetProject/global.json` with the same policy as the
workspace root pins the project SDK resolution for the fixture to the 10.0.1xx band:

```json
{
  "sdk": {
    "version": "10.0.100",
    "rollForward": "minor"
  }
}
```

When the fixture is copied to `/tmp/…`, the SDK resolver now finds this `global.json`
at the temp root, picks the latest 10.0.1xx SDK (10.0.101 on this machine), and the
Razor Analyzer item points to the 10.0.101 DLL (Roslyn 5.0.0 requirement) → compatible
with csharp-ls's 5.3.0.

### The Roslyn-from-SDK question

An alternative architecture would be to **load Roslyn assemblies from the running SDK**
instead of from NuGet, eliminating the version mismatch permanently. This is how
`roslyn-language-server` (OmniSharp's successor) works — it ships as part of the SDK
and references the Roslyn assemblies co-installed with it.

**Advantages:**
- Razor generator always runs, regardless of which SDK feature band is installed
- No manual version bump needed each time the SDK ships a new Roslyn pre-release

**Disadvantages and open questions:**
- `MSBuildWorkspace` (in `Microsoft.CodeAnalysis.Workspaces.MSBuild`) is a NuGet-only
  package; it is not present in the SDK directory. The only "Roslyn from SDK" path is
  the compiler + workspace dlls inside `sdk/<ver>/Roslyn/`, but those are the compiler
  host dlls, not the workspace API surface.
- Loading from SDK at runtime requires finding the SDK path, constructing an
  `AssemblyLoadContext`, and resolving all transitive Roslyn deps from that directory —
  complex and fragile.
- The NuGet packages are the stable, documented, and tested API surface for
  `MSBuildWorkspace`. The SDK-internal Roslyn DLLs are not guaranteed to match the NuGet
  API at the same version.
- Roslyn 5.5.0 (the version bundled with SDK 10.0.300) **has not been published to
  NuGet** (confirmed May 2025 at https://www.nuget.org/packages/Microsoft.CodeAnalysis).
  Pre-release Roslyn versions used internally by preview SDKs are not available as
  NuGet packages.

**Current conclusion:** Keep NuGet Roslyn packages. Track the SDK 10.0.300 / Roslyn
5.5.0 situation — if and when Roslyn 5.5.0 is published to NuGet, bump
`RoslynPackageVersion` in `Directory.Packages.props`. Until then, SDK 10.0.300 Razor
support is blocked by the NuGet/SDK version gap; SDK 10.0.1xx (GA band) works correctly.

The `global.json` in the workspace root pins the *build* and *test-runner* process to
the 10.0.1xx feature band. The `global.json` added to the `aspnetProject` fixture pins
the *project SDK resolver* (inside MSBuild) to the same band, closing the gap.

**What happens on machines that only have SDK 10.0.300?** The `rollForward: "minor"`
policy will step forward to 10.0.300 (since no 10.0.1xx is installed). Razor features
will fail silently — `GetGenerators()` returns 0 due to the Roslyn 5.5.0 mismatch. This
is an acceptable degradation until Roslyn 5.5.0 reaches NuGet. A future improvement
would be to detect this case and log a warning to the user.

---

## What Does NOT Change

| Area | Why |
|---|---|
| `WorkspaceFolder.fs` | Already calls through `solutionGetRazorDocumentForPath` |
| `DocumentHighlight.fs` | Uses a single-document scope; generated docs for the active file retrieved via same `solutionGetRazorDocumentForPath` path |
| Position mapping / `#line` logic | Generated tree has same `#line` directives as before; `GetMappedLineSpan` path in `Location.fromRoslynLocation` unchanged |
| `--features razor-support` flag | Unchanged |
| `SourceGeneratorTests.fs` | Unaffected; uses a different fixture with its own pre-build |
| `aspnetProject` fixture files | No `obj/` or editorconfig needs committing |
| Test fixture activation | All `aspnetProject` tests stay on plain `activateFixture`; no `prebuildAspnet` needed |

### Breaking changes

| Area | Change |
|---|---|
| `altCshtmlPathTranslated` compat | **Removed** — flat-path naming (`Views_Test_Index_cshtml.g.cs`) used by SDK < 10.0.201 is gone |
| `compilation.SyntaxTrees` search | **Removed** from `solutionGetRazorDocumentForPath` — the pre-10.0.300 on-disk `.g.cs` path is dropped entirely |
| SDK 10.0.300 Razor support | **Blocked** by Roslyn 5.5.0 not being on NuGet yet; SDK 10.0.1xx (GA band) works; silent degradation on 10.0.300 |

---

## Approaches Considered and Rejected

**Intercept `IWorkspaceConfigurationService` to force `Automatic`** — implemented and
confirmed to fire, but had no effect. `MSBuildWorkspace` already defaults to `Automatic`.
The real blocker was not the execution preference but using the wrong lookup API.

**Read `.g.cs` from disk** — files are not written to disk in SDK 10.0.300 as part of
the design-time build. The paths shown in full-build error messages are transient.

**Use `AdditionalDocuments`** — `.cshtml` files are there as raw markup, not C#; no
`#line` directives.

**Synthesise + inject editorconfig via `AddAnalyzerConfigDocument`** — the plan's
original Part A. Dropped after confirming that MSBuildWorkspace already populates
`project.AnalyzerConfigDocuments` from `EditorConfigFiles` items produced during its
own design-time build. Synthesising the config manually would be redundant and fragile.

**Pre-build fixture via `dotnet build`** — would work but introduces a `dotnet` CLI
dependency and awkward error-handling (the fixture has intentional build errors). Not
needed.

**Parallel `GeneratorDriver` compilation** — generated trees live outside the `Solution`;
`SymbolFinder.FindReferencesAsync` and `GetSourceGeneratedDocumentsAsync` are both blind
to them. Rejected.

**Commit `obj/` to the fixture** — editorconfig bakes in absolute `ProjectDir` paths;
not portable across machines.

**Lean on the new Razor-in-Roslyn APIs** — the `dotnet/razor` repo was archived and its
source merged into `dotnet/roslyn` under `src/Razor/` (see *[Appendix A](#appendix-a--razor-merger-into-dotnetroslyn)* below).
However, all Razor language-service types (cohosting, span mapping, document services,
EA types) are `internal` and gated behind `InternalsVisibleTo` partner keys. The only
public hook remains the `RazorSourceGenerator` itself as a standard `IIncrementalGenerator`.
No shortcut exists for an external LSP server. This situation may improve as Roslyn
stabilises its public LSP extension API (tracked in roslyn#68696), but not in 5.3.0.

---

## Investigation Timeline

| Step | Finding |
|---|---|
| Run full test suite | 14 failures, all cshtml/Razor |
| Focus on `testPullDiagnosticsWorkForRazorFiles` | Returns 0 diagnostics instead of 1 |
| Add `eprintfn` + enable logging | `SyntaxTrees count=8`, no `.g.cs`, `sourceGeneratedDocuments count=0` |
| Check `project.AdditionalDocuments` | Contains `.cshtml` files as raw markup |
| Inspect `project.AnalyzerReferences` | `Microsoft.CodeAnalysis.Razor.Compiler` IS loaded |
| Inspect Roslyn 5.3.0 internals via reflection | `IWorkspaceConfigurationService`, `SourceGeneratorExecutionPreference`, confirmed `Automatic` is the default |
| Implement `WorkspaceConfigurationServiceInterceptor` | Interceptor fires, but `GetSourceGeneratedDocumentsAsync` still returns 0 |
| Note that `SourceGeneratorTests.fs` passes | Source generators work — uses `prebuildGenerator` before server start |
| `dotnet restore aspnetProject` | No editorconfig produced |
| `dotnet build aspnetProject` | Produces `obj/Debug/net10.0/Project.GeneratedMSBuildEditorConfig.editorconfig` |
| Read editorconfig contents | Contains `build_property.RazorLangVersion`, `build_property.RootNamespace`, per-file `TargetPath` — everything the Razor generator needs |
| **Root cause confirmed** | Razor generator silently produces no output when its required `AnalyzerConfigOptions` are absent; the editorconfig that provides them is only created by `dotnet build`, which doesn't run before server load for `aspnetProject` |
| Research `CSharpGeneratorDriver` public API | Fully public; takes `generators`, `AdditionalText` list, `AnalyzerConfigOptionsProvider`, `ParseOptions` — no reflection needed |
| Research `AnalyzerConfigOptionsProvider` construction | `AnalyzerConfig.Parse` + `AnalyzerConfigSet.Create` are public; custom `AnalyzerConfigOptionsProvider` subclass wraps the result — no internal types needed |
| Research Razor merger | `dotnet/razor` archived; source merged into `dotnet/roslyn` under `src/Razor/`; all language-service types remain `internal` behind EA boundary; no new public API for csharp-ls in 5.3.0 |
| Research Razor cohosting | Cohosting suppresses `RazorSourceGenerator` output when `UseRazorCohostServer=true`; not relevant at 5.3.0 but a future concern |
| Research `solution.AddAnalyzerConfigDocument` | Public API; returns new immutable `Solution` snapshot with `CompilationTracker` invalidated; next `GetSourceGeneratedDocumentsAsync` call re-runs generators with new config |
| Research `SymbolFinder` + `SourceGeneratedDocument` | `FindReferencesAsync(symbol, solution)` does NOT search `SourceGeneratedDocument` objects — confirmed bug roslyn#63375; must pass them explicitly in document set |
| Research `Location.fromRoslynLocation` | Already calls `GetMappedLineSpan()` first; `.cshtml` path from `#line` directives passes `File.Exists` check → no extra code needed for reference location mapping |
| Intermediate direction | Part A: synthesise editorconfig, inject via `AddAnalyzerConfigDocument`; Part B: `GetSourceGeneratedDocumentsAsync` + `HintName`; Part C: `SourceGeneratedDocument` set to `FindReferencesAsync` |
| **Re-investigation: editorconfig claim** | Read Roslyn source (`ProjectBuildManager`, `ProjectInstanceReader`): MSBuildWorkspace runs `Compile`+`CoreCompile` → `GenerateMSBuildEditorConfigFile` runs automatically (BeforeTargets=CoreCompile) → adds to `EditorConfigFiles` item group → `GetEditorConfigFiles()` populates `ProjectFileInfo.AnalyzerConfigDocuments`. Empirically confirmed: design-time build on raw (non-restored) fixture produces the editorconfig; `_GeneratedEditorConfigShouldRun=true` in verbose log |
| **Part A dropped** | Editorconfig is naturally present in `project.AnalyzerConfigDocuments`. Root cause is purely that `solutionGetRazorDocumentForPath` looks in `compilation.SyntaxTrees` (empty for Razor in SDK 10.0.300) rather than `GetSourceGeneratedDocumentsAsync`. Only Parts B + C needed. |
| Implement Parts B + C | `solutionGetRazorDocumentForPath` rewritten to use `GetSourceGeneratedDocumentsAsync` + `HintName`; `References.fs` fixed to pass regular + source-generated docs to `FindReferencesAsync`. Build succeeds. |
| Tests still fail after B + C | `testPullDiagnosticsWorkForRazorFiles` still returns 0. `eprintfn` diagnostics added but not visible — test infra only forwards server stderr when `clientProfile.LoggingEnabled = true`; Razor tests use default profile. |
| Probe with standalone F# app | Written `MSBuildWorkspace` probe that opens the aspnetProject fixture. With no `global.json` in `/tmp`, `MSBuildLocator.RegisterDefaults()` picks SDK **10.0.300**. Result: `Microsoft.CodeAnalysis.Razor.Compiler → 0 generators` + `GetSourceGeneratedDocumentsAsync → 0`. |
| Diagnose zero generators | `Assembly.LoadFrom(razorDllPath)` + `asm.GetTypes()` throws `ReflectionTypeLoadException`: `Microsoft.CodeAnalysis 5.5.0.0` not found — manifest mismatch. The SDK 10.0.300 Razor DLL needs Roslyn **5.5.0** but csharp-ls has **5.3.0**. Roslyn 5.5.0 is not published on NuGet. |
| `csharp-ls --diagnose` | Confirms `MSBuildLocator` picks **10.0.101** (not 10.0.300) when run from this repo root (global.json `rollForward: minor` from 10.0.100 → latest 10.0.1xx patch). SDK 10.0.101 Razor DLL needs Roslyn 5.0.0 → compatible with 5.3.0. |
| Run test with `activateFixtureWithLoggingEnabled` | `HintName` pattern `Views/Test/ViewFileWithErrors_cshtml.g.cs` is correct. But `GetSourceGeneratedDocumentsAsync → 0 docs` confirmed. Razor DLL path logged: `/sdk/10.0.300/…/Microsoft.CodeAnalysis.Razor.Compiler.dll` with 0 generators — SDK 10.0.300 is being used |
| MSBuildLocator vs. SDK resolver distinction | MSBuildLocator registers the *engine* (10.0.101). The `.NET SDK resolver` inside MSBuild picks the project SDK via the directory-tree `global.json` walk. No `global.json` in the temp fixture path → resolver falls back to latest installed = **10.0.300**. The two SDKs are independent |
| Root cause confirmed | Fixture temp dir has no `global.json` → SDK resolver picks 10.0.300 → Razor DLL from 10.0.300 added as `Analyzer` item → DLL needs Roslyn 5.5.0 → `GetGenerators()` returns 0 → `GetSourceGeneratedDocumentsAsync` returns 0 |
| Fix identified | Add `global.json` (`"version": "10.0.100", "rollForward": "minor"`) to `aspnetProject` fixture root. File filter in `prepareTempTestDirFrom` already passes `global.json` through. SDK resolver finds it in the temp root, picks 10.0.101, Razor DLL from 10.0.101 needs only Roslyn 5.0.0 → compatible with 5.3.0 |
| After global.json: 1/4 Razor tests pass | `testReferenceWorksToRazorPageReferencedValue` passes (Part C fix, finds refs in source-generated docs). 3 still fail — all go through `solutionGetRazorDocumentForPath`. Generator now produces 5 docs (confirmed via logging) |
| HintName format wrong | Looking for `Views/Test/ViewFileWithErrors_cshtml.g.cs` (slash-based). Actual HintName: `Views_Test_ViewFileWithErrors_cshtml.g.cs` (all underscores). The generator replaces BOTH directory separators AND dots with `_` when computing the HintName from `TargetPath`. The slash-based format was only ever correct for `SyntaxTree.FilePath` virtual paths, never for `HintName`. |
| Fix HintName pattern | Replace both `.` AND `Path.DirectorySeparatorChar` with `_` when building the match pattern. Single unified formula, no `altCshtmlPathTranslated` alias needed. |
| All 4 Razor tests pass | `testPullDiagnosticsWorkForRazorFiles`, `testHoverWorksInRazorFile`, `testReferenceWorksToRazorPageReferencedValue`, `testReferenceWorksFromRazorPageReferencedValue` — all green |
| Full suite: 201/204 pass | 3 pre-existing failures (`testPullDiagnosticsIncludeEditorConfigAnalyzerRules`, `testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics`, `testPushDiagnosticsIncludeEditorConfigAnalyzerRules`) confirmed pre-existing via `git stash` baseline check — not introduced by this fix |
| Roslyn-from-SDK question | Analysed and rejected for now (NuGet packages are the stable API surface; SDK-internal Roslyn DLLs are not versioned to match NuGet; complex at runtime). Track Roslyn 5.5.0 NuGet release instead. |

---

_Investigation conducted: 2025-05, .NET SDK 10.0.300 / 10.0.101, Roslyn 5.3.0, csharp-ls on branch `fix-dotnet-10-300-breakage`_

Key tooling used:
- Standalone F# probe program (`MSBuildWorkspace.Create()` + `GetGenerators()` + `GetSourceGeneratedDocumentsAsync()`)
- `csharp-ls --diagnose` for MSBuildLocator SDK selection
- `activateFixtureWithLoggingEnabled` + `eprintfn` on `AnalyzerReference.FullPath` / `HintName` for in-process confirmation
- Roslyn source via GitHub API (`ProjectBuildManager.cs`, `ProjectInstanceReader.cs`, `SolutionCompilationState_SourceGenerators.cs`, `SolutionCompilationState.RegularCompilationTracker_Generators.cs`)

---

## Appendix A — Razor Merger into dotnet/roslyn

The `dotnet/razor` repository was archived in 2024–2025. Its source — including the
`RazorSourceGenerator`, the Razor compiler, and the Razor language server — was merged
into `dotnet/roslyn` under the `src/Razor/` subtree:

- Archive notice: <https://github.com/dotnet/razor/blob/archive/README.md>
- New location: <https://github.com/dotnet/roslyn/tree/main/src/Razor/>

### What this means for csharp-ls

The `RazorSourceGenerator` class (`Microsoft.NET.Sdk.Razor.SourceGenerators.RazorSourceGenerator`
in `Microsoft.CodeAnalysis.Razor.Compiler`) is now maintained at:
```
src/Razor/src/Microsoft.CodeAnalysis.Razor.Compiler/src/SourceGenerators/RazorSourceGenerator.cs
```
Its generator assembly is still delivered to projects as an SDK-bundled `AnalyzerReference`
— the move does not change how it is loaded or invoked.

### Roslyn External Access (EA) boundary

All Razor language-service types above the generator itself live behind an
`InternalsVisibleTo` "External Access" wall:

- `src/Tools/ExternalAccess/Razor/` — span mapping, document services, LSP cohosting hooks
- `src/Tools/ExternalAccess/RazorCompiler/` — previously a public EA package; deliberately
  **deleted** after it caused fragile deployment issues. The internals it exposed were moved
  to `Microsoft.CodeAnalysis.Experimental` (still internal) and the Razor generator now
  accesses them via reflection.

None of these are consumable from an external assembly. There is no stable public API for
Razor language services in Roslyn 5.3.0.

### Razor cohosting (not yet in 5.3.0)

Razor cohosting moves the Razor language service in-process with Roslyn (same process as
the C# LSP), eliminating the separate Razor LSP process. It is gated by
`build_property.UseRazorCohostServer` and first enabled in VS 2026 / 18.3.

When cohosting is active, `RazorSourceGenerator` checks `UseRazorCohostServer` and
**suppresses its own output** (the cohost server takes over generation). This is not
relevant for csharp-ls at 5.3.0, but worth noting: if a user's project sets
`UseRazorCohostServer=true` in future, the generator will produce nothing and our
`GeneratorDriver` approach will also produce nothing. A detection+warning path may be
needed then.

### Roslyn public LSP API stabilisation

Roslyn intends to make its LSP extension API public so that third-party servers can
extend C# LSP behaviour and build non-C# servers on top of CLaSP. This is tracked in
roslyn#68696. As of 5.3.0 the stabilisation is **not complete** — no reliance on it
is possible yet.

---

## Appendix B — Reference: Roslyn Source Code

When Roslyn internals are relevant (e.g. tracing how `MSBuildWorkspace` constructs
`AnalyzerConfigOptions`, how `CompilationTracker` invokes `GeneratorDriver`, or how
`CanSkipRunningGeneratorsAsync` decides whether to execute a generator), the full Roslyn
source is available on GitHub and can be searched or read directly during investigation:

- Main repo: <https://github.com/dotnet/roslyn>
- Workspace layer (compilation tracking, source-gen execution):
  `src/Workspaces/Core/Portable/` and `src/Workspaces/MSBuild/`
- `GeneratorDriver` and incremental generator host:
  `src/Compilers/Core/Portable/SourceGeneration/`
- MSBuild workspace project loading:
  `src/Workspaces/MSBuild/MSBuild/MSBuildProjectLoader.cs`
- Razor source generator (post-merger):
  `src/Razor/src/Microsoft.CodeAnalysis.Razor.Compiler/src/SourceGenerators/`
- Razor External Access layer:
  `src/Tools/ExternalAccess/Razor/`

Tag the search to the Roslyn version in use (`5.3.0` → git tag `v5.3.0` or the matching
`VS 17.x` branch) to avoid reading code that doesn't match the runtime behaviour.
The Razor subtree was merged after 5.3.0; for Razor generator internals, searching
`main` and then cross-checking against the archived `dotnet/razor` repo at the relevant
SDK version is the most reliable approach.
