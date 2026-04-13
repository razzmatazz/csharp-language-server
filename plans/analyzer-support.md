# Analyzer Support

**Tracking:** [#148 Adding Analyzer Support](https://github.com/razzmatazz/csharp-language-server/issues/148),
[spacemacs#17230 csharp lsp backend doesn't show errors from .editorconfig rules violations](https://github.com/syl20bnr/spacemacs/issues/17230)

---

## Problem Statement

Roslyn-based C# development relies heavily on **Roslyn analyzers** — NuGet packages and SDK-supplied
assemblies that inspect code and emit diagnostics at the `DiagnosticSeverity.Warning` / `Error` /
`Info` / `Hidden` level.  Two concrete pain points motivate this feature:

1. **IDE code-style rules & `.editorconfig` violations are silently dropped.**  Rules such as
   `IDE0032` (use auto-property), `IDE0040` (accessibility modifiers required), `IDE0051` (unused
   private member), etc. are delivered entirely through Roslyn's `ImmutableArray<DiagnosticAnalyzer>`
   pipeline — they are *not* part of the Roslyn compiler's built-in diagnostics.  Users working in
   Spacemacs/Emacs, Neovim, or any other editor using `csharp-ls` therefore see zero style warnings
   even when `.editorconfig` configures dozens of rules (as reported in spacemacs#17230).

2. **Third-party package analyzers are also dropped.**  Packages such as
   `Microsoft.CodeAnalysis.NetAnalyzers`, `StyleCop.Analyzers`, `SonarAnalyzer.CSharp`, etc. are
   referenced as `<PackageReference>` entries with `IncludeAssets="analyzers"`.  Their diagnostics
   (e.g. `CA1001`, `S1135`) are invisible in `csharp-ls` even though they appear in `dotnet build`
   output and in VS / VS Code.

### Root Cause

`MSBuildWorkspace` loads the solution/project and populates the Roslyn `Project.AnalyzerReferences`
collection from the `<Analyzer>` items in the project file.  However, when diagnostics are requested
(either via push diagnostics in `PushDiagnostics.fs` or pull diagnostics in `Handlers/Diagnostic.fs`)
the code calls:

```fsharp
// push path (PushDiagnostics.fs)
doc.GetSemanticModelAsync() |> … |> semanticModel.GetDiagnostics()

// pull path (Handlers/Diagnostic.fs)
semanticModel.GetDiagnostics()

// workspace pull path (Handlers/Diagnostic.fs)
compilation.GetDiagnostics()
```

All three call sites use **compiler diagnostics only**.  The analyzer pipeline is separate and must be
invoked explicitly through
`Microsoft.CodeAnalysis.Diagnostics.CompilationWithAnalyzers` / `AnalysisResult`.

---

## What Needs to Change

There are three largely independent work items:

| # | Item | Files touched |
|---|------|---------------|
| A | **Run analyzers in the diagnostic pipeline** | `Handlers/Diagnostic.fs`, `Runtime/PushDiagnostics.fs` |
| B | **DLL locking fix on Windows (shadow-copy loader)** | `Roslyn/WorkspaceServices.fs`, `Roslyn/Solution.fs` |
| C | **EditorConfig / code-style analyzers via compiler options** | `Roslyn/Solution.fs`, possibly `Types.fs` |

Item B is a prerequisite on Windows (the analyzer DLLs are locked, which blocks incremental builds),
but is not needed for correctness on Linux/macOS.  Item C ensures the full set of IDE/style
diagnostics is available to Roslyn in the first place; without it the IDE analyzers are absent from
`Project.AnalyzerReferences`.

---

## Item A — Run Analyzers in the Diagnostic Pipeline

### How `CompilationWithAnalyzers` works

```fsharp
open Microsoft.CodeAnalysis.Diagnostics

// Get the analyzers registered for the project
let analyzers: ImmutableArray<DiagnosticAnalyzer> =
    project.AnalyzerReferences
    |> Seq.collect (fun r -> r.GetAnalyzers(LanguageNames.CSharp))
    |> ImmutableArray.CreateRange

// Wrap the compilation
let compilationWithAnalyzers =
    compilation.WithAnalyzers(analyzers, AnalyzerOptions(project.AdditionalDocuments, …))

// Get all diagnostics (compiler + analyzer)
let! allDiagnostics =
    compilationWithAnalyzers.GetAllDiagnosticsAsync(cancellationToken)
    |> Async.AwaitTask

// Or per-document for the pull path
let! docDiagnostics =
    compilationWithAnalyzers.GetAnalyzerSemanticDiagnosticsAsync(semanticModel, …)
    |> Async.AwaitTask
```

The `AnalyzerOptions` constructor takes the project's `AdditionalDocuments` (which includes the
`.editorconfig` content as `AnalyzerConfigDocument`) so that IDE style rules can read `.editorconfig`
keys.

### Shared helper

Introduce a shared helper in `Diagnostics.fs` (or a new `Roslyn/Analyzers.fs`):

```fsharp
// Returns compiler diagnostics + all analyzer diagnostics for a compilation.
let getCompilationDiagnosticsWithAnalyzers
    (project: Project)
    (compilation: Compilation)
    (ct: CancellationToken)
    : Async<ImmutableArray<Microsoft.CodeAnalysis.Diagnostic>> =
  async {
    let analyzers =
        project.AnalyzerReferences
        |> Seq.collect (fun r -> r.GetAnalyzers(LanguageNames.CSharp))
        |> ImmutableArray.CreateRange

    if analyzers.IsEmpty then
        return compilation.GetDiagnostics(ct)
    else
        let opts = AnalyzerOptions(project.AdditionalDocuments)
        let cwa = compilation.WithAnalyzers(analyzers, opts, ct)
        return! cwa.GetAllDiagnosticsAsync(ct) |> Async.AwaitTask
  }

// Returns semantic diagnostics for a single document (compiler + analyzers).
let getDocumentDiagnosticsWithAnalyzers
    (project: Project)
    (semanticModel: SemanticModel)
    (ct: CancellationToken)
    : Async<ImmutableArray<Microsoft.CodeAnalysis.Diagnostic>> =
  async {
    let analyzers =
        project.AnalyzerReferences
        |> Seq.collect (fun r -> r.GetAnalyzers(LanguageNames.CSharp))
        |> ImmutableArray.CreateRange

    if analyzers.IsEmpty then
        return semanticModel.GetDiagnostics(cancellationToken = ct)
    else
        let opts = AnalyzerOptions(project.AdditionalDocuments)
        let cwa = semanticModel.Compilation.WithAnalyzers(analyzers, opts, ct)
        return!
            cwa.GetAnalyzerSemanticDiagnosticsAsync(semanticModel, filterSpan = null, ct)
            |> Async.AwaitTask
  }
```

> **Performance note:** `WithAnalyzers` runs analyzers incrementally; results for unchanged
> compilations are cached by Roslyn internally.  However, running all analyzers on every keystroke is
> expensive.  A safe starting point is to gate analyzer execution on a debounce already present in
> `PushDiagnosticsBacklogUpdate` and use the existing `CancellationToken` from
> `workspaceFolderTeardown` to abort stale runs.

### Call-site changes

**`Handlers/Diagnostic.fs` — `handle` (per-document pull):**

```fsharp
// Before
let! diagnostics = semanticModel.GetDiagnostics() |> async.Return

// After
let! diagnostics =
    getDocumentDiagnosticsWithAnalyzers project semanticModel context.CancellationToken
```

**`Handlers/Diagnostic.fs` — `getWorkspaceDiagnosticReports` (workspace pull):**

```fsharp
// Before
let diags = compilation.GetDiagnostics()

// After
let! diags =
    getCompilationDiagnosticsWithAnalyzers project compilation ct
    |> Async.StartAsTask
    |> fun t -> t.Result   // already inside a Task.Run / async context
```

**`Runtime/PushDiagnostics.fs` — `resolveDocumentDiagnostics`:**

```fsharp
// Before
let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
let diags = semanticModel.GetDiagnostics(cancellationToken = ct)

// After
let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask
let! diags = getDocumentDiagnosticsWithAnalyzers project semanticModel ct
```

---

## Item B — DLL Locking Fix (Shadow-Copy Loader)

> **Prerequisite for Windows only.**  On Linux/macOS file handles do not prevent concurrent writes,
> so this item does not block Item A on those platforms.

Described in detail in [`plans/source-generator-support.md`](source-generator-support.md).  The same
shadow-copy loader intercept that prevents source-generator DLL locking also prevents analyzer DLL
locking.  The two fixes share a single implementation in `WorkspaceServicesInterceptor`.

Briefly: intercept `IAnalyzerAssemblyLoaderProvider` in `WorkspaceServicesInterceptor.Intercept` and
return a proxy that wraps `DefaultAnalyzerAssemblyLoaderProvider` with
`ShadowCopyAnalyzerPathResolver`.

---

## Item C — EditorConfig / Code-Style Analyzers

IDE code-style rules (`IDE0xxx`) are implemented by the `Microsoft.CodeAnalysis.CSharp.Features`
assembly (already a dependency) as `DiagnosticAnalyzer` subclasses.  They are activated when:

1. The Roslyn `Project.CompilationOptions` has the correct `SpecificDiagnosticOptions` / severity
   overrides derived from `.editorconfig`.
2. The `.editorconfig` is surfaced as an `AnalyzerConfigDocument` in the `Project.AdditionalDocuments`
   collection (MSBuildWorkspace does this automatically when it finds `.editorconfig` files).
3. The analyzers themselves are loaded — they live in `Microsoft.CodeAnalysis.CSharp.Features` which
   is already referenced.

**Investigation needed:** confirm that `MSBuildWorkspace` with `CSharpLspHostServices` correctly
resolves `.editorconfig` into `AnalyzerConfigDocument` entries and that
`AnalyzerOptions(project.AdditionalDocuments)` passes them to the analyzer.

If `.editorconfig` is not surfaced automatically, a manual loading step is needed:
scan the directory tree upward from the project file to find `.editorconfig` files and pass them as
`AnalyzerConfigOptionsProvider` to `AnalyzerOptions`.  This is the same discovery logic that Roslyn
itself uses in `CSharpCommandLineParser`.

---

## Test Plan (Tests First)

All tests must be written **before** the implementation and used in TDD style to drive the work.

### New fixture: `projectWithEditorConfigAnalyzers`

Create `tests/CSharpLanguageServer.Tests/Fixtures/projectWithEditorConfigAnalyzers/` with:

```
projectWithEditorConfigAnalyzers/
├── projectWithEditorConfigAnalyzers.sln
└── Project/
    ├── Project.csproj
    ├── .editorconfig
    └── Class.cs
```

**`Project/Project.csproj`:**

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <!-- Enable IDE style rules at warning level -->
    <EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
    <AnalysisLevel>latest-recommended</AnalysisLevel>
  </PropertyGroup>
</Project>
```

**`Project/.editorconfig`:**

```ini
root = true

[*.cs]
# IDE0040: Add accessibility modifiers
dotnet_style_require_accessibility_modifiers = always:warning

# IDE0051: Remove unused private members
dotnet_diagnostic.IDE0051.severity = warning

# IDE0032: Use auto property
dotnet_style_prefer_auto_properties = true:warning
```

**`Project/Class.cs`** — deliberately triggers the three editorconfig rules:

```csharp
// IDE0040: missing 'private' on field
// IDE0051: unused private method
// IDE0032: backing field could be auto property
class MyClass
{
    int _value = 0;                 // IDE0040 (no accessibility modifier)

    int Value { get { return _value; } set { _value = value; } }  // IDE0032

    void UnusedMethod() { }        // IDE0051
}
```

### New test file: `AnalyzerTests.fs`

Location: `tests/CSharpLanguageServer.Tests/AnalyzerTests.fs`

Must be added to `CSharpLanguageServer.Tests.fsproj` **before** the existing tests that could share fixtures.

```fsharp
module CSharpLanguageServer.Tests.AnalyzerTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Tests.Tooling

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
type AnalyzerTests() =

    /// Pull diagnostics on a file with editorconfig style violations
    /// should include IDE0040, IDE0051, IDE0032 in addition to compiler diagnostics.
    [<Test>]
    member _.testPullDiagnosticsIncludeEditorConfigRules() =
        use client = activateFixture "projectWithEditorConfigAnalyzers"
        use classFile = client.Open("Project/Class.cs")

        let pullParams: DocumentDiagnosticParams =
            { TextDocument = { Uri = classFile.Uri }
              Identifier = None
              PreviousResultId = None
              WorkDoneToken = None
              PartialResultToken = None }

        let result: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", pullParams)

        match result with
        | Some(U2.C1 full) ->
            let codes =
                full.Items
                |> Array.choose (fun d -> d.Code |> Option.map (fun c -> c.ToString()))
                |> Set.ofArray
            Assert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040, got: {codes}")
            Assert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051, got: {codes}")
            Assert.IsTrue(codes.Contains("IDE0032"), $"Expected IDE0032, got: {codes}")
        | _ -> Assert.Fail("Expected a full document diagnostic report")

    /// Push diagnostics should also surface editorconfig violations.
    [<Test>]
    member _.testPushDiagnosticsIncludeEditorConfigRules() =
        use client = activateFixture "projectWithEditorConfigAnalyzers"
        use classFile = client.Open("Project/Class.cs")

        // Wait for push diagnostics (same budget as testPushDiagnosticsWork)
        System.Threading.Thread.Sleep(8000)

        let publishedDiags =
            client.GetPublishedDiagnostics(classFile.Uri)

        let codes =
            publishedDiags
            |> Array.choose (fun d -> d.Code |> Option.map (fun c -> c.ToString()))
            |> Set.ofArray

        Assert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040 in push diagnostics, got: {codes}")
        Assert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051 in push diagnostics, got: {codes}")

    /// Workspace-level pull diagnostics should include analyzer diagnostics.
    [<Test>]
    member _.testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics() =
        use client = activateFixture "projectWithEditorConfigAnalyzers"

        let wsParams: WorkspaceDiagnosticParams =
            { Identifier = None
              PreviousResultIds = [||]
              WorkDoneToken = None
              PartialResultToken = None }

        let report: WorkspaceDiagnosticReport option =
            client.Request("workspace/diagnostic", wsParams)

        match report with
        | Some r ->
            let allCodes =
                r.Items
                |> Array.collect (fun item ->
                    match item with
                    | U2.C1 full -> full.Items |> Array.choose (fun d -> d.Code |> Option.map string)
                    | _ -> [||])
                |> Set.ofArray
            Assert.IsTrue(allCodes.Contains("IDE0040"), $"Expected IDE0040, got: {allCodes}")
        | None -> Assert.Fail("Expected a workspace diagnostic report")

    /// Third-party NuGet analyzer diagnostics should appear.
    /// Uses the existing `projectWithEditorConfigAnalyzers` fixture extended with
    /// a NetAnalyzers package reference.
    [<Test>]
    member _.testNuGetAnalyzerDiagnosticsAreSurfaced() =
        // This test is a placeholder — the fixture needs a NuGet analyzer package
        // reference (e.g. Microsoft.CodeAnalysis.NetAnalyzers). Adding a NuGet restore
        // step to the fixture setup is outside the scope of the first iteration.
        // For now, assert that the pull diagnostic call does not crash and returns a
        // result (i.e., the analyzer pipeline is wired up and robust to analyzers
        // that are present).
        use client = activateFixture "projectWithEditorConfigAnalyzers"
        use classFile = client.Open("Project/Class.cs")

        let pullParams: DocumentDiagnosticParams =
            { TextDocument = { Uri = classFile.Uri }
              Identifier = None
              PreviousResultId = None
              WorkDoneToken = None
              PartialResultToken = None }

        let result: DocumentDiagnosticReport option =
            client.Request("textDocument/diagnostic", pullParams)

        Assert.IsNotNull(result, "Pull diagnostics must not crash when analyzers are present")
```

> **Note on `GetPublishedDiagnostics`:** if this helper doesn't exist in `Tooling.fs`, add a
> thin wrapper around the `textDocument/publishDiagnostics` notification log stored during the
> session.

### Additional integration test: `projectWithAnalyzerPackage` (future)

Once NuGet restore is supported in fixture setup (or a pre-restored fixture is committed), add a
dedicated fixture:

```
projectWithAnalyzerPackage/
└── Project/
    ├── Project.csproj          (references Microsoft.CodeAnalysis.NetAnalyzers)
    ├── .editorconfig            (sets CA1001 to warning)
    └── DisposableClass.cs      (class with IDisposable field but no Dispose → CA1001)
```

---

## Implementation Order

1. **Write tests** (this step is first, per TDD):
   - Create `Fixtures/projectWithEditorConfigAnalyzers/` with the files above.
   - Create `Tests/AnalyzerTests.fs` and add it to the test `.fsproj`.
   - Run `dotnet test --filter AnalyzerTests` — all tests should fail (red).

2. **Add `Roslyn/Analyzers.fs`** with `getCompilationDiagnosticsWithAnalyzers` and
   `getDocumentDiagnosticsWithAnalyzers` helpers.  Add to `.fsproj` after `Roslyn/Solution.fs`.

3. **Wire helpers into pull diagnostics** (`Handlers/Diagnostic.fs`):
   - Replace `semanticModel.GetDiagnostics()` with `getDocumentDiagnosticsWithAnalyzers`.
   - Replace `compilation.GetDiagnostics()` with `getCompilationDiagnosticsWithAnalyzers`.

4. **Wire helpers into push diagnostics** (`Runtime/PushDiagnostics.fs`):
   - Replace the `GetSemanticModelAsync` → `GetDiagnostics` chain with the shared helper.

5. **Verify Item C (EditorConfig surfacing):**
   - Run the new tests; if IDE0xxx codes are absent, investigate `AnalyzerConfigDocument` loading.
   - If missing, add manual `.editorconfig` discovery in `Roslyn/Solution.fs` and pass it to
     `AnalyzerOptions`.

6. **Item B — shadow-copy loader** (Windows, can be done in parallel with Items A/C):
   - Add `IAnalyzerAssemblyLoaderProvider` intercept in `WorkspaceServicesInterceptor`.
   - Add Windows-only integration test in `AnalyzerTests.fs` that builds the project while the
     server is running (asserts no file-lock error).

7. **Run full test suite** (`dotnet test`) — all tests green.

8. **Manual verification** with Spacemacs/Emacs using `lsp-mode` and the `testDiagnosticsWork`
   fixture extended with an `.editorconfig` that triggers `IDE0051`.

---

## Files to Create / Modify

| File | Action |
|------|--------|
| `src/CSharpLanguageServer/Roslyn/Analyzers.fs` | **Create** — shared diagnostic helpers |
| `src/CSharpLanguageServer/CSharpLanguageServer.fsproj` | **Modify** — add `Roslyn/Analyzers.fs` after `Roslyn/Solution.fs` |
| `src/CSharpLanguageServer/Handlers/Diagnostic.fs` | **Modify** — use analyzer helpers |
| `src/CSharpLanguageServer/Runtime/PushDiagnostics.fs` | **Modify** — use analyzer helpers |
| `src/CSharpLanguageServer/Roslyn/WorkspaceServices.fs` | **Modify (Item B)** — shadow-copy loader intercept |
| `src/CSharpLanguageServer/Roslyn/Solution.fs` | **Modify (Item C/B)** — editorconfig surfacing, shadow dir cleanup |
| `tests/CSharpLanguageServer.Tests/Fixtures/projectWithEditorConfigAnalyzers/` | **Create** — fixture |
| `tests/CSharpLanguageServer.Tests/AnalyzerTests.fs` | **Create** — integration tests |
| `tests/CSharpLanguageServer.Tests/CSharpLanguageServer.Tests.fsproj` | **Modify** — add `AnalyzerTests.fs` |

---

## Open Questions

1. **Performance budget:** Running `CompilationWithAnalyzers.GetAllDiagnosticsAsync` on large
   projects (hundreds of files, many analyzers) can be slow (2–10 s).  The existing debounce in
   `PushDiagnosticsBacklogUpdate` provides some relief.  Consider adding a separate
   `--no-analyzers` flag (or `csharp.analyzers = false`) as an escape hatch, similar to how
   `useMetadataUris` gates virtual URI emission.

2. **IDE analyzer availability:** The IDE analyzers live in
   `Microsoft.CodeAnalysis.CSharp.Features`.  When loaded from NuGet (not from the SDK), the
   versions may diverge from the runtime Roslyn version, causing assembly load failures.  The
   project currently references `Microsoft.CodeAnalysis.CSharp.Features 5.3.0` directly, so this
   should be fine for SDK-supplied analyzers but may surface for older project TFMs.

3. **`AnalyzerOptions` and `.editorconfig`:** Needs confirmation that
   `project.AdditionalDocuments` includes `AnalyzerConfigDocument` entries after
   `MSBuildWorkspace.OpenSolutionAsync`.  If not, a manual
   `AnalyzerConfigOptionsProvider` must be constructed from the `.editorconfig` files on disk.

4. **Severity filtering:** Some analyzers emit `Hidden` severity items (e.g. code refactoring
   suggestions).  The `diagnosticIsToBeListed` helper in `Diagnostic.fs` currently only filters
   `CS8019` on `.cshtml`.  A broader severity filter (e.g. suppress `Hidden` unless the client
   opt-in) may be warranted to avoid flooding the editor with low-signal hints.

5. **Cancellation:** `CompilationWithAnalyzers` honors a `CancellationToken` — the workspace-
   folder's `CancellationTokenSource` (bumped on `workspaceFolderTeardown`) should be threaded
   through to all analyzer calls to ensure stale runs abort promptly on reload.
