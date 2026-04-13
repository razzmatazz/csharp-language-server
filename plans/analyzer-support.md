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
// push path (PushDiagnostics.fs — resolveDocumentDiagnostics)
let! semanticModelMaybe = doc.GetSemanticModelAsync()
// …
let diagnostics =
    semanticModel.GetDiagnostics()
    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
    |> Seq.map fst
    |> Array.ofSeq

// pull path (Handlers/Diagnostic.fs — handle)
let diagnostics =
    semanticModel.GetDiagnostics()
    |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
    |> Seq.map fst
    |> Array.ofSeq

// workspace pull path (Handlers/Diagnostic.fs — getWorkspaceDiagnosticReports)
let! compilation = project.GetCompilationAsync(ct) |> Async.AwaitTask
// …
compilation.GetDiagnostics(ct)
|> Seq.filter (fun d -> …)
```

All three call sites use **compiler diagnostics only**.  None of them pass a `CancellationToken` in
the document-level paths (`semanticModel.GetDiagnostics()` is called without arguments); only the
workspace pull path threads `ct` via `let! ct = Async.CancellationToken`.  The push path in
`PushDiagnostics.fs` runs inside a `task { }` CE with no cancellation token at all.

The analyzer pipeline is separate and must be invoked explicitly through
`Microsoft.CodeAnalysis.Diagnostics.CompilationWithAnalyzers` / `AnalysisResult`.

> **Note:** The codebase currently has **zero references** to `CompilationWithAnalyzers`,
> `WithAnalyzers`, `AnalyzerOptions`, `AnalyzerReferences`, `GetAnalyzers`, or
> `AnalyzerConfigDocument` in any `.fs` source file.  Analyzer support is entirely absent.

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

Introduce a new module `Roslyn/Analyzers.fs`.  (`Diagnostics.fs` is **not** a shared utility — it
contains the `--diagnose` CLI command implementation and `LspClientStub`; it is the wrong place.)
Insert in `.fsproj` compile order after `Roslyn/Solution.fs`.

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
> compilations are cached by Roslyn internally.  Running all analyzers on large projects can add
> latency, but this is the same tradeoff made by VS / VS Code and is acceptable.  The existing
> debounce in `PushDiagnosticsBacklogUpdate` and the `CancellationToken` from
> `workspaceFolderTeardown` are sufficient to keep stale runs from piling up.

### Call-site changes

**`Handlers/Diagnostic.fs` — `handle` (per-document pull):**

The `handle` function receives `context: RequestContext` and `p: DocumentDiagnosticParams`.
It obtains the semantic model via `workspaceFolderDocumentSemanticModel`.  The change needs to also
obtain the `Project` object (for `AnalyzerReferences`) — this requires plumbing it from the
workspace-folder lookup.  There is **no explicit `CancellationToken`** available in the document
pull handler today; one must be obtained via `let! ct = Async.CancellationToken`.

```fsharp
// Before (Diagnostic.fs, handle, ~line 61)
let diagnostics =
    semanticModel.GetDiagnostics()
    |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
    |> Seq.map fst
    |> Array.ofSeq

// After
let! ct = Async.CancellationToken
let! allDiags = getDocumentDiagnosticsWithAnalyzers project semanticModel ct
let diagnostics =
    allDiags
    |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
    |> Seq.map fst
    |> Array.ofSeq
```

> **Plumbing note:** `workspaceFolderDocumentSemanticModel` currently returns
> `(SemanticModel * string) option`.  We also need the `Project` to extract
> `AnalyzerReferences`.  Options: (a) add a new helper that returns
> `(Project * SemanticModel * string) option`, or (b) look up the project separately via
> `workspaceFolderProjectForPath`.

**`Handlers/Diagnostic.fs` — `getWorkspaceDiagnosticReports` (workspace pull):**

The workspace pull path already has `project` and `ct` in scope.

```fsharp
// Before (~line 100)
compilation.GetDiagnostics(ct)
|> Seq.filter (fun d -> …)

// After
let! allDiags = getCompilationDiagnosticsWithAnalyzers project compilation ct
allDiags
|> Seq.filter (fun d -> …)
```

Note: the code inside `getWorkspaceDiagnosticReports` spawns one `Async.Start` per project.
The `getCompilationDiagnosticsWithAnalyzers` call is already async and returns
`Async<ImmutableArray<Diagnostic>>`, so it fits naturally.

**`Runtime/PushDiagnostics.fs` — `resolveDocumentDiagnostics`:**

This is the most delicate call site.  The push path runs inside a `task { }` CE, not an
`async { }` CE.  There is **no `CancellationToken`** in scope — `doc.GetSemanticModelAsync()` is
called without one.  Additionally, the push path does not have a `Project` in scope — it has only
the `Document` (obtained from `workspaceFolderDocument`).

```fsharp
// Before (~line 87)
let resolveDocumentDiagnostics () : Task = task {
    let! semanticModelMaybe = doc.GetSemanticModelAsync()
    // …
    let diagnostics =
        semanticModel.GetDiagnostics()
        |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
        |> Seq.map fst
        |> Array.ofSeq
    Ok(docUri, None, diagnostics) |> postResolution
}

// After — needs project + cancellation token threading
let resolveDocumentDiagnostics () : Task = task {
    let! semanticModelMaybe = doc.GetSemanticModelAsync()
    // …
    let! allDiags =
        getDocumentDiagnosticsWithAnalyzersTask project semanticModel ct
    let diagnostics =
        allDiags
        |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
        |> Seq.map fst
        |> Array.ofSeq
    Ok(docUri, None, diagnostics) |> postResolution
}
```

> **Threading issues to resolve:**
> 1. The `project` must be passed down to `processPendingPushDiagnostics` or obtained from
>    `doc.Project` (which is available on `Document` and returns the containing `Project`).
> 2. A `CancellationToken` should be plumbed through — ideally from the workspace folder's
>    `CancellationTokenSource` to ensure stale analyzer runs abort on reload.
> 3. The `task { }` CE needs a `Task`-returning overload of the helper (or `Async.StartAsTask`).

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

> **Naming note:** A `projectWithEditorConfig` fixture already exists, but it contains only
> **formatting rules** (`csharp_space_after_keywords_in_control_flow_statements`, etc.) — no
> diagnostic severity rules.  The new fixture must be distinct.

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

Must be added to `CSharpLanguageServer.Tests.fsproj` in the `<Compile>` list (e.g. after
`DiagnosticTests.fs`).

> **Style note:** The existing test files use **module-level `[<Test>]` `let` bindings**, not
> `[<TestFixture>]` class style.  `AnalyzerTests.fs` must follow this pattern.

> **Pull diagnostics capability:** `defaultClientCapabilities` sets
> `TextDocument.Diagnostic = None` and `Workspace.Diagnostics = None`, meaning pull diagnostics
> are **not** advertised by default.  Pull-diagnostic tests must use `activateFixtureExt` with a
> custom `LspClientProfile` that sets both capabilities, the same way `DiagnosticTests.fs` does.

> **Push diagnostics access:** There is **no** `GetPublishedDiagnostics` method on
> `LspTestClient`.  Push diagnostics are accessed via `client.GetState().PushDiagnostics`,
> which returns `Map<string, int option * Diagnostic[]>`.

```fsharp
module CSharpLanguageServer.Tests.AnalyzerTests

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types
open CSharpLanguageServer.Tests.Tooling

// Client profile with pull diagnostics enabled
let private analyzerClientProfile =
    { defaultClientProfile with
        ClientCapabilities =
            { defaultClientCapabilities with
                TextDocument =
                    Some { defaultClientCapabilities.TextDocument.Value with
                            Diagnostic = Some { DynamicRegistration = Some true
                                                 RelatedDocumentSupport = None } }
                Workspace =
                    Some { defaultClientCapabilities.Workspace.Value with
                            Diagnostics = Some { RefreshSupport = Some true } } } }

[<Test>]
let testPullDiagnosticsIncludeEditorConfigAnalyzerRules () =
    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" analyzerClientProfile emptyFixturePatch id
    use classFile = client.Open("Project/Class.cs")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    match report with
    | Some(U2.C1 report) ->
        let codes =
            report.Items
            |> Array.choose (fun d -> d.Code |> Option.map string)
            |> Set.ofArray
        Assert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040, got: {codes}")
        Assert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051, got: {codes}")
        Assert.IsTrue(codes.Contains("IDE0032"), $"Expected IDE0032, got: {codes}")
    | _ -> failwith "U2.C1 (full report) was expected"

[<Test>]
let testPushDiagnosticsIncludeEditorConfigAnalyzerRules () =
    use client = activateFixture "projectWithEditorConfigAnalyzers"
    use classFile = client.Open("Project/Class.cs")

    // Wait for push diagnostics — same budget as testPushDiagnosticsWork
    System.Threading.Thread.Sleep(8000)

    let state = client.GetState()
    let diag = state.PushDiagnostics |> Map.tryFind classFile.Uri
    Assert.IsTrue(diag.IsSome, "Expected push diagnostics for Class.cs")

    let _version, diagnosticList = diag.Value
    let codes =
        diagnosticList
        |> Array.choose (fun d -> d.Code |> Option.map string)
        |> Set.ofArray

    Assert.IsTrue(codes.Contains("IDE0040"), $"Expected IDE0040 in push diagnostics, got: {codes}")
    Assert.IsTrue(codes.Contains("IDE0051"), $"Expected IDE0051 in push diagnostics, got: {codes}")

[<Test>]
let testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics () =
    use client =
        activateFixtureExt "projectWithEditorConfigAnalyzers" analyzerClientProfile emptyFixturePatch id

    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let report: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    match report with
    | Some report ->
        let allCodes =
            report.Items
            |> Array.collect (fun item ->
                match item with
                | U2.C1 fullReport ->
                    fullReport.Items |> Array.choose (fun d -> d.Code |> Option.map string)
                | _ -> [||])
            |> Set.ofArray
        Assert.IsTrue(allCodes.Contains("IDE0040"), $"Expected IDE0040, got: {allCodes}")
    | _ -> failwith "'Some' was expected"

[<Test>]
let testAnalyzerPipelineDoesNotCrashWhenNoAnalyzersPresent () =
    // Verify that the analyzer pipeline is robust when a project has no analyzer references.
    // Uses the existing genericProject fixture which has no .editorconfig analyzer rules.
    use client =
        activateFixtureExt "genericProject" analyzerClientProfile emptyFixturePatch id
    use classFile = client.Open("Project/Class.cs")

    let diagnosticParams: DocumentDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          TextDocument = { Uri = classFile.Uri }
          Identifier = None
          PreviousResultId = None }

    let report: DocumentDiagnosticReport option =
        client.Request("textDocument/diagnostic", diagnosticParams)

    // Should get a report (possibly with zero items) — not a crash
    match report with
    | Some(U2.C1 _) -> ()
    | _ -> failwith "U2.C1 (full report) was expected"
```

> **`LspClientState.PushDiagnostics`** is a `Map<string, int option * Diagnostic[]>` keyed by
> document URI.  The `int option` is the document version.

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

### Pre-build callback

The `projectWithSourceGenerator` tests use a `prebuildGenerator` callback passed as the
`patchFixtureDir` argument to `activateFixtureExt`.  The `projectWithEditorConfigAnalyzers` fixture
may also need a pre-build step (e.g. `dotnet build` to trigger NuGet restore and populate
`obj/project.assets.json`) so that `MSBuildWorkspace` can resolve `AnalyzerReferences`.

If needed, add a similar callback:

```fsharp
let prebuildProject (fixtureDir: string) =
    let projectDir = Path.Combine(fixtureDir, "Project")
    let psi = ProcessStartInfo("dotnet", "build")
    psi.WorkingDirectory <- projectDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    let proc = Process.Start(psi)
    proc.WaitForExit()
    Assert.AreEqual(0, proc.ExitCode, "Pre-build of analyzer fixture failed")
```

### Server CLI flags in tests

The test harness currently starts the server with `--features razor-support` hardcoded in
`Tooling.fs`.  No changes to CLI flags are needed — analyzers run unconditionally.

---

## Implementation Order

1. **Write tests** (this step is first, per TDD):
   - Create `Fixtures/projectWithEditorConfigAnalyzers/` with the files specified above
     (`.sln`, `.csproj`, `.editorconfig`, `Class.cs`).
   - Pre-build the fixture (`dotnet build`) to ensure `obj/project.assets.json` exists; decide
     whether to commit the `obj/` artifacts or use a `patchFixtureDir` callback.
   - Create `AnalyzerTests.fs` and add `<Compile Include="AnalyzerTests.fs" />` to
     `CSharpLanguageServer.Tests.fsproj` (after `DiagnosticTests.fs`).
   - Run `dotnet test --filter AnalyzerTests` — all tests should fail (red).

2. **Add `Roslyn/Analyzers.fs`** with `getCompilationDiagnosticsWithAnalyzers` and
   `getDocumentDiagnosticsWithAnalyzers` helpers.  Add `<Compile Include="Roslyn/Analyzers.fs" />`
   to `CSharpLanguageServer.fsproj` after `Roslyn/Solution.fs`.  Provide both
   `Async`-returning and `Task`-returning overloads (the push path uses `task { }`, not
   `async { }`).

3. **Wire helpers into pull diagnostics** (`Handlers/Diagnostic.fs`):
   - In `handle`: obtain `CancellationToken` via `let! ct = Async.CancellationToken`; plumb the
     `Project` (either via `doc.Project` from the resolved `Document`, or a new helper); replace
     `semanticModel.GetDiagnostics()` with `getDocumentDiagnosticsWithAnalyzers`.
   - In `getWorkspaceDiagnosticReports`: replace `compilation.GetDiagnostics(ct)` with
     `getCompilationDiagnosticsWithAnalyzers project compilation ct`.

4. **Wire helpers into push diagnostics** (`Runtime/PushDiagnostics.fs`):
   - In `resolveDocumentDiagnostics` (`task { }` CE): use `doc.Project` to get the `Project`;
     thread the workspace folder's `CancellationToken`; replace
     `semanticModel.GetDiagnostics()` with the `Task`-returning analyzer helper.
   - In the Razor (`.cshtml`) path: same change (replace `semanticModel.GetDiagnostics()`).

5. **Verify Item C (EditorConfig surfacing):**
   - Run the new tests; if IDE0xxx codes are absent, investigate whether
     `project.AnalyzerConfigOptions` / `project.AdditionalDocuments` includes `.editorconfig`
     entries after `MSBuildWorkspace.OpenSolutionAsync`.
   - If missing, add manual `.editorconfig` discovery in `Roslyn/Solution.fs` and pass via
     `AnalyzerConfigOptionsProvider` to the `CompilationWithAnalyzersOptions` constructor.

6. **Item B — shadow-copy loader** (Windows, can be done in parallel with Items A/C):
   - Add `IAnalyzerAssemblyLoaderProvider` intercept in `WorkspaceServicesInterceptor` (shares
     implementation with `plans/source-generator-support.md`).
   - Add Windows-only integration test in `AnalyzerTests.fs` that builds the project while the
     server is running (asserts no file-lock error).

7. **Run full test suite** (`dotnet test`) — all tests green.

8. **Manual verification** with Spacemacs/Emacs using `lsp-mode` and a project with
   `.editorconfig` that triggers `IDE0040` / `IDE0051`.

---

## Files to Create / Modify

| File | Action |
|------|--------|
| `src/CSharpLanguageServer/Roslyn/Analyzers.fs` | **Create** — shared diagnostic helpers (Async + Task overloads) |
| `src/CSharpLanguageServer/CSharpLanguageServer.fsproj` | **Modify** — add `Roslyn/Analyzers.fs` after `Roslyn/Solution.fs` |
| `src/CSharpLanguageServer/Handlers/Diagnostic.fs` | **Modify** — use analyzer helpers; plumb `Project` and `CancellationToken` |
| `src/CSharpLanguageServer/Runtime/PushDiagnostics.fs` | **Modify** — use analyzer helpers; plumb `Project` via `doc.Project`; thread `CancellationToken` |
| `src/CSharpLanguageServer/Roslyn/WorkspaceServices.fs` | **Modify (Item B)** — shadow-copy loader intercept |
| `src/CSharpLanguageServer/Roslyn/Solution.fs` | **Modify (Item C/B)** — editorconfig surfacing investigation, shadow dir cleanup |
| `tests/.../Fixtures/projectWithEditorConfigAnalyzers/` | **Create** — fixture (`.sln`, `.csproj`, `.editorconfig`, `Class.cs`) |
| `tests/.../AnalyzerTests.fs` | **Create** — integration tests (module-level `let` bindings, not class-based) |
| `tests/.../CSharpLanguageServer.Tests.fsproj` | **Modify** — add `<Compile Include="AnalyzerTests.fs" />` |

---

## Open Questions

1. **Performance budget:** Running `CompilationWithAnalyzers.GetAllDiagnosticsAsync` on large
   projects (hundreds of files, many analyzers) can be slow (2–10 s).  The existing debounce in
   `PushDiagnosticsBacklogUpdate` and cancellation on workspace reload provide the main
   mitigation.  Analyzers run unconditionally — no feature flag or config setting is needed.

2. **IDE analyzer availability:** The IDE analyzers live in
   `Microsoft.CodeAnalysis.CSharp.Features`.  When loaded from NuGet (not from the SDK), the
   versions may diverge from the runtime Roslyn version, causing assembly load failures.  The
   project currently references `Microsoft.CodeAnalysis.CSharp.Features 5.3.0` (set via
   `RoslynPackageVersion` in `Directory.Packages.props`), so this should be fine for SDK-supplied
   analyzers but may surface for older project TFMs.

3. **`AnalyzerOptions` and `.editorconfig`:** Needs confirmation that
   `project.AdditionalDocuments` includes `AnalyzerConfigDocument` entries after
   `MSBuildWorkspace.OpenSolutionAsync`.  If not, a manual
   `AnalyzerConfigOptionsProvider` must be constructed from the `.editorconfig` files on disk.
   Note: the existing `projectWithEditorConfig` fixture uses formatting-only rules and does not
   exercise diagnostic severity overrides at all.

4. **Severity filtering:** Some analyzers emit `Hidden` severity items (e.g. code refactoring
   suggestions).  The `diagnosticIsToBeListed` helper in `Handlers/Diagnostic.fs` currently only
   filters `CS8019` on `.cshtml` URIs.  A broader severity filter (e.g. suppress `Hidden` unless
   the client opts in) may be warranted to avoid flooding the editor with low-signal hints.
   This ties into the existing `workspace/diagnostic` flood issue documented in
   `plans/workspace-diagnostics-flood.md`.

5. **Cancellation threading:** `CompilationWithAnalyzers` honors a `CancellationToken`.
   Currently, the document pull handler (`Diagnostic.fs` `handle`) has **no** cancellation token —
   it must add `let! ct = Async.CancellationToken`.  The push path (`PushDiagnostics.fs`
   `resolveDocumentDiagnostics`) runs in a `task { }` CE with no token at all.  The workspace
   folder's `CancellationTokenSource` (bumped on `workspaceFolderTeardown`) should be threaded
   through to all analyzer calls to ensure stale runs abort promptly on reload.

6. **`doc.Project` availability:** The push diagnostics path currently only has a `Document`
   object (from `workspaceFolderDocument`), not the `Project`.  Roslyn's `Document.Project`
   property provides the containing `Project` — verify this returns a `Project` with populated
   `AnalyzerReferences` and not a stripped-down view.

7. **Fixture pre-build / obj/ artifacts:** MSBuildWorkspace needs `obj/project.assets.json` to
   resolve `<PackageReference>` analyzer entries.  The `projectWithSourceGenerator` fixture uses
   a `prebuildGenerator` callback in `patchFixtureDir` to pre-build before the server starts.
   The analyzer fixture may need a similar approach.  Alternatively, commit the `obj/` directory
   (like `genericProject` does) — but this ties the fixture to a specific SDK version.

8. **`task { }` vs `async { }` in helpers:** The push diagnostics path uses `task { }`, while
   the pull diagnostics path uses `async { }`.  The shared helpers in `Roslyn/Analyzers.fs`
   should provide both `Async<_>` and `Task<_>` overloads to avoid unnecessary
   `Async.AwaitTask` / `Async.StartAsTask` round-trips.
