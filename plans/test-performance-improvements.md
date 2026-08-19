# Test Performance Improvements

## Problem Statement

The test suite takes ~47 seconds to run, with a significant portion spent in unconditional
`Thread.Sleep` calls rather than waiting for observable conditions.

### Original baseline measurements (2025-05-20, 211 tests, pre-fix)

Two runs were taken to distinguish stable sleep-bound times from variable real-work times.
Run 2 total wall time was higher (~97 s) due to inherent test randomness — specifically
`testReadyToReconfiguringToConfiguredPhaseTransition` took 67 s in run 2 vs 4.5 s in run 1
(a confirmed random occurrence, not machine load). This outlier was traced to
**`activeClientsSemaphore` starvation**: the semaphore was only held during `LoadSolution`,
so NUnit could schedule unlimited concurrent server processes, starving slower tests of CPU
slots. Total wall time is therefore not a reliable single-run metric; per-test numbers for
the sleep-bound tests are stable and meaningful.

| Test | Run 1 | Run 2 | Sleep contribution |
|------|-------|-------|--------------------|
| `testPushDiagnosticsWork` | 12.77 s | 12.83 s | `Thread.Sleep(8000)` + `Thread.Sleep(4000)` — **stable, all sleep** |
| `testPushDiagnosticsIncludeEditorConfigAnalyzerRules` | 11.82 s | 8.50 s | none — real analyzer work (varies) |
| `testPullDiagnosticsIncludeEditorConfigAnalyzerRules` | 11.71 s | 6.29 s | none — real analyzer work (varies) |
| `go-to-definition on generated symbol works without obj directory` | 8.93 s | 6.97 s | none — real work |
| `workspace diagnostics do not include diagnostics from source-generated files` | 8.63 s | 7.57 s | none — real work |
| `testWorkspacePhaseTransitionConfiguredLoadingReadyShuttingDown` | 7.83 s | 7.55 s | none — real work |
| `go-to-definition on a generated symbol returns a csharp:/<proj>/generated/ URI` | 7.79 s | 5.76 s | none — real work |
| `testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics` | 7.75 s | 9.72 s | none — real work |
| `testWorkspaceDiagnosticsWhileSolutionIsLoading` | 7.67 s | 7.91 s | none — real work |
| `test textDocument/documentHighlight works in .cshtml file` | 7.36 s | — | `Thread.Sleep(250)` Razor race |
| `testPullDiagnosticsWorkForRazorFiles` | 7.35 s | 5.33 s | `Thread.Sleep(250)` Razor race |
| `completion works in cshtml files` | 6.58 s | 6.14 s | `Thread.Sleep(250)` Razor race |
| `testDidCloseNotificationWillRevertCshtmlFileToStateOnDisk` | 5.97 s | 6.72 s | 5 × `Thread.Sleep(250)` — **stable** |
| `testHoverWorksInRazorFile` | 5.31 s | — | `Thread.Sleep(250)` Razor race |
| `testDidCloseNotificationWillRevertFileToStateOnDisk` | 5.30 s | 5.88 s | 5 × `Thread.Sleep(250)` — **stable** |
| `testWorkspaceDiagnosticsWorkWithStreaming` | 4.84 s | — | `Thread.Sleep(1000)` |
| Workspace diagnostics cluster (~15 tests) | ~3–4 s each | ~3–5 s each | none — real work |

**Run 1 total: 46.85 s** · **Run 2 total: ~97 s** (inflated by test randomness — see below) · parallelism = CPU count

### Pre-existing flaky tests (observed across all runs)

- **`testDidChangeConfigurationAloneTriggersSolutionReload`** — fails occasionally with
  `Request cancelled` on `$/csharp/debugInfo` during a workspace reload race.
- **`Some true: sent`** — fails occasionally with a `workspace/configuration` timing race.
- **`testReadyToReconfiguringToConfiguredPhaseTransition`** — occasionally takes 60+ s
  due to semaphore starvation (fixed, see below); also occasionally fails with
  `Request cancelled` on `$/csharp/debugInfo`.

These are pre-existing races unrelated to sleep-removal work.

---

## Changes Made

### Fix 1: `testPushDiagnosticsWork` — replace `Thread.Sleep` with `waitUntilOrTimeout` ✅

**File:** `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs`

Replaced `Thread.Sleep(8000)` and `Thread.Sleep(4000)` with `waitUntilOrTimeout` calls
polling `client.GetState().PushDiagnostics`.

**Result: 12.77 s → ~3.8 s** (avg of two post-fix runs)

### Fix 2: Extend `activeClientsSemaphore` to full test lifetime ✅

**File:** `tests/CSharpLanguageServer.Tests/Tooling.fs`

The semaphore was previously acquired and released inside `activateFixtureExt` — only
throttling concurrent solution loads, not concurrent test execution. NUnit's
`ParallelScope.All` could therefore schedule unlimited server processes simultaneously,
causing CPU starvation and the occasional 60+ s outlier on
`testReadyToReconfiguringToConfiguredPhaseTransition`.

**Fix:** Moved `activeClientsSemaphore.Wait()` to the top of the `LspTestClient`
constructor and `activeClientsSemaphore.Release()` to the bottom of `IDisposable.Dispose`,
so the slot is held for the full lifetime of each test. Removed the now-redundant
`Wait()`/`Release()` from `activateFixtureExt`.

**Result:** The 60+ s outlier is now structurally impossible. Total CPU time across all
tests dropped by ~40 s (sum of individual durations), and many real-work tests ran
noticeably faster due to reduced contention.

### Post-fix baseline measurements (2025-05-20, after both fixes)

Two runs taken (run 3 had 2 pre-existing flaky failures; run 4 all 211 passed):

| Test | Original | Post-fix R1 | Post-fix R2 | Avg | Change |
|------|----------|-------------|-------------|-----|--------|
| `testPushDiagnosticsWork` | 12.77 s | 3.72 s | 3.81 s | **3.77 s** | **−9.0 s** |
| `testPushDiagnosticsIncludeEditorConfigAnalyzerRules` | 11.82 s | 6.07 s | 5.44 s | **5.76 s** | −6.1 s |
| `testPullDiagnosticsIncludeEditorConfigAnalyzerRules` | 11.71 s | 8.45 s | 8.40 s | **8.42 s** | −3.3 s |
| `go-to-definition on generated symbol works without obj directory` | 8.93 s | 7.47 s | 5.70 s | 6.58 s | −2.4 s |
| `workspace diagnostics do not include diagnostics from source-generated files` | 8.63 s | 7.79 s | 5.51 s | 6.65 s | −2.0 s |
| `testWorkspaceDiagnosticsWhileSolutionIsLoading` | 7.67 s | 6.44 s | 6.76 s | 6.60 s | −1.1 s |
| `testWorkspacePhaseTransitionConfiguredLoadingReadyShuttingDown` | 7.83 s | 7.16 s | 7.46 s | 7.31 s | −0.5 s |
| `testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics` | 7.75 s | 7.83 s | 6.85 s | 7.34 s | −0.4 s |
| `test textDocument/documentHighlight works in .cshtml file` | 7.36 s | 5.79 s | 5.32 s | 5.55 s | −1.8 s |
| `testPullDiagnosticsWorkForRazorFiles` | 7.35 s | 4.86 s | 4.66 s | 4.76 s | −2.6 s |
| `completion works in cshtml files` | 6.58 s | 6.36 s | 6.10 s | 6.23 s | −0.4 s |
| `testDidCloseNotificationWillRevertCshtmlFileToStateOnDisk` | 5.97 s | 6.60 s | 6.47 s | 6.54 s | +0.6 s |
| `testDidCloseNotificationWillRevertFileToStateOnDisk` | 5.30 s | 5.15 s | 5.46 s | 5.31 s | +0.0 s |
| `testWorkspaceDiagnosticsWorkWithStreaming` | 4.84 s | 4.49 s | 4.46 s | 4.47 s | −0.4 s |
| `testReadyToReconfiguringToConfiguredPhaseTransition` | 4.48 s *(was up to 67 s)* | 3.70 s | 4.20 s | 3.95 s | stable |

**Post-fix wall times: R1 = 44 s (2 pre-existing flaky failures) · R2 = 40 s (211/211 passed)**

The `testDidClose...` tests remain unchanged at ~5–6 s — those sleeps are still present
and are the next target.

---

## Root Cause

Three patterns account for nearly all the wasted time:

1. **Large unconditional sleeps** — `Thread.Sleep(8000)`, `Thread.Sleep(4000)`,
   `Thread.Sleep(1000)` scattered in diagnostic tests. These assume the server will have
   responded within a fixed window; on a fast machine they massively over-wait, and on a
   slow machine they are still racy.

2. **Repeated 250 ms sleeps after document mutations** — every `Change()` or `Save()`
   call in `DocumentSyncTests.fs` is followed by a flat `Thread.Sleep(250)` before the
   next `textDocument/diagnostic` pull (10 sleeps across two tests = ~2.5 s actual cost).

3. **Razor race-workaround sleeps** — `Thread.Sleep(250)` placed immediately after
   `client.Open(*.cshtml)` in six tests across five files, all tagged
   `// TODO: work around race for Razor support`. These have no condition to check; they
   just hope the server is ready. The 250 ms itself is a small fraction of those tests'
   runtime — most time is real Razor loading work — so the practical gain here is modest.

The infrastructure for condition-based waiting already exists:

- `waitUntilOrTimeout` in `Tooling.fs` — polls a predicate every 50 ms up to a timeout
- `waitUntil` in `JsonRpcTests.fs` — polls a condition every 50 ms with a deadline

> **Note:** `WaitForProgressEnd` does **not** exist in the codebase — references to it in
> earlier drafts of this plan were incorrect. The equivalent is `waitUntilOrTimeout` plus
> a predicate over `client.GetState()` or the RPC log.

`testPushDiagnosticsIncludeEditorConfigAnalyzerRules` already uses `waitUntilOrTimeout`
correctly — its 11.8 s runtime is genuine analyzer work, not sleep.

---

## Plan

### 1. Fix `testPushDiagnosticsWork` (saves ~10 s) ✅

**File:** `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs`

Replace `Thread.Sleep(8000)` (line 47) and `Thread.Sleep(4000)` (line 75) with
`waitUntilOrTimeout` calls that poll `client.GetState().PushDiagnostics`, the same
pattern already used in `testPushDiagnosticsIncludeEditorConfigAnalyzerRules`.

```fsharp
// Before
Thread.Sleep(8000)

// After
waitUntilOrTimeout
    (TimeSpan.FromSeconds(30.0))
    (fun () -> client.GetState().PushDiagnostics |> Map.containsKey classFile.Uri)
    "Expected push diagnostics for Class.cs"
```

For the second sleep (after `classFile.Change("")`), poll until the diagnostics entry
for the file is present with an empty list:

```fsharp
// Before
Thread.Sleep(4000)

// After
waitUntilOrTimeout
    (TimeSpan.FromSeconds(30.0))
    (fun () ->
        client.GetState().PushDiagnostics
        |> Map.tryFind classFile.Uri
        |> Option.map (fun (_, diags) -> diags.Length = 0)
        |> Option.defaultValue false)
    "Expected push diagnostics to clear for Class.cs"
```

### 2. Fix `testWorkspaceDiagnosticsWorkWithStreaming` (saves ~1 s) ✅

**File:** `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs`

`Thread.Sleep(1000)` sat at the top of the test, immediately after `activateFixture`.
Deleted it — `activateFixture` already waits for the "Finished loading workspace" progress
event before returning, so the sleep was fully redundant. Test passes cleanly at ~3 s.

### 3. Fix `DocumentSyncTests.fs` 250 ms sleeps (saves ~2 s)

**File:** `tests/CSharpLanguageServer.Tests/DocumentSyncTests.fs`

Replace the 10 × `Thread.Sleep(250)` calls (5 in `testDidCloseNotificationWillRevertFileToStateOnDisk`,
5 in `testDidCloseNotificationWillRevertCshtmlFileToStateOnDisk`) with condition-based
waits. Each sleep precedes a `textDocument/diagnostic` pull that asserts a specific
diagnostic state. Since the pull is synchronous (request/response), the right fix is a
retry loop: issue the pull request, check the result, and if it doesn't match, sleep 50 ms
and retry — up to a timeout. This removes the blind sleep while remaining correct on slow
machines.

Alternatively, try deleting the sleeps entirely first. If the tests pass without them
(because the server processes `didChange`/`didSave`/`didClose` synchronously before
responding to the next request), no retry loop is needed.

### 4. Fix Razor `Thread.Sleep(250)` TODOs (saves ~0.5 s across 6 tests)

**Files (confirmed locations):**
- `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs:151`
- `tests/CSharpLanguageServer.Tests/CompletionTests.fs:151`
- `tests/CSharpLanguageServer.Tests/DocumentHighlightTests.fs:44`
- `tests/CSharpLanguageServer.Tests/ReferenceTests.fs:158, 260`
- `tests/CSharpLanguageServer.Tests/HoverTests.fs:76`

All are `Thread.Sleep(250)` immediately after `client.Open(*.cshtml)`. The practical
saving is small (~250 ms per test) because the bulk of those tests' runtime is real Razor
document loading, not the sleep.

Replace each sleep with a `waitUntilOrTimeout` that polls for a concrete ready signal —
e.g. a `$/progress` end notification for the Razor document, or the document appearing
in the solution state. First investigate what observable event the server emits when Razor
loading completes, then pick the appropriate predicate.

The underlying race condition should also be understood: if `activateFixture` / `client.Open`
can be extended to wait for Razor readiness, the per-test sleep would be unnecessary entirely.

### 5. (Minor) Reduce flush-guard sleeps in `JsonRpcTests.fs`

**File:** `tests/CSharpLanguageServer.Tests/JsonRpcTests.fs`

- Lines 849 and 1058: `Thread.Sleep(200)` used as unconditional race guards before
  checking actor state. These could be replaced with short `waitUntil` polls on the
  observable condition (e.g. the response channel having a value, or the transport being
  in the expected state).
- Line 75: `Thread.Sleep(100)` flush guard after `waitUntil` inside `waitForMessages`.
  Investigate whether this can be removed or replaced with a non-empty buffer check.

---

## Progress & Expected Outcome

| Change | Estimated saving | Status | Actual saving |
|--------|-----------------|--------|---------------|
| Fix `testPushDiagnosticsWork` sleeps | ~10 s | ✅ Done | **~9 s** |
| Extend semaphore to full test lifetime | n/a (correctness fix) | ✅ Done | **~7 s** wall time, eliminated 60 s outlier |
| Fix `testWorkspaceDiagnosticsWorkWithStreaming` sleep | ~1 s | ✅ Done | **~1 s** |
| Fix `DocumentSyncTests.fs` 250 ms sleeps | ~2 s | pending | — |
| Fix Razor race sleeps (×6 tests) | ~0.5 s | pending | — |
| Fix `JsonRpcTests.fs` race guards | ~0.5 s | pending | — |
| **Total remaining** | **~3 s** | | |

**Baseline: 46.85 s → current best: 40 s** (after two fixes, target ~36 s)

The analyzer tests (`testPushDiagnosticsIncludeEditorConfigAnalyzerRules`,
`testPullDiagnosticsIncludeEditorConfigAnalyzerRules`) and the workspace/source-generator
cluster (7–9 s each) are not caused by sleeps — they reflect real server work (loading the
solution, running analyzers, compiling). Those can only be improved by speeding up the
server itself or sharing solution state across tests.

---

## NUnit 4 upgrade (2026-08-19)

Bumped `NUnit` 3.14.0 → **4.6.1** and `NUnit3TestAdapter` 4.6.0 → **6.2.0** in
`Directory.Packages.props`.

NUnit 4 moved the classic `Assert.AreEqual`/`IsTrue`/`IsFalse`/`Fail`/etc. (and
`StringAssert`/`CollectionAssert`) out of `NUnit.Framework` into
`NUnit.Framework.Legacy.ClassicAssert`/`StringAssert`/`CollectionAssert`. (NUnit 4.5+
brings classic asserts back into the `Assert` class too, but only via a C# 14
extension-member mechanism that isn't visible from F# — so that shortcut doesn't apply
here.) Since rewriting ~700 assertions across 33 files to the `Assert.That(x, Is.EqualTo
y)` constraint model was out of scope for this change, the mechanical fix was:
- Add `open NUnit.Framework.Legacy` next to each file's `open NUnit.Framework`.
- Rename all classic `Assert.<Method>(...)` call sites to `ClassicAssert.<Method>(...)`
  (`Assert.That(...)`/`Assert.Multiple(...)` were left untouched — those stay on the
  modern `Assert` class).
- Two call sites (`RequestSchedulingTests.fs`, `ProgressReporterTests.fs`) needed an
  explicit `System.Action`/`System.Func<Task>` wrapper around a lambda passed to
  `ClassicAssert.DoesNotThrow(Async)`, since NUnit 4 added a non-obsolete delegate overload
  alongside the old one and F#'s lambda type inference can't disambiguate between them.

Full suite (292 tests) passes after the upgrade.

---

## Round 2 (2026-08-19): shared read-only fixtures for InlayHint/FoldingRange tests

### Problem

`InlayHintTests.fs` alone called `activateFixture "genericProject"` 54 times — once per
`[<Test>]` — each spinning up a brand-new `CSharpLanguageServer` process and reloading the
whole solution (~2 s of `textDocument/didOpen` + MSBuildWorkspace load per test), just to
issue a single read-only `textDocument/inlayHint` request against the same
`Project/InlayHintTest.cs` file every time. `FoldingRangeTests.fs` had the same shape (12
activations, 11 against the same file). Across the two files that's 65 redundant
server-process + solution-load cycles.

### Fix: `SharedReadOnlyFixture` base class (`Tooling.fs`)

Added a class-based NUnit fixture base (`SharedReadOnlyFixture`) that:
- Activates the client/solution and opens the document **once**, in `[<OneTimeSetUp>]`.
- Exposes the shared `Client`/`Doc` to every `[<Test>]` member in the fixture.
- Relies on NUnit's default "SingleInstance" fixture lifecycle (one instance of the class
  for the whole fixture) plus NUnit's documented behavior of calling `Dispose()`
  automatically once, after the last test in the fixture has run, when the fixture class
  implements `IDisposable` — **no explicit `[<OneTimeTearDown>]` needed**.
- Is explicitly scoped to **read-only** tests (no `Change`/`Save`/mutation) since the
  client/document are shared, mutable state reused by every test method.

`InlayHintTests.fs` and `FoldingRangeTests.fs` were mechanically rewritten from top-level
`[<Test>] let ``name`` () = use client = activateFixture ...` functions into instance
members of a `[<TestFixture>] type ...() = inherit SharedReadOnlyFixture(...)`, replacing
the two `use client = ...` / `use doc = ...` lines with `let client = this.Client` /
`let doc = this.Doc`. `FoldingRangeTests.fs` has one outlier test that opens a different
file (`Project/Class.cs`); it was left as a standalone top-level `[<Test>] let` function
since converting a single test wasn't worth a second fixture class.

### Results

| Suite | Before | After | Speedup |
|-------|--------|-------|---------|
| `InlayHintTests.fs` (54 tests) | 24.95 s | **~2 s** | ~12x |
| `FoldingRangeTests.fs` (12 tests) | ~13 s (est., 1 activation/test) | **~1 s** | ~13x |
| Full suite (292 tests) | 64 s | **42–44 s** | ~1.5x |

This pattern generalizes to any other file with the "many `[<Test>]`s against one
read-only file" shape (e.g. `DocumentSymbolTests.fs`, `SemanticTokenTests.fs`,
`ImplementationTests.fs`, `CSharpMetadataTests.fs`) — those weren't converted in this
round since their per-file win is much smaller (a handful of tests each vs. InlayHint's
54), but the same `SharedReadOnlyFixture` base is available for them.
