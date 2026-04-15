# Test Performance Improvements

## Problem Statement

The test suite takes ~41 seconds to run, with the bulk of that time spent in unconditional
`Thread.Sleep` calls rather than waiting for observable conditions. The slowest tests are:

| Test | Duration |
|------|----------|
| `testPushDiagnosticsWork` | ~13 s |
| `testPushDiagnosticsIncludeEditorConfigAnalyzerRules` | ~10 s |
| `workspace diagnostics do not include diagnostics from source-generated files` | ~7 s |
| `testWorkspaceDiagnosticsIncludeAnalyzerDiagnostics` | ~7 s |
| `go-to-definition on a generated symbol returns a csharp:/<proj>/generated/ URI` | ~6 s |
| `testDidCloseNotificationWillRevertCshtmlFileToStateOnDisk` | ~6 s |
| `testWorkspaceDiagnosticsWorkWithStreaming` | ~5–6 s |
| Workspace diagnostics cluster (~15 tests) | ~3–4 s each |

---

## Root Cause

Three patterns account for nearly all the wasted time:

1. **Large unconditional sleeps** — `Thread.Sleep(8000)`, `Thread.Sleep(4000)`,
   `Thread.Sleep(1000)` scattered in diagnostic tests. These assume the server will have
   responded within a fixed window; on a fast machine they massively over-wait, and on a
   slow machine they are still racy.

2. **Repeated 250 ms sleeps after document mutations** — every `Change()` or `Save()`
   call in `DocumentSyncTests.fs` is followed by a flat `Thread.Sleep(250)` before the
   next `textDocument/diagnostic` pull (10 sleeps across two tests = ~2.5 s).

3. **Razor race-workaround sleeps** — `Thread.Sleep(250)` placed immediately after
   `client.Open(*.cshtml)` in six test files, all tagged `// TODO: work around race for
   Razor support`. These have no condition to check; they just hope the server is ready.

The infrastructure for condition-based waiting already exists:

- `waitUntilOrTimeout` in `Tooling.fs` — polls a predicate every 50 ms up to a timeout
- `WaitForProgressEnd` in `Tooling.fs` — polls for a `$/progress[end]` notification every 25 ms
- `waitUntil` in `JsonRpcTests.fs` — polls a condition every 50 ms with a deadline

`testPushDiagnosticsIncludeEditorConfigAnalyzerRules` already uses `waitUntilOrTimeout`
correctly and finishes faster as a result. The other tests just haven't been updated yet.

---

## Plan

### 1. Fix `testPushDiagnosticsWork` (saves ~10 s)

**File:** `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs`

Replace `Thread.Sleep(8000)` (line 48) and `Thread.Sleep(4000)` (line 79) with
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

Similarly for the second sleep — poll until the diagnostics map no longer contains the
file (or contains an empty list), indicating that the server has cleared them.

### 2. Fix `testWorkspaceDiagnosticsWorkWithStreaming` (saves ~1 s)

**File:** `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs`, line 214

Replace `Thread.Sleep(1000)` with a `waitUntilOrTimeout` or `WaitForProgressEnd` call
to wait until the workspace is ready before issuing the first `workspace/diagnostic`
request.

### 3. Fix `DocumentSyncTests.fs` 250 ms sleeps (saves ~2.5 s)

**File:** `tests/CSharpLanguageServer.Tests/DocumentSyncTests.fs`

Replace the 10 × `Thread.Sleep(250)` calls (5 in each of the two `testDidClose...`
tests) with condition-based waits. After a `Change()` or `Save()`, poll
`textDocument/diagnostic` in a loop until the response reflects the expected state
rather than sleeping blindly.

Alternatively, pull diagnostics in a retry loop and assert on the result, removing the
sleep entirely.

### 4. Fix Razor `Thread.Sleep(250)` TODOs (saves ~1.5 s across 6 tests)

**Files:**
- `tests/CSharpLanguageServer.Tests/DiagnosticTests.fs:151`
- `tests/CSharpLanguageServer.Tests/CompletionTests.fs:151`
- `tests/CSharpLanguageServer.Tests/DocumentHighlightTests.fs:44`
- `tests/CSharpLanguageServer.Tests/ReferenceTests.fs:158,260`
- `tests/CSharpLanguageServer.Tests/HoverTests.fs:76`

All are `Thread.Sleep(250)` immediately after `client.Open(*.cshtml)`. Replace with
`WaitForProgressEnd` (which already polls for a `$/progress[end]` event indicating
the server finished processing the open), or poll until the document appears in the
solution state.

The root cause of the race should also be investigated — if the server sends a progress
notification when Razor document loading completes, `WaitForProgressEnd` is a correct
and cheap fix.

### 5. (Minor) Reduce flush-guard sleeps in `JsonRpcTests.fs`

**File:** `tests/CSharpLanguageServer.Tests/JsonRpcTests.fs`

- Lines 849 and 1058: `Thread.Sleep(200)` used as unconditional race guards before
  checking actor state. These could be replaced with short `waitUntil` polls on the
  observable condition (e.g. the response channel having a value, or the transport being
  in the expected state).
- Line 75: `Thread.Sleep(100)` flush guard after `waitUntil` inside `waitForMessages`.
  Investigate whether this can be removed or replaced with a non-empty buffer check.

---

## Expected Outcome

| Change | Estimated saving |
|--------|-----------------|
| Fix `testPushDiagnosticsWork` sleeps | ~10 s |
| Fix `DocumentSyncTests.fs` 250 ms sleeps | ~2.5 s |
| Fix Razor race sleeps | ~1.5 s |
| Fix `testWorkspaceDiagnosticsWorkWithStreaming` sleep | ~1 s |
| Fix `JsonRpcTests.fs` race guards | ~0.5 s |
| **Total** | **~15 s** (~37% reduction) |

The workspace/pull-diagnostics tests (~3–5 s each) are not caused by sleeps — they
reflect real server work (loading the solution, computing diagnostics). Those can only
be improved by speeding up the server itself or sharing solution load across tests.
