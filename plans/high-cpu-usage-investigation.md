# High CPU Usage Investigation

This document records a live profiling session against a running `csharp-ls` instance
(inside VS Code, workspace pointed at a large multi-project solution) to identify the
source of sustained high CPU usage, and proposes fixes. No code changes have been made
yet — this is the investigation + proposal.

---

## Methodology (reproducible)

### 1. Find the running server process

```
ps aux | grep -i csharp-ls
```

### 2. Sample live CPU usage independently (sanity check, no profiler needed)

```
for i in $(seq 1 6); do ps -p <PID> -o pid,%cpu,%mem,etime,time,stat; sleep 5; done
```

### 3. Capture a CPU/thread-time trace with `dotnet-trace`

`dotnet-trace` is a global .NET tool (`dotnet tool install -g dotnet-trace`).

**Important (macOS/Linux difference):** the `cpu-sampling` profile only applies to
`dotnet-trace collect-linux` (kernel perf events). On macOS, use the
`dotnet-sampled-thread-time` profile instead, which uses the in-process
`Microsoft-DotNETCore-SampleProfiler` EventPipe provider (~100 Hz managed stack
sampling) — this works cross-platform via `dotnet-trace collect`:

```
dotnet-trace collect -p <PID> \
  --profile dotnet-sampled-thread-time \
  --format speedscope \
  --duration 00:00:01:00 \
  -o ~/csharp-ls-cpu-trace.nettrace
```

This produces `~/csharp-ls-cpu-trace.nettrace` (raw) and
`~/csharp-ls-cpu-trace.speedscope.json` (for https://speedscope.app or local analysis).
Run `dotnet-trace list-profiles` to see all available profiles (`gc-verbose`,
`database`, etc.) for more targeted sessions.

### 4. Analyze the speedscope JSON

The speedscope "evented" format encodes each thread as open (`"O"`)/close (`"C"`)
events referencing shared frame indices. A short Python script (stack simulation) was
used to compute inclusive ("total") time per frame and self time per synthetic
category, aggregated across all 40 sampled threads over the 60 s window.

### 5. Cross-reference with the RPC log

`~/csharp-ls-rpc.log` (`--rpclog`) records every JSON-RPC message. It was grepped for
`workspace/diagnostic` request/response volume and `"kind":"full"` vs
`"kind":"unchanged"` diagnostic report counts.

---

## Findings

### 1. CPU usage is genuinely, persistently high

`ps` sampling over a ~30 s window (during light editor use, not a deliberate stress
test) showed the server continuously in the `R` (running) state, pegging **1–3 CPU
cores**:

```
%CPU   STAT
213.5  R
318.8  R
183.1  R
109.4  R
186.5  R
181.1  R
```

This is not a brief spike — it is sustained across every 5 s sample.

### 2. The RPC log shows the diagnostics-flood mitigation is only partially effective

`plans/workspace-diagnostics-flood.md` previously diagnosed a `workspace/diagnostic`
polling flood and landed two fixes (per-project `resultId` + shared CS8019 filter),
which are confirmed present in `Handlers/Diagnostic.fs` today. They worked — the RPC
log for this session is 46 MB / 62 k lines, not the 400 MB / 14 M lines described in
that plan.

But the fix only reduced the **volume of bytes on the wire**, not the **amount of Roslyn
recompute work**. Counting this session's log:

| Metric | Count |
|---|---|
| `workspace/diagnostic` requests | 124 |
| `"kind":"full"` diagnostic reports emitted | 10,130 |
| `"kind":"unchanged"` diagnostic reports emitted | 53,938 |

~82 "full" (i.e. freshly recomputed) document reports per poll, on average, for the
life of the session.

### 3. Root cause: `resultId` cache invalidation is project-scoped, not document-scoped

`projectResultId` (`Handlers/Diagnostic.fs:59`) is:

```fsharp
let private projectResultId (analyzersEnabled: bool) (project: Microsoft.CodeAnalysis.Project) = async {
    let! version = project.GetDependentVersionAsync(ct) |> Async.AwaitTask
    return sprintf "%s/%b" (string version) analyzersEnabled
}
```

`Project.GetDependentVersionAsync` changes whenever **that project or any project it
transitively depends on** changes. `generateProjectDiagnosticReports'` then treats a
project as unchanged only if **every** document the client already knows about in that
project matches this single id:

```fsharp
let clientHasCurrentProjectVersion =
    clientKnownResultsForProject.Length > 0
    && clientKnownResultsForProject |> Array.forall (fun (_, knownId) -> knownId = resultId)
```

If it doesn't match, the whole project is recompiled from scratch
(`project.GetCompilationAsync` → `compilation.GetDiagnostics()`, or
`getCompilationDiagnosticsWithAnalyzers` if analyzers are enabled) and **every**
document in that project is re-emitted as a `"full"` report — even documents whose
actual diagnostics did not change.

The traced workspace is a large, deeply interdependent solution (dozens of
inter-referencing projects, per the URIs seen in the log). A single edit in a
low-level/foundation project invalidates the dependent version for every project
downstream of it. `getWorkspaceDiagnosticReports`
then kicks off recompilation for **all** projects in the solution in parallel on every
subsequent poll (`for project in solutionProjects do Async.Start(...)`), regardless of
which file is actually open/being edited:

```fsharp
for project in solutionProjects do
    Async.Start(generateProjectDiagnosticReports wf project)
```

VS Code re-polls `workspace/diagnostic` on its own schedule (this is the "loop" from
the prior flood investigation) for the lifetime of the session, so this cascade repeats
continuously, not just right after an edit.

### 4. CPU trace corroborates: identifiable hot frames are Roslyn's full compile/bind pipeline

Where the sampler resolved managed frames, the only application-level (non-runtime,
non-thread-pool) methods that showed up with measurable inclusive time were:

- `Microsoft.CodeAnalysis.CSharp.Binder.BindStatement`
- `Microsoft.CodeAnalysis.CSharp.Binder.BindExpressionInternal` / `BindInvocationExpression`
- `Microsoft.CodeAnalysis.CSharp.Binder.BindBlock`
- `Microsoft.CodeAnalysis.CSharp.MethodCompiler.CompileMethod` / `CompileNamedType`
- `Microsoft.CodeAnalysis.CSharp.MethodCompiler.BindMethodBody`
- `Microsoft.CodeAnalysis.CSharp.CSharpCompilation.GetDiagnostics`
- `Microsoft.CodeAnalysis.FindSymbols.FindReferencesSearchEngine` / `DependentTypeFinder` / `SymbolFinder` frames (from the find-references/hover interaction during the capture)

No `CSharpLanguageServer.*`, `Newtonsoft.Json`, or JSON-RPC transport frames appeared
with any measurable weight — i.e. the server's own message-handling/serialization code
is not the bottleneck; the cost is squarely inside Roslyn's semantic binding and
method-body compilation, which is exactly the work `Compilation.GetDiagnostics()`
triggers.

**Caveat:** on this macOS/arm64 + .NET 10 setup, the large majority of samples
(88.5% `UNMANAGED_CODE_TIME`, 11.5% `CPU_TIME`) landed in two generic/unresolved
buckets rather than named managed frames — a known limitation of EventPipe stack
walking on macOS for JIT'd code without full symbol/unwind info. So the *relative*
weighting between the named Roslyn methods above should not be over-interpreted, but
their mere presence (and the total absence of any other named application code) is
still meaningful signal: it confirms full Roslyn compilation is actively running during
normal editor use, not just occasionally.

### 5. Confirmed: analyzers were OFF for the entire traced/measured window

Grepping the RPC log for `"analyzersEnabled"` shows the setting toggled from `false`
(the `initialize` response, log line 8) to `true` only at log line 144,336 — well after
both the `ps` CPU sampling and the `dotnet-trace` capture, which were taken while the
log was still at 62,045 lines / 46 MB. **Analyzers were disabled the entire time this
data was collected.**

This is an important correction to the initial read of the trace: the `Binder.*` /
`MethodCompiler.*` activity did **not** come from `CompilationWithAnalyzers`
(`getCompilationDiagnosticsWithAnalyzers` / `getDocumentDiagnosticsWithAnalyzers` in
`Roslyn/Analyzers.fs` are only reached when `analyzersEnabled = true`). It came purely
from the plain path: `compilation.GetDiagnostics()` /
`semanticModel.GetDiagnostics()`. In other words, **this is not an "analyzers are
expensive" problem — the baseline, analyzer-free compiler-diagnostics pipeline is
itself expensive enough, at this solution's size and dependency depth, to peg multiple
cores continuously when it's re-triggered on every poll for every invalidated project.**

---

## Conclusion

The dominant CPU cost is **repeated, solution-wide Roslyn recompilation driven by
`workspace/diagnostic` polling**, amplified by project-granularity (not
document-granularity) change detection in `projectResultId`. This is a continuation of
the issue in `plans/workspace-diagnostics-flood.md`: that plan's fixes stopped the
*network flood*, but the underlying *recompute* cascade — one edit invalidating and
triggering a full rebuild of every dependent project, on every poll, for the whole
session — was not addressed and remains the primary CPU sink.

Since analyzers were confirmed off throughout data collection (§5 above), this cost is
entirely attributable to the non-analyzer `Compilation.GetDiagnostics()` path. There is
no "turn analyzers off" escape hatch available here — that's already the state — which
raises the priority of fixing the invalidation granularity itself (fixes #2–#4 below)
over anything analyzer-specific.

---

## Related upstream work: PR [#404](https://github.com/razzmatazz/csharp-language-server/pull/404)

While drafting this plan, [PR #404](https://github.com/razzmatazz/csharp-language-server/pull/404)
("perf(diagnostics): share analyzer results across document requests", fixing
[#403](https://github.com/razzmatazz/csharp-language-server/issues/403)) was raised
upstream. It is closely related but addresses a **different** redundancy than the one
in this document:

- **What it fixes:** with `analyzersEnabled = true`, each individual
  `textDocument/diagnostic` request (and each push-diagnostics document) reran a full
  `CompilationWithAnalyzers.GetAllDiagnosticsAsync()` over the *entire project* and
  then filtered down to one document. Pulling N open documents from the same unchanged
  solution snapshot repeated the same project-wide analyzer pass N times. The fix adds
  `AnalyzerDiagnosticsCache` — one `Lazy<Task<ImmutableArray<Diagnostic>>>` per
  `(Solution snapshot, ProjectId)`, stored on `LspWorkspaceFolder.AnalyzerDiagnostics`
  (created at solution load, reset on teardown) — so concurrent/subsequent document
  requests against the same snapshot await the one shared task instead of starting
  their own. Benchmarked: 12 document requests over one snapshot went from 563 ms
  wall / 2,150 ms server CPU to 27 ms wall / 40 ms server CPU.
- **Why it doesn't apply here:** it only changes the two `analyzers`-gated functions in
  `Roslyn/Analyzers.fs`; with `analyzersEnabled = false` (confirmed above), neither is
  ever called, and the plain `compilation.GetDiagnostics()` / `semanticModel.GetDiagnostics()`
  calls have no caching of any kind, before or after #404. It also doesn't touch
  `getWorkspaceDiagnosticReports`, which already computes each project's compilation
  once per poll (not once per document) — so it wouldn't have deduplicated the
  poll-over-poll recompute cascade described in §3 even if analyzers had been on.
- **Design note worth reusing:** the reviewer initially objected to a process-global
  cache and asked for it to live on the workspace folder instead so it's naturally
  scoped/torn down with `wf`'s lifecycle — the author agreed and moved it onto
  `LspWorkspaceFolder`. Any future cache for the plain-diagnostics path (see fix #2
  below) should follow the same pattern rather than introducing a second global
  dictionary.

---

## Proposed follow-up fixes (not yet implemented)

1. **Escape hatch (fastest, already scaffolded):** set `WorkspaceDiagnostics = false`
   in `Diagnostic.fs` registration options. Stops all `workspace/diagnostic` polling;
   per-document `textDocument/diagnostic` (scoped to open files only) continues to
   work. Trades away workspace-wide "Problems" panel coverage for CPU.

2. **Debounce/coalesce recompiles:** if multiple `workspace/diagnostic` polls (or a
   `workspace/diagnostic` poll and a `textDocument/diagnostic`/push-diagnostics request
   for an open document in the same project) arrive while that project's compilation is
   already being (re)computed for the same solution snapshot, share the in-flight work
   instead of recomputing. `PR #404`'s `AnalyzerDiagnosticsCache` (§ "Related upstream
   work" above) is a directly reusable pattern for this — a
   `ConditionalWeakTable<Solution, ConcurrentDictionary<ProjectId, Lazy<Task<...>>>>`
   stored on `LspWorkspaceFolder` — except it would need to wrap the plain
   `compilation.GetDiagnostics()` call too (currently uncached), not just the
   analyzers-enabled path. Note this only dedupes *concurrent/overlapping* requests
   against the *same* snapshot; it does not reduce the number of *new* snapshots
   produced by the dependent-version cascade in §3 — see #3/#4 for that.

3. **Narrow the blast radius of dependent-version invalidation:** consider hashing only
   the diagnostics that are actually about to be sent (as the original Fix 1 draft in
   `workspace-diagnostics-flood.md` proposed — a SHA-1 of the sorted diagnostic
   list per document) *in addition to* the project version check, so that documents
   whose diagnostics didn't actually change can still be reported `"unchanged"` even
   though their project's dependent version ticked over. This reduces wire payload
   further but does **not** reduce compute, since the diagnostics must still be computed
   once to hash them — see #2 for the compute-side fix.

4. **Scope workspace scanning to relevant projects:** instead of unconditionally
   recomputing every project in the solution on every poll
   (`for project in solutionProjects do Async.Start(...)`), prioritize/limit to
   projects reachable from currently open documents, with the rest computed lazily or
   on a longer cadence. This is a bigger behavioral change and needs design (it affects
   the completeness guarantee of workspace diagnostics).

5. ~~Check `analyzersEnabled`~~ — confirmed disabled throughout data collection (§5
   above). If/when it's enabled, `getCompilationDiagnosticsWithAnalyzers` /
   `getDocumentDiagnosticsWithAnalyzers` are already covered by PR #404's cache for the
   per-document/push paths; `getWorkspaceDiagnosticReports`'s per-project call would
   still pay the extra analyzer cost once per invalidated project per poll, on top of
   everything in this document.

---

## Suggested validation for whichever fix is picked

- Re-run the `ps` CPU sampling method above with the fix applied, idle (no typing) for
  ~30 s after opening the workspace, and confirm CPU settles near 0% once the initial
  workspace load/compile completes.
- Re-run the `dotnet-trace` capture and confirm `Binder.*` / `MethodCompiler.*` frames
  no longer accumulate meaningful inclusive time during idle periods.
- Grep a fresh `~/csharp-ls-rpc.log` for `"kind":"full"` vs `"kind":"unchanged"` counts
  and confirm the ratio drops significantly for a comparable editing session.

---

## References

- `plans/workspace-diagnostics-flood.md` — prior investigation of the same subsystem
  (network volume, not CPU)
- `src/CSharpLanguageServer/Handlers/Diagnostic.fs` — `projectResultId`,
  `getWorkspaceDiagnosticReports`, `generateProjectDiagnosticReports'`
- `src/CSharpLanguageServer/Runtime/PushDiagnostics.fs` — push-diagnostics fallback path
  (confirmed inactive here since VS Code supports pull diagnostics)
- `src/CSharpLanguageServer/Roslyn/Analyzers.fs` — `AnalyzerDiagnosticsCache` (added by
  PR #404), scoped to the `analyzersEnabled = true` path only
- [PR #404](https://github.com/razzmatazz/csharp-language-server/pull/404) — "share
  analyzer results across document requests" (upstream, related but does not fix the
  issue in this document — analyzers were off during data collection)
- [Issue #403](https://github.com/razzmatazz/csharp-language-server/issues/403) —
  underlying report for PR #404
- [`dotnet-trace` docs](https://learn.microsoft.com/dotnet/core/diagnostics/dotnet-trace)
