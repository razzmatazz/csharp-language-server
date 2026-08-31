# Interactive Request Latency Under Analyzer Load

This document analyses why `textDocument/completion` (and other interactive UI
requests) can go unanswered for the entire lifetime of a client-side request when
analyzers are enabled, and proposes a fix that bounds CPU contention from
analyzer-driven diagnostics without touching the scheduler's correctness rules.

This is a **companion** to `plans/workspace-diagnostics-flood.md`, which addresses
the *volume* and *polling frequency* of `workspace/diagnostic` traffic. That plan
does not fix the problem described here: even a single, correctly-throttled
`workspace/diagnostic` poll still fans out analyzer work across every project in
the solution with no concurrency limit, and that unbounded fan-out is what starves
interactive requests. The two plans are independent and both worth doing.

---

## Background

`csharp-ls` uses a hand-rolled `RequestQueue` (`Runtime/RequestScheduling.fs`) with
three scheduling modes — `ReadOnly`, `ReadWrite`, `ReadOnlyBackground` (plus
`OutOfBand` for `$/csharp/debugInfo`). These modes exist to protect **workspace
mutation correctness** — e.g. a `ReadWrite` request (`didChange`, `didOpen`, …)
must not run concurrently with anything else, while `ReadOnly` requests
(`completion`, `hover`, `textDocument/diagnostic`, …) may run concurrently with
each other, and `ReadOnlyBackground` requests (`workspace/diagnostic`) never block
retirement of anything else.

Crucially, **none of these modes control actual CPU / .NET `ThreadPool`
scheduling**. They only decide when a request is *allowed to start* from the
scheduler's point of view. Once a request is activated, its handler runs as
ordinary F# `Async`/`Task` work on the shared `ThreadPool`, alongside every other
concurrently-running request — with no priority differentiation between, say, a
`textDocument/completion` continuation and a `CompilationWithAnalyzers.
GetAllDiagnosticsAsync` call that is walking every syntax node in a project with
every enabled Roslyn analyzer.

## Observed Symptom

Reproduced from a real session's `csharp-ls-rpc.log` against a solution with a
`printlog.sln` composed of **59 projects**, `analyzersEnabled: true`.

Right after the client opens a document and starts typing, three
`textDocument/completion` requests are issued in short succession while
`workspace/diagnostic` (`ReadOnlyBackground`, analyzers enabled) is still
streaming its initial full-solution scan via `$/progress`:

```
READ  id=42  textDocument/completion  (triggerCharacter ".")
...
READ  $/cancelRequest id=42
READ  id=50  textDocument/completion  (triggerKind 1)
READ  $/cancelRequest id=50
READ  id=51  textDocument/completion  (triggerCharacter ".")
READ  $/cancelRequest id=51
```

None of these three requests ever receives a `WRITE` response — grepping the
entire log for `"id":42,"result"` / `"id":42,"error"` (and the same for 50 and
51) returns nothing. Every one was cancelled by the client (VS Code cancels a
stale completion request once the user keeps typing and no response arrives).
Meanwhile, in between and around those completion requests, `$/progress`
notifications for `workspace/diagnostic` — one per file, carrying CA1305 /
CA1711 / CA2201 / CA1822 / ASP0018 analyzer diagnostics — continue arriving
without any gap. The background analyzer sweep never yields; the interactive
requests never get a chance to finish before the client gives up on them.

This is not a one-off hiccup — it is the steady-state behavior for as long as
the analyzer-driven diagnostic sweep is in flight.

## Root Cause

`getWorkspaceDiagnosticReports` in `Handlers/Diagnostic.fs` launches **every
project in the solution concurrently and unbounded**:

```fsharp
for wf in workspaceFolders do
    let solutionProjects = ...
    for project in solutionProjects do
        Async.Start(generateProjectDiagnosticReports wf project)
```

Each of those tasks calls (via `Roslyn/Analyzers.fs`):

```fsharp
let cwa = compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)
let! allDiags = cwa.GetAllDiagnosticsAsync(ct) |> Async.AwaitTask
```

`GetAllDiagnosticsAsync` is CPU-bound, synchronous-under-the-hood work (analyzer
callbacks walking syntax/semantic trees). For a 59-project solution this means up
to 59 concurrent, CPU-saturating analyzer passes competing for the same
`ThreadPool` that also runs every `ReadOnly` interactive handler
(`textDocument/completion`, `textDocument/hover`, `textDocument/signatureHelp`,
…).

The `RequestMode = ReadOnlyBackground` tag on `workspace/diagnostic` only means
"don't block queue retirement" — it says nothing about CPU budget. Two
compounding .NET runtime effects turn this into the observed multi-second (or
longer) stalls:

1. **No thread-pool priority.** The CLR `ThreadPool` has no notion of "this task
   is interactive, prioritize it" — work items are serviced roughly FIFO per
   queue, and a burst of CPU-bound work item saturates every worker thread
   equally.
2. **Slow thread injection under sustained load.** Once all existing worker
   threads are busy, the `ThreadPool`'s hill-climbing heuristic injects new
   threads slowly (on the order of one every several hundred milliseconds), by
   design, to avoid over-provisioning threads for bursty load. A sudden fan-out
   of dozens of long-running CPU-bound tasks defeats this heuristic's
   assumptions and can leave a "ready to run" interactive continuation queued
   for a surprisingly long time.

Notably, the codebase already solved *this exact class of problem* once before:
`Runtime/PushDiagnostics.fs`'s background push-diagnostics pipeline deliberately
caps itself to **one in-flight document resolution at a time**
(`CurrentDocTask`), specifically to avoid flooding the thread pool with
concurrent analyzer runs. The `workspace/diagnostic` pull path
(`getWorkspaceDiagnosticReports`) never received the same treatment — it is a
plain unbounded fan-out.

## Affected Files

| File | Concern |
|------|---------|
| `src/CSharpLanguageServer/Roslyn/Analyzers.fs` | `getCompilationDiagnosticsWithAnalyzers` / `getDocumentDiagnosticsWithAnalyzers` — the two entry points where all CPU-bound analyzer execution funnels through, regardless of caller |
| `src/CSharpLanguageServer/Handlers/Diagnostic.fs` | `getWorkspaceDiagnosticReports` — unbounded per-project `Async.Start` fan-out |
| `src/CSharpLanguageServer/Runtime/PushDiagnostics.fs` | Existing single-in-flight precedent; also funnels through `Analyzers.fs` and should share the same limiter |
| `src/CSharpLanguageServer/Types.fs` | `CSharpConfiguration` — home for a new tunable concurrency-limit setting |

## Proposed Fix

**Status: Option B below was implemented and shipped, then empirically
falsified — see "Post-implementation results" after it. Option C has since
been implemented and is the current state of the code** — see
"Post-implementation notes (Option C)" after its section below.

There are three approaches considered so far. Options A and B both gate
analyzer execution through the same choke point (`Roslyn/Analyzers.fs`)
without changing the unbounded per-project fan-out in `Diagnostic.fs`. Option
C instead removes the fan-out itself and additionally disables Roslyn's
internal analyzer concurrency.

- **Option A — shared semaphore.** Simple, small diff, but has a real
  trade-off: a static cap either wastes idle cores (if sized below
  `ProcessorCount`) or fails to protect interactivity (if sized at
  `ProcessorCount`). See the discussion below for why. Not implemented.
- **Option B — dedicated worker-thread pool.** More moving parts, elastic in
  theory (uses every core when nothing else needs them). **Implemented, then
  found insufficient in practice** — isolating analyzer execution onto its own
  OS threads does not create spare CPU capacity when that pool is sized at
  `ProcessorCount` and fully busy; it only fixes ThreadPool thread-injection
  latency, which turned out not to be the dominant effect. See
  "Post-implementation results" below.
- **Option C — sequential, single-project processing with non-concurrent
  analyzer execution (preferred).** Removes the unbounded fan-out entirely
  (one project's diagnostics are fully computed before the next project
  starts) and disables Roslyn's own internal per-analyzer-pass concurrency
  (`concurrentAnalysis: false`), matching the precedent already set by
  Roslyn's own IDE layer and OmniSharp. This bounds analyzer CPU usage to
  ~1 core system-wide by construction, rather than relying on isolation or a
  sized pool to *hopefully* leave headroom.

Read Option A first — its limitation is what motivates Option B. Read the
"Post-implementation results" section after Option B for why Option C
replaces it as the recommendation.

### Option A — Bound concurrent analyzer execution with a shared semaphore (simple, static cap)

Add a single shared concurrency gate at the lowest common layer: the two
functions in `Analyzers.fs` that actually invoke `CompilationWithAnalyzers.
GetAllDiagnosticsAsync`. Every analyzer-driven diagnostic computation — push,
per-document pull, and workspace pull — funnels through one of these two
functions, so gating there caps *total* concurrent analyzer CPU usage across all
three pipelines at once, with no changes needed to the fan-out logic itself.

```fsharp
// Roslyn/Analyzers.fs

open System.Threading

/// Caps the number of CompilationWithAnalyzers passes that may run
/// concurrently across all diagnostic pipelines (push, pull-per-document,
/// pull-workspace). Analyzer execution is CPU-bound and otherwise saturates
/// the shared .NET ThreadPool, starving interactive LSP requests (completion,
/// hover, signatureHelp, …) that run on the same pool. Sized to leave
/// headroom for interactive work rather than using every core.
let private analyzerConcurrencyGate =
    new SemaphoreSlim(max 1 (System.Environment.ProcessorCount / 2))

let private withAnalyzerConcurrencyGate (work: unit -> Async<'T>) : Async<'T> = async {
    do! analyzerConcurrencyGate.WaitAsync() |> Async.AwaitTask
    try
        return! work ()
    finally
        analyzerConcurrencyGate.Release() |> ignore
}
```

Wrap the analyzer-executing branch of both functions (the compiler-only
fallback path, which is cheap, stays ungated):

```fsharp
let getCompilationDiagnosticsWithAnalyzers (project: Project) (compilation: Compilation) : Async<Diagnostic list> = async {
    let! ct = Async.CancellationToken
    let analyzers = ...

    if analyzers.IsEmpty then
        return compilation.GetDiagnostics(ct) |> List.ofSeq
    else
        return!
            withAnalyzerConcurrencyGate (fun () -> async {
                let cwa = compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)
                let! allDiags = cwa.GetAllDiagnosticsAsync(ct) |> Async.AwaitTask
                return allDiags |> List.ofSeq
            })
}
```

(and the equivalent wrap in `getDocumentDiagnosticsWithAnalyzers`).

Because the gate lives inside `Async`, the existing unbounded
`Async.Start`-per-project loop in `getWorkspaceDiagnosticReports` does not need
to change at all: all 59 project tasks still start immediately, but only N of
them are doing real CPU work at any moment — the rest sit cheaply parked on
`SemaphoreSlim.WaitAsync()`, which does not occupy a thread-pool worker thread.
`Async.Start`'s per-task overhead (scheduling a continuation once its turn
comes) is negligible compared to the analyzer work it's gating.

**Limitation.** A `SemaphoreSlim(N)` is a static, all-or-nothing cap — it has no
way to express "use everything when idle, back off when interactive work shows
up":

- `N < ProcessorCount` (e.g. `ProcessorCount / 2`) leaves cores sitting unused
  whenever no interactive request is in flight, for no benefit.
- `N = ProcessorCount` doesn't actually fix the starvation: the observed stall
  happens even when analyzer work alone is enough to saturate every core, which
  is precisely the situation this setting permits without restriction — it does
  nothing to reserve a core for `completion`'s continuation to run on.

Making the cap adaptive (shrink it while an interactive request is pending, grow
it back afterward) is possible but turns the semaphore into its own small
scheduler — tracking an "interactive requests in flight" counter fed by
`EnterRequestContext`/`LeaveRequestContext`, resizing admission accordingly —
which is most of the complexity of Option B without the isolation benefits
described there. If Option A is implemented, size it conservatively (below
`ProcessorCount`) and treat the wasted idle cores as a known, accepted trade-off
for simplicity.

### Sizing / configurability (Option A)

Default to `max(1, Environment.ProcessorCount / 2)` — leaving roughly half the
machine's cores free for interactive request handling and everything else the
process does (JSON parsing, MSBuild/Roslyn incremental work, GC, etc.). Expose
it as an optional override on `CSharpConfiguration` for users who want to tune
it (e.g. dedicating more cores to background analysis on a otherwise-idle CI
box, or fewer on a laptop):

```fsharp
// Types.fs
type CSharpConfiguration =
    { ...
      maxConcurrentAnalyzerRuns: int option }
```

wired through `mergeCSharpConfiguration` the same way `analyzersEnabled` is, and
read once at startup (or on `didChangeConfiguration`) to size/resize the
semaphore — see "Open questions" below for whether resizing needs to be
supported at runtime or only at process start.

### Option B — Dedicate a separate worker-thread pool to analyzer execution (elastic; implemented, superseded by Option C)

The actual bottleneck isn't raw core count — it's that analyzer work and
interactive-handler continuations both get scheduled through the **same CLR
`ThreadPool`**, which has two properties that combine badly here:

1. **Slow thread injection under sustained load.** Once a burst of long
   CPU-bound analyzer tasks occupies every existing worker thread, the
   `ThreadPool`'s hill-climbing heuristic grows the worker count slowly (on the
   order of one new thread every several hundred ms), by design, to avoid
   over-provisioning for bursty load. A new work item — the completion
   handler's continuation — queues behind that throttled growth curve.
2. **No priority differentiation.** The pool doesn't distinguish "interactive"
   from "background" work items; everything queued is serviced roughly
   FIFO/round-robin per worker.

Isolating analyzer execution onto its **own fixed set of real OS threads** —
sized to `Environment.ProcessorCount`, not less — sidesteps both problems at
once, without guessing at a static cap:

```fsharp
// Roslyn/Analyzers.fs

/// Dedicated worker-thread pool for CPU-bound analyzer execution, decoupled
/// from the CLR's default ThreadPool (which services JSON-RPC transport and
/// every interactive LSP handler). Using our own persistent OS threads means
/// the .NET ThreadPool's slow thread-injection heuristic never gets in the
/// way of interactive continuations — the OS's own preemptive scheduler
/// naturally time-slices CPU between these threads and whatever else needs
/// to run, on a scheduler-quantum timescale (single-digit ms) rather than a
/// ThreadPool-injection timescale (hundreds of ms to seconds). Analyzer work
/// still saturates every core when nothing else is running; it just no
/// longer blocks the pool that interactive requests depend on.
type private AnalyzerWorkItem = unit -> unit

let private analyzerWorkQueue = new BlockingCollection<AnalyzerWorkItem>()

let private analyzerWorkerThreads =
    [ for _ in 1 .. max 1 System.Environment.ProcessorCount ->
        let t =
            Thread(fun () ->
                for work in analyzerWorkQueue.GetConsumingEnumerable() do
                    work ())
        t.IsBackground <- true
        // Best-effort only — see the portability caveat below. Harmless no-op
        // where the OS/runtime doesn't honor it.
        t.Priority <- ThreadPriority.BelowNormal
        t.Start()
        t ]

/// Runs `work` on the dedicated analyzer pool and returns its result as an
/// Async, bridging back onto the caller's synchronization context the same
/// way `Async.AwaitTask` does for CLR-pool work.
let private runOnAnalyzerPool (work: unit -> 'T) : Async<'T> =
    Async.FromContinuations(fun (resolve, reject, _) ->
        analyzerWorkQueue.Add(fun () ->
            try
                resolve (work ())
            with ex ->
                reject ex))
```

Then, in `getCompilationDiagnosticsWithAnalyzers` / `
getDocumentDiagnosticsWithAnalyzers`, run the `GetAllDiagnosticsAsync` call (or
rather, block on it synchronously) inside `runOnAnalyzerPool` instead of
`Async.AwaitTask`-ing it directly:

```fsharp
if analyzers.IsEmpty then
    return compilation.GetDiagnostics(ct) |> List.ofSeq
else
    return!
        runOnAnalyzerPool (fun () ->
            let cwa = compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)
            cwa.GetAllDiagnosticsAsync(ct).GetAwaiter().GetResult() |> List.ofSeq)
```

Because every analyzer thread only ever runs synchronous, CPU-bound work
(`GetAwaiter().GetResult()` is fine here — the thread has nothing else to do
while waiting), there's no need for the queue itself to be async-aware; only the
bridge back to the caller (`runOnAnalyzerPool`'s `Async.FromContinuations`) needs
to hand the result back onto whichever context resumes it.

**Portability caveat on thread priority.** `Thread.Priority` is not reliably
portable in .NET: it has real effect on Windows, but on Linux's default
`SCHED_OTHER` scheduling policy (and to a lesser extent on macOS) priority hints
are weak or effectively ignored. Treat `ThreadPriority.BelowNormal` above as a
*best-effort enhancement where the OS honors it*, not the mechanism the fix
depends on. The isolation from the CLR `ThreadPool` — not the priority — is what
actually prevents interactive-request starvation, because it removes analyzer
work from the pool whose slow thread-injection heuristic was the real bottleneck;
ordinary OS preemptive scheduling then interleaves the dedicated analyzer
threads with everything else fairly, regardless of whether priority hints are
honored.

**Interaction with the unbounded `Async.Start` fan-out.** Same as Option A: the
per-project loop in `getWorkspaceDiagnosticReports` doesn't need to change.
Each project's task still starts immediately; the CPU-bound part of its work
just gets handed off to the dedicated pool and waits its turn there via the
`BlockingCollection`, instead of parking on a `SemaphoreSlim`.

**Sizing.** Unlike Option A there's no headroom trade-off to tune — the pool can
be sized to the full `Environment.ProcessorCount` because isolation (not a
reduced count) is what protects interactivity. A `maxConcurrentAnalyzerRuns`
config override is still worth keeping for users who want to reserve some cores
for other processes on the same machine, but the *default* no longer needs to
undershoot the core count.

### Post-implementation results: Option B does not fix the problem

Option B was implemented as described above: `Roslyn/Analyzers.fs` gained a
private `AnalyzerPool` module — a `BlockingCollection<unit -> unit>` work queue
serviced by `max 1 Environment.ProcessorCount` dedicated, background,
`BelowNormal`-priority OS threads — and both
`getCompilationDiagnosticsWithAnalyzers` and
`getDocumentDiagnosticsWithAnalyzers` were switched from `Async.AwaitTask` on
`GetAllDiagnosticsAsync` to `runOnAnalyzerPool (fun () -> ....GetAwaiter()
.GetResult())`. The unbounded per-project `Async.Start` fan-out in
`getWorkspaceDiagnosticReports` was left unchanged, per the design.

A second reference session was captured against the same 59-project
`printlog.sln` (`~/csharp-ls-rpc.log`, distinct from the log referenced
elsewhere in this doc) with the fix live. The result: **no material
improvement.** Completion requests issued while `workspace/diagnostic`'s
initial full-solution scan was in flight (ids `39`, `56`, `57` in that log,
all `textDocument/completion` against the same open file) never received a
`result`/`error` response from the handler — they were swept up in the same
mass `$/cancelRequest`-driven batch of `-32800 Request cancelled` responses as
dozens of other stale requests, the same failure mode `plans/
interactive-request-latency-vs-analyzers.md` originally documented before
Option B existed.

**Why isolation onto a dedicated pool didn't help:** a fixed pool of
`Environment.ProcessorCount` OS threads, once fully occupied by CPU-bound
analyzer work, saturates every core exactly as thoroughly as the same work
running on the CLR `ThreadPool` would — moving it to a separate pool changes
*which* threads are busy, not *how many cores are free*. The isolation only
buys freedom from the ThreadPool's slow hill-climbing thread-injection
heuristic; it does nothing to reserve spare CPU capacity for an interactive
request's continuation, because there was never any spare capacity to
reserve — sizing the pool at the full core count was precisely the
choice that (per the Option A discussion above) "doesn't actually fix the
starvation." In hindsight this was already flagged as a risk in the Option A
section but wrongly assumed not to apply to Option B; it applies identically,
because both approaches ultimately run analyzer work at full concurrency
across every core.

There is a second, compounding contributor that neither Option A nor Option B
addressed: `getWorkspaceDiagnosticReports`'s per-project fan-out calls
`project.GetCompilationAsync` and `project.GetSourceGeneratedDocumentsAsync`
for **every project concurrently**, *before* any of that work reaches the
analyzer gate/pool. For a 59-project solution this alone is up to 59
concurrent Roslyn compilation-construction passes competing for CPU, independent
of whatever concurrency limit is placed on the analyzer step itself.

This motivates Option C below, which removes the fan-out itself (rather than
gating what happens after it) and additionally addresses concurrency Roslyn
introduces *inside* a single project's analyzer pass, which Options A and B
both left unaddressed.

### Option C — Sequential, single-project processing with non-concurrent analyzer execution (preferred)

Two independent changes, both bounding concurrency at its source rather than
gating or isolating it after the fact:

1. **Process one project at a time.** Replace the unbounded
   `for project in solutionProjects do Async.Start(generateProjectDiagnosticReports wf project)`
   fan-out in `getWorkspaceDiagnosticReports` (`Handlers/Diagnostic.fs`) with a
   plain sequential loop — `do! generateProjectDiagnosticReports wf project`
   for each project in turn, awaited before moving to the next. Only one
   project's compilation is ever being built, and only one project's analyzer
   pass is ever running, at any moment across the whole process. The existing
   `Channel`-based streaming of `DiagnosticsReport`/`ReportingDoneForProject`
   items can be simplified or removed — with no concurrent producers, the
   `asyncSeq` block can `yield` reports directly as it walks each project in
   order, instead of routing them through a channel meant to fan results in
   from parallel writers.

2. **Disable Roslyn's own internal analyzer concurrency.** Both entry points
   in `Roslyn/Analyzers.fs` currently call the
   `compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)` overload,
   which defaults to `concurrentAnalysis: true` — meaning a *single* project's
   analyzer pass can still internally fan out across every core via Roslyn's
   own `AnalyzerDriver`, even once cross-project concurrency is eliminated by
   (1). Switch to the `CompilationWithAnalyzersOptions` overload with
   `concurrentAnalysis: false` explicitly:

   ```fsharp
   let analysisOptions =
       CompilationWithAnalyzersOptions(
           options = project.AnalyzerOptions,
           onAnalyzerException = null,
           concurrentAnalysis = false,
           logAnalyzerExecutionTime = false)

   let cwa = compilation.WithAnalyzers(analyzers, analysisOptions)
   ```

   This is not a novel or aggressive choice — it is the existing precedent in
   the Roslyn codebase itself: the IDE diagnostics layer
   (`DiagnosticIncrementalAnalyzer.CompilationManager`) always sets
   `concurrentAnalysis == false`, with an inline comment explaining why:
   *"in IDE, we always set concurrentAnalysis == false otherwise, we can get
   into thread starvation due to async being used with synchronous blocking
   concurrency."* OmniSharp's Roslyn integration (`OmniSharp.Roslyn`) does the
   same, unconditionally, for the same reason.

With both changes in place, the total number of cores that analyzer-driven
diagnostic computation can occupy system-wide, across all three pipelines
(push, pull-per-document, pull-workspace), is bounded to **1** by
construction — not by a sized pool or semaphore that has to guess where the
line between "enough headroom" and "wasted idle cores" is. The dedicated
`AnalyzerPool`/`BlockingCollection`/OS-thread machinery added for Option B
becomes unnecessary complexity at that point and should be removed: a plain
`Async.AwaitTask` around `GetAllDiagnosticsAsync` is sufficient once the work
it's awaiting is itself capped to one thread and one project.

**Trade-off.** Wall-clock time to fully populate `workspace/diagnostic` results for a
large solution will increase relative to either A or B — this is the direct, accepted cost
of removing concurrency rather than gating it, same as the trade-off already called out in
this doc's Acceptance Criteria. Given that Option B's attempt to keep full parallelism
*and* interactivity failed empirically, this trade-off should be treated as required, not
optional. `PushDiagnostics.fs`'s existing single-in-flight design already made the same
trade-off for the push pipeline; Option C simply extends the same principle to the
pull-workspace pipeline instead of trying to preserve its parallelism through isolation.

**Interaction with `PushDiagnostics.fs`.** No changes needed — it already
processes one document at a time (`CurrentDocTask`), so it's unaffected either way
by whichever `concurrentAnalysis` setting `Analyzers.fs` ends up using.

### Post-implementation notes (Option C)

Option C has been implemented as described above, with one deviation from the
original write-up (see "concurrentAnalysis re-enabled" below):

- `Roslyn/Analyzers.fs`: both `getCompilationDiagnosticsWithAnalyzers` and
  `getDocumentDiagnosticsWithAnalyzers` now build a `CompilationWithAnalyzersOptions`
  explicitly (with `logAnalyzerExecutionTime = false`, `onAnalyzerException = null`)
  instead of using the plain `compilation.WithAnalyzers(analyzers,
  project.AnalyzerOptions)` overload. No `AnalyzerPool`/Option-B machinery existed in
  the codebase at the time Option C was implemented, so there was nothing to remove
  there.
- `Handlers/Diagnostic.fs`: `getWorkspaceDiagnosticReports`'s unbounded per-project
  `Async.Start` fan-out and the `Channel<WorkspaceDiagnosticsReportsChannelItem>`
  fan-in were both removed, keeping the diff minimal by leaving
  `generateProjectDiagnosticReports'` nested exactly where it was — only its `async {`
  became `asyncSeq {` and each `do! writeToChannel (DiagnosticsReport r)` became
  `yield r`. The driving loop now iterates workspace folders and projects with plain
  nested `for` loops and `yield!`s each project's reports in turn (wrapped in `try/with`
  per project so one failing project doesn't abort the rest of the sweep), so only one
  project's compilation construction and analyzer pass are ever in flight at a time.
- All existing `AnalyzerTests.fs` / `DiagnosticTests.fs` tests (20 total, covering push
  diagnostics, pull-per-document, pull-workspace, resultId/unchanged-report caching, and
  multi-project cycles) pass unchanged against the new sequential implementation.
- Not yet done: the dedicated instrumented-counter unit test and the
  concurrent-completion-during-sweep integration test from the Testing Checklist below
  were not added as part of this change; the existing test suite was relied on to catch
  regressions in diagnostic *results*, per the Acceptance Criteria note that results are
  unaffected. Revisit the Testing Checklist if stronger concurrency guarantees need to be
  asserted in CI rather than by code inspection.

**`concurrentAnalysis` re-enabled.** The write-up above disables
`concurrentAnalysis` on the theory that it matches Roslyn's own IDE-layer precedent
and caps analyzer CPU usage to one core "by construction." In practice, once the
sequential per-project processing above was in place, `concurrentAnalysis: true` was
tried instead and validated against a real multi-project session with analyzers
enabled: interactive requests stayed responsive through a full `workspace/diagnostic`
sweep. This makes sense in hindsight — the actual failure mode this whole doc is
about was many projects' analyzer passes competing at once, not one project's pass
using multiple cores; with cross-project concurrency already eliminated, there is no
second pass left to compete with the one project currently being analyzed, so
Roslyn's own internal fan-out no longer has anything to starve. `concurrentAnalysis`
is therefore left at its default (`true`) in both `Analyzers.fs` entry points — the
one-project-at-a-time sequencing in `getWorkspaceDiagnosticReports` is what's doing
the actual work of protecting interactivity, and disabling `concurrentAnalysis` on
top of it would only slow down each individual project's sweep for no additional
interactivity benefit. If a future regression is ever traced back to a single very
large project's analyzer pass saturating every core during its own sweep, revisit
this — that failure mode (unlike the original multi-project one) is bounded to that
project's duration rather than the whole sweep's, so it's expected to be far less
severe if it occurs at all.

### Relationship to `plans/workspace-diagnostics-flood.md`

That plan reduces how *often* `workspace/diagnostic` re-scans the whole
solution (via `resultId`/`previousResultIds` support) and how much it scans per
poll (filtering generated files). Both are valuable and should still be done —
they reduce the total amount of analyzer work performed over a session. But
they do not bound the *peak concurrency* of a single poll: even with perfect
incremental polling, the first poll after opening a large solution (or after
any project actually changes) still fans out across every affected project at
once. Option C above is what caps that peak and keeps interactive requests
responsive during it, regardless of how well-behaved the polling cadence is.

### Why not add a new higher-priority `RequestMode` instead?

A tempting alternative is to add e.g. an `Interactive` mode that the scheduler
prioritizes. This would not address the actual bottleneck: `completion` is
already `ReadOnly` and is *already* allowed to activate immediately — the
scheduler never blocks it. The stall happens after activation, inside the
shared `ThreadPool`'s CPU scheduling, which `RequestScheduling.fs` has no
control over. A new `RequestMode` would add complexity without fixing the
symptom; bounding the CPU-heavy work at its source is the direct fix.

---

## Acceptance Criteria

- With `analyzersEnabled: true` and a multi-project solution, issuing a
  `textDocument/completion` request while a `workspace/diagnostic` full scan is
  in flight receives a response well within the client's request-cancellation
  window (i.e. it does not get silently cancelled the way ids 42/50/51 did in
  the first reference log, and ids 39/56/57 did in the post-Option-B reference
  log).
- At most one project is ever being processed (compilation construction *and*
  analyzer execution) by `getWorkspaceDiagnosticReports` at any moment — i.e.
  analyzer-driven diagnostic computation is bounded to a single project system-wide,
  across all three pipelines (push, pull-document, pull-workspace) combined. Within
  that one project, `CompilationWithAnalyzers.GetAllDiagnosticsAsync` runs with
  `concurrentAnalysis: true` (Roslyn's default) rather than `false` — see
  "`concurrentAnalysis` re-enabled" under "Post-implementation notes (Option C)"
  above for why this was found not to reintroduce the starvation this plan targets.
- Existing `AnalyzerTests.fs`, `DiagnosticTests.fs`, and push-diagnostics tests
  continue to pass — total diagnostic *results* are unaffected, only the
  concurrency of computing them changes.
- Total wall-clock time to fully populate `workspace/diagnostic` results for a
  large solution does not need to hold steady — a real, possibly significant,
  increase is expected and accepted as the direct cost of Option C (see
  "Trade-off" under Option C above); the acceptance bar is interactive
  responsiveness, not sweep completion time.

## Testing Checklist

- [ ] Unit test in a new `AnalyzersTests.fs`: fire N concurrent calls into
      `getCompilationDiagnosticsWithAnalyzers` / `getDocumentDiagnosticsWithAnalyzers`
      against a fixture with analyzers enabled, and separately drive
      `getWorkspaceDiagnosticReports` across a multi-project fixture; assert
      via an instrumented counter that at most one project/analyzer pass is
      ever running concurrently.
- [ ] Integration test using `projectWithEditorConfigAnalyzers` (or a new
      multi-project fixture) with `AnalyzersEnabled = Some true`: open a
      document, trigger `workspace/diagnostic`, and concurrently issue
      `textDocument/completion`; assert the completion response arrives within
      a bounded time (e.g. a few seconds) rather than timing out.
- [ ] Verify cancellation still works correctly mid-sweep: cancelling a
      `workspace/diagnostic` request partway through the sequential project
      loop must stop promptly rather than running the remaining projects to
      completion regardless.
- [ ] Verify the sequential loop and `concurrentAnalysis: false` don't
      introduce any deadlock or ordering bug on a single-project,
      single-document fixture (the degenerate case).
- [ ] Existing `AnalyzerTests.fs` / `DiagnosticTests.fs` / push-diagnostics
      tests pass unchanged.

## Open Questions

| # | Question | Notes |
|---|---|---|
| 1 | Should there be a config knob to opt back into higher concurrency (e.g. for CI/batch use with no interactive client attached, where sweep latency matters more than completion responsiveness)? | Not needed for the interactive-editor use case this plan targets; worth keeping in mind if a batch/headless mode (`--diagnose` or similar) is ever built on top of the same code path, but out of scope for this fix. |
| 2 | Does disabling `concurrentAnalysis` measurably slow down cancellation responsiveness for a single project's analyzer pass (since there's no longer a second thread free to notice a cancellation token promptly)? | Needs empirical check once implemented; Roslyn's own IDE layer runs this way in practice without reported issues, so expected to be a non-issue, but worth verifying against the project sizes seen in `printlog.sln`. |
| 3 | Should `getWorkspaceDiagnosticReports`'s existing `Channel`-based plumbing be removed outright, or kept (with a single producer) so future work can reintroduce bounded concurrency more easily? | Lean toward removing it — simpler code that matches what's actually happening (sequential) is preferable to keeping infrastructure for a concurrency model that was just abandoned; can be re-added if a future need arises. |

## Out of Scope

- Option A (shared semaphore) and Option B (dedicated worker-thread pool) as
  previously proposed/implemented — both abandoned per the empirical results
  documented above. Revisit only if Option C's sequential trade-off proves
  unacceptable in practice (see Open Question 1).
- True OS-level thread priority / a dedicated low-priority `TaskScheduler` for
  background analyzer work — moot once analyzer work is bounded to one core
  by construction.
- Changes to `RequestScheduling.fs`'s `RequestMode` semantics — the scheduler's
  activation rules are not the bottleneck here (see "Why not add a new
  higher-priority `RequestMode` instead?" above).
- The `workspace/diagnostic` volume/polling-frequency problem — tracked
  separately in `plans/workspace-diagnostics-flood.md`.

## References

- `src/CSharpLanguageServer/Roslyn/Analyzers.fs` — `getCompilationDiagnosticsWithAnalyzers`, `getDocumentDiagnosticsWithAnalyzers`; currently contains the Option B `AnalyzerPool` machinery, to be removed when Option C lands
- `src/CSharpLanguageServer/Handlers/Diagnostic.fs` — `getWorkspaceDiagnosticReports`; contains the unbounded per-project `Async.Start` fan-out to be replaced by a sequential loop
- `src/CSharpLanguageServer/Runtime/PushDiagnostics.fs` — single-in-flight precedent that Option C extends to the pull-workspace pipeline
- `src/CSharpLanguageServer/Runtime/RequestScheduling.fs` — `RequestMode`, `processRequestQueue`
- `plans/workspace-diagnostics-flood.md` — companion plan for polling volume/frequency
- First reference session log: `csharp-ls-rpc.log` (59-project solution, `analyzersEnabled: true`) — completion request ids 42, 50, 51 cancelled by the client with no server response while a `workspace/diagnostic` analyzer sweep was in flight; captured before Option B existed
- Second reference session log: `~/csharp-ls-rpc.log` (same 59-project solution) — completion request ids 39, 56, 57 cancelled the same way, captured with Option B's dedicated analyzer thread pool live, demonstrating it does not fix the problem
- Roslyn `DiagnosticIncrementalAnalyzer.CompilationManager` — precedent for `concurrentAnalysis: false` in an IDE context (dotnet/roslyn PR #77113)
- OmniSharp `OmniSharp.Roslyn` diagnostics provider — independent precedent for `concurrentAnalysis: false`
