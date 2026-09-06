# Workspace Diagnostics Flood Plan

This document analyses the root cause of runaway `workspace/diagnostic` traffic
observed in `csharp-ls-rpc.log` (400 MB+), and proposes concrete fixes.

---

## Background

The LSP pull-diagnostics protocol lets a client poll `workspace/diagnostic` on a
recurring schedule to stay up to date with workspace-wide diagnostics.  The server
can stream results back incrementally using `$/progress` partial-result
notifications — one payload per document — before sending the final empty response.

VS Code sends this request repeatedly because the server advertises
`WorkspaceDiagnostics = true` in its `DiagnosticRegistrationOptions`.

---

## Observed Symptom

`csharp-ls-rpc.log` contains **72 separate `workspace/diagnostic` READ messages**,
each one a new request from VS Code with a fresh `partialResultToken` and an empty
`previousResultIds: []`.  The requests are spaced roughly every ~194 500 log lines
apart, at a near-constant cadence throughout the session:

```
line      1 088  READ  id=3    workspace/diagnostic  token=86733651-...
line    222 326  READ  id=154  workspace/diagnostic  token=e49f4bca-...
line    426 320  READ  id=...  workspace/diagnostic  token=...
...                            (every ~194 500 lines)
...
line 13 872 355  READ  id=317  workspace/diagnostic  token=e1d49675-...
```

For every one of those 72 requests, the server performs a **full re-scan of the
entire solution** and emits one `$/progress` WRITE per file — including every
Razor source-generated `.g.cs` file.  Each round produces ~194 000 lines of
`$/progress` output.  72 rounds × ~194 000 lines = the entire 14-million-line,
400 MB file.

The dominant diagnostic on almost every file is **CS8019** ("Unnecessary using
directive", severity: warning).

---

## Root Causes

There are two independent problems.  Either one alone would degrade performance;
together they multiply into the observed flood.

### Root cause 1 — VS Code re-polls shortly after each response (the loop)

VS Code's pull-diagnostics engine re-issues `workspace/diagnostic` shortly after
the previous response arrives.  The log confirms the pattern is sequential — each
request's final `{ "items": [] }` response completes before the next request
starts (e.g. request id=3 response at line ~218 110, followed by ~4 200 lines of
other traffic, then request id=154 at line 222 326) — but the gap is small
relative to the ~194 000 lines of diagnostic output per round, so the net effect
is a tight polling loop for the lifetime of the session.

Because the server never populates `resultId` in its
`WorkspaceFullDocumentDiagnosticReport` items (`ResultId = None` at line 132 of
`Diagnostic.fs`), VS Code has no stable result-ids to send back, so every poll
carries `previousResultIds: []` — telling the server it knows nothing and needs
everything.

The LSP spec's intended flow is:

```
client → workspace/diagnostic (previousResultIds: [])
server → $/progress ... (resultId per document)
server → { items: [] }  ← final response

client → workspace/diagnostic (previousResultIds: [{uri, resultId}, ...])
server → WorkspaceUnchangedDocumentDiagnosticReport for unchanged docs
server → { items: [] }
```

Without `resultId`, VS Code cannot detect that nothing changed and keeps
requesting full results indefinitely.

### Root cause 2 — Source-generated files are not filtered (the volume)

`getWorkspaceDiagnosticReports` in `Handlers/Diagnostic.fs` calls:

```fsharp
compilation.GetDiagnostics(ct)
```

This returns diagnostics for **every file** Roslyn compiled, including hundreds of
Razor source-generator outputs under `obj/`:

```
obj/Debug/net8.0/Microsoft.CodeAnalysis.Razor.Compiler/
    Microsoft.NET.Sdk.Razor.SourceGenerators.RazorSourceGenerator/
        Views_OfficeOrders_OrdersMerge_cshtml.g.cs
        Views_*.g.cs   (one per .cshtml view)
        ...
.AssemblyAttributes.cs
.RazorAssemblyInfo.cs
```

Each of these files gets its own `$/progress` notification.  The user cannot edit
them and the diagnostics (mostly CS8019) are not actionable.

The only filter in the entire diagnostics pipeline is a `CS8019` suppression for
`.cshtml` files inside the per-document `textDocument/diagnostic` handler —
`workspace/diagnostic` has **no filters at all**.

Each `.cshtml` view can therefore appear **twice** per poll: diagnostics whose
locations fall within a `#line`-remapped region of the `.g.cs` are attributed to
the `.cshtml` URI via `GetMappedLineSpan()`, while diagnostics on auto-generated
code outside `#line` regions (e.g. `CS8019` on generated `using` directives) keep
the `.g.cs` URI.  Both URIs appear in the same round in the log (e.g.
`OrdersMerge.cshtml` at line ~218 101 and its `.g.cs` at line ~199 842).

---

## Affected Files

| File | Concern |
|------|---------|
| `src/CSharpLanguageServer/Handlers/Diagnostic.fs` | `getWorkspaceDiagnosticReports` / `handleWorkspaceDiagnostic` — no `resultId` emitted, no generated-file filter |
| `src/CSharpLanguageServer/Roslyn/Conversions.fs` | `Diagnostic.fromRoslynDiagnostic` — URI from `GetMappedLineSpan().Path`; no generated-file guard |

---

## Proposed Fixes

Fix both root causes.  Fix 1 (resultId) stops the polling loop from being tight.
Fix 2 (generated-file filter) reduces the volume of each individual poll.
Together they reduce the log from hundreds of megabytes to kilobytes.

### Fix 1 — Emit `resultId` per document to enable incremental responses (high priority)

Populate the `resultId` field on every `WorkspaceFullDocumentDiagnosticReport`
item emitted by `getWorkspaceDiagnosticReports`.  A stable, cheap `resultId` is a
hash of the diagnostic list (e.g. a SHA-1 or FNV of the sorted diagnostic
messages and ranges).

Then in `handleWorkspaceDiagnostic`, build a lookup from the incoming
`previousResultIds` and emit `WorkspaceUnchangedDocumentDiagnosticReport` for any
document whose current hash matches what the client already holds.

```fsharp
// Helper — deterministic hash of a diagnostic array
let diagnosticResultId (items: Diagnostic array) : string =
    items
    |> Array.map (fun d -> sprintf "%A|%s" d.Range d.Message)
    |> Array.sort
    |> String.concat "\n"
    |> (fun s -> s |> System.Text.Encoding.UTF8.GetBytes |> SHA1.HashData)
    |> Convert.ToHexString

// In generateProjectDiagnosticReports, emit resultId:
let fullDocumentReport: WorkspaceFullDocumentDiagnosticReport =
    { Kind = "full"
      ResultId = Some (diagnosticResultId items)   // <-- currently None
      Uri = uri
      Items = items
      Version = None }

let documentReport: WorkspaceDocumentDiagnosticReport = U2.C1 fullDocumentReport

// In handleWorkspaceDiagnostic, respect previousResultIds:
let knownResultIds =
    p.PreviousResultIds
    |> Option.defaultValue [||]
    |> Seq.map (fun r -> r.Uri, r.Value)
    |> Map.ofSeq

// Before emitting a full report, check:
match Map.tryFind uri knownResultIds with
| Some prev when prev = currentResultId ->
    // emit WorkspaceUnchangedDocumentDiagnosticReport instead
| _ ->
    // emit full report as before
```

With this in place VS Code receives stable result-ids on the first poll and sends
them back on every subsequent poll, allowing the server to respond with
`Unchanged` for all unmodified documents — making subsequent polls near-zero cost.

> **Note:** The per-document `textDocument/diagnostic` handler also sets
> `ResultId = None` (line 61), but this is lower priority — per-document pull
> requests are scoped to one file and do not cause the flood.  Adding `resultId`
> there would be a follow-on improvement.

### Fix 2 — Share `diagnosticIsToBeListed` with `workspace/diagnostic` (high priority)

`textDocument/diagnostic` already suppresses CS8019 on `.cshtml` URIs because the
diagnostic appears out of place there (no correct line-mapping).  The same
condition arises in `workspace/diagnostic` — `.cshtml` URIs appear in workspace
results via `GetMappedLineSpan()` remapping — but the suppression is not applied
there.

Extract `diagnosticIsToBeListed` as a module-level private helper that both
handlers share, keyed on the **emitted URI** rather than the request URI:

```fsharp
// Replace the inline closure in `handle` with this shared helper:
let private diagnosticIsToBeListed (uri: string) (d: Microsoft.CodeAnalysis.Diagnostic) =
    if uri.EndsWith(".cshtml", StringComparison.OrdinalIgnoreCase) then
        // CS8019 has no correct line-mapping on .cshtml files and appears out of place
        d.Id <> "CS8019"
    else
        true
```

Apply it in `handle` (replacing the existing inline closure):

```fsharp
let diagnostics =
    semanticModel.GetDiagnostics()
    |> Seq.filter (diagnosticIsToBeListed p.TextDocument.Uri)
    |> Seq.map (Diagnostic.fromRoslynDiagnostic wfPathToUri)
    |> Seq.map fst
    |> Array.ofSeq
```

Apply it in `generateProjectDiagnosticReports'`, after the `groupBy` where the URI
is already known:

```fsharp
for uri, items in diagnosticsByDocument do
    let items =
        items
        |> Seq.map fst
        |> Seq.filter (diagnosticIsToBeListed uri)
        |> Array.ofSeq
    // skip emitting a $/progress notification if filtering left nothing to report
    if items.Length > 0 then
        let fullDocumentReport: WorkspaceFullDocumentDiagnosticReport =
            { Kind = "full"
              ResultId = None
              Uri = uri
              Items = items
              Version = None }
        // ... write to channel as before
```

The `if items.Length > 0` guard avoids emitting a `$/progress` notification for a
document whose only diagnostics were all suppressed — an easy additional win since
`.cshtml` files often have nothing left after removing CS8019.

### Fix 3 — Fallback: disable `WorkspaceDiagnostics` (escape hatch)

If Fixes 1 and 2 are not yet ready, set `WorkspaceDiagnostics = false` in
`registrationOptions`.  This stops VS Code from ever sending
`workspace/diagnostic`.  Per-document pull diagnostics (`textDocument/diagnostic`)
continue to work normally and are unaffected.

```fsharp
let private registrationOptions documentSelector : DiagnosticRegistrationOptions =
    { ...
      WorkspaceDiagnostics = false
      ... }
```

---

## Acceptance Criteria

- A `workspace/diagnostic` request against a Razor/ASP.NET solution produces
  **no `$/progress` notifications** for files under `obj/` or with a `.g.cs`
  suffix.
- A second `workspace/diagnostic` request with the `previousResultIds` returned
  by the first results in `WorkspaceUnchangedDocumentDiagnosticReport` for all
  unmodified documents — no `$/progress` flood.
- The RPC log size for a full session with VS Code open against a Razor solution
  is comparable to a few `textDocument/diagnostic` bursts (kilobytes, not
  megabytes).
- Existing tests in `DiagnosticTests.fs` continue to pass.
- A new integration test using the `aspnetProject` fixture verifies:
  - no `.g.cs` URIs appear in workspace diagnostic results
  - a second poll with returned result-ids yields only `Unchanged` reports

---

## Non-fix: `workspace/diagnostic/refresh`

`workspace/diagnostic/refresh` is a **server → client** request that tells VS Code
to discard its cached diagnostic results and re-poll.  It is the wrong tool for
this problem because VS Code is already polling too aggressively — sending refresh
would give the server a way to trigger *additional* polls on demand, not fewer.

The stub implementation in `Diagnostics.fs` (`WorkspaceDiagnosticRefresh() = async
{ return LspResult.Ok() }`) is correct as-is — it only lives in the `--diagnose`
command's fake client and is never called in the real server path.

Once Fix 1 (resultId) is in place and each poll is cheap, calling
`lspClient.WorkspaceDiagnosticRefresh()` after a `WorkspaceReloadRequested` event
fires would be a useful follow-on: it would tell VS Code to re-fetch immediately
after a workspace reload rather than waiting for its next scheduled poll.  That is
a feature addition, not a fix for the current flood.

---

## References

- LSP spec — [Workspace Diagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_diagnostic)
- `Handlers/Diagnostic.fs` — `getWorkspaceDiagnosticReports`, `handleWorkspaceDiagnostic`
- `Runtime/PushDiagnostics.fs` — push-diagnostics subsystem (separate; not affected)
- `Roslyn/Conversions.fs` — `Diagnostic.fromRoslynDiagnostic`

---

## Update: two more per-poll optimizations prototyped, then reverted

With Fix 1 and Fix 2 above already implemented (resultId caching + generated-file
filtering) and the sequential per-project processing from
`plans/interactive-request-latency-vs-analyzers.md` (Option C) also in place, a
live `csharp-ls` session against a real, large multi-project solution (73
projects, `~/csharp-ls-rpc.log`) still showed periodic CPU spikes (up to ~17% sustained,
occasionally much higher) and a rapidly-growing rpc log (140 MB+ within a single
session). Two additional, narrower inefficiencies were identified in
`Handlers/Diagnostic.fs` and prototyped:

1. **One `$/progress` write per document, never batched.** In
   `handleWorkspaceDiagnostic`'s streaming branch, `Items` was always a
   single-element array — for a large solution, a poll that touches thousands of
   documents meant thousands of individual JSON-serialize + JSON-RPC-write round
   trips, even when every item was a cheap `"unchanged"` stub.
2. **O(files-in-solution × projects) cache lookup per poll.** In
   `generateProjectDiagnosticReports'`, `clientKnownResultsForProject` was
   computed as `knownResultIds |> Map.toSeq |> Seq.filter (fun (uri, _) ->
   projectDocumentUris.Contains uri)` — re-walking the *entire*, solution-wide
   `knownResultIds` map once per project, rather than probing per-document with
   `Map.tryFind`.

Both were implemented, built cleanly, and all 20 existing `DiagnosticTests.fs` /
`AnalyzerTests.fs` tests passed unchanged. Observed effect on the live session
after a VS Code restart: `$/progress` messages dropped from one line per file to
one line per ~200 files (confirmed in the rpc log — batches of exactly 200
`"unchanged"` items per line), and a tail sample of the log after the initial
cold sweep was **100% `"unchanged"` reports** (cache hits, no re-analysis),
confirming both changes worked as intended and didn't change results.

### Why the real-world win was small anyway

Investigating why CPU/log growth continued well past the initial cold sweep led
to reading the actual client-side scheduling logic in
[`microsoft/vscode-languageserver-node`](https://github.com/microsoft/vscode-languageserver-node),
`client/src/common/diagnostic.ts` (`main` branch), `DiagnosticFeatureProviderImpl.pullWorkspace`:

```typescript
public pullWorkspace(): void {
    if (this.isDisposed) {
        return;
    }
    this.pullWorkspaceAsync().then(() => {
        this.workspaceTimeout = RAL().timer.setTimeout(() => {
            this.pullWorkspace();
        }, 2000);
    }, (error) => {
        if (!(error instanceof LSPCancellationError) && !DiagnosticServerCancellationData.is(error.data)) {
            this.client.error(`Workspace diagnostic pull failed.`, error, false);
            this.workspaceErrorCounter++;
        }
        if (this.workspaceErrorCounter <= 5) {
            this.workspaceTimeout = RAL().timer.setTimeout(() => {
                this.pullWorkspace();
            }, 2000);
        }
    });
}
```

`pullWorkspace()` is self-perpetuating: the moment one `workspace/diagnostic`
round trip finishes (success, or failure up to 5 consecutive errors), it
schedules the *next* one via a hardcoded `setTimeout(..., 2000)`. This runs for
the entire lifetime of the editor session — forever, roughly every ~2 seconds —
as long as the server advertises `WorkspaceDiagnostics = true` in
`DiagnosticRegistrationOptions`, **completely independent of whether anything in
the solution changed**. This matches
[microsoft/vscode-languageserver-node#1261](https://github.com/microsoft/vscode-languageserver-node/issues/1261),
which reports exactly this cadence ("vscode repeats the workspace diagnostic
request every 2 seconds").

This reframes the whole problem: no matter how cheap a single poll is made
server-side (resultId caching, batched `$/progress`, indexed lookups), the
server still has to touch every project and every known document — at minimum a
`GetDependentVersionAsync` call and a document-uri scan per project — **on every
one of these ~2-second ticks, for as long as VS Code is connected**. For a
73-project, several-thousand-file solution that recurring per-project baseline
cost, multiplied by roughly 1800 ticks/hour, is a real, continuous CPU/IO cost
that per-poll micro-optimizations cannot remove — they only make each tick
somewhat cheaper, not less frequent. A live comparison bore this out: CPU stayed
bursty (spikes well over 100%, multi-core) for 20+ minutes after the initial cold
sweep had already completed and the log showed only cache hits, because the
*rate* of polling — not the cost of an individual poll — is what's driving the
sustained load for a solution this size.

### Decision

Given the above, the batching and indexed-lookup changes were **implemented,
verified, and then reverted** (`src/CSharpLanguageServer/Handlers/Diagnostic.fs`
is back to its pre-change state) — the win was real but small relative to the
~2-second, solution-size-independent polling cadence that dominates the cost for
a solution this large. They remain a reasonable future win for solutions with
enough live churn that individual polls (rather than polling frequency) are the
bottleneck, but are not currently justified as a standalone change.

The only lever that actually addresses the *recurring, forever* cost for very
large solutions is **Fix 3 above** (`WorkspaceDiagnostics = false`), since that's
the sole way to stop `pullWorkspace()` from ever starting client-side. That
remains the next thing to evaluate if this keeps being a problem in practice —
e.g. gating it behind a size heuristic (project count) or a user-facing config
option, since disabling it outright loses workspace-wide diagnostics for closed
files.

### References (this update)

- `client/src/common/diagnostic.ts` (`DiagnosticFeatureProviderImpl.pullWorkspace`) —
  https://github.com/microsoft/vscode-languageserver-node/blob/main/client/src/common/diagnostic.ts
- https://github.com/microsoft/vscode-languageserver-node/issues/1261 — "Workspace
  diagnostic pull retries continuously even when sending
  DiagnosticServerCancellationData error"
- Reference session: `~/csharp-ls-rpc.log` against a large real-world solution
  (73 projects), captured after a VS Code restart with both prototyped fixes live
