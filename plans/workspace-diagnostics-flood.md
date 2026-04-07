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
