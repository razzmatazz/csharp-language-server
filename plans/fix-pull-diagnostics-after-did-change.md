# Fix: stale pull diagnostics after `textDocument/didChange`

## Symptom

`testPullDiagnosticsWork` fails on `use-system-text-json-v` (introduced by commit `ad4270e2`
"chore: convert Ionide.LSP serializers to use STJ").

After `classFile.Change("")`, a second `textDocument/diagnostic` request returns the **same
`resultId`** as the first, and still returns 3 diagnostic items instead of 0.  The test asserts
`report.Items.Length = 0`.

The same test passes on `28f67bba` (parent of the STJ commit).

---

## How `resultId` works

`resultId` for pull diagnostics is:

```fsharp
let private projectResultId (analyzersEnabled: bool) (project: Project) =
    sprintf "%s/%b" (string project.Version) analyzersEnabled
```

`project.Version` is Roslyn's `VersionStamp` — bumped every time document text is replaced via
`doc.WithText(newText)`.  If the second `textDocument/diagnostic` returns the same `resultId`,
the diagnostic handler read a `Solution` whose `project.Version` had not yet been updated —
i.e. it saw the **pre-change** workspace state.

---

## Investigation — what the logging trace showed

Running `testPullDiagnosticsWork` with `activateFixtureWithLoggingEnabled` and `eprintfn` probes
in `wrapHandler`, `didChange`, and `didChangeCsharpFile`:

```
[+003147] WRITE: textDocument/didChange  {"contentChanges":[{"text":""}]}
[+003147] WRITE: textDocument/diagnostic
[+003147] [DEBUG wrapHandler] method=textDocument/didChange  ← server receives it, no exception
[+003155] [DEBUG didChange] uri=… version=2 changes=1       ← deserialized correctly
[+003157] [DEBUG didChangeCsharpFile] numChanges=1          ← handler body runs
[+003157] [DEBUG didChangeCsharpFile]   change[0]: ranged text=""  ← U2.C1 branch, Range=None
[+003159] [DEBUG wrapHandler] method=textDocument/diagnostic
[+003160] READ: result {"resultId":"2026-01-14T…-10002-0/false", "items":[…3 items…]}
```

Key observations:

1. **`textDocument/didChange` was received and deserialized without exception.**  It ran and
   completed (confirmed by `$/csharp/debugInfo` stats: `textDocument/didChange count=1`).

2. **The second `textDocument/diagnostic` returned the same `resultId`** — Roslyn's
   `project.Version` did not change between the two diagnostic calls, so the handler read the
   old `Solution`.

3. **The workspace `generation` Guid never changed** across the whole test run, confirming no
   solution reload occurred — the in-memory solution was simply not updated before the diagnostic
   ran.

---

## Root cause — scheduler race in `ApplyWorkspaceUpdate`

When `didChange`'s handler returns, it posts `LeaveRequestContext(ordinal, wsUpdate)` where
`wsUpdate` contains `FolderUpdates = [workspaceFolderDocumentTextUpdated; workspaceFolderDocOpened]`.

These update functions replace `wf.Solution` with the Roslyn `Solution` produced by
`doc.WithText(newSourceText)` — the one with the bumped `project.Version`.

The update functions travel through the state-actor mailbox before reaching the workspace:

```
Handler returns
  → LeaveRequestContext(N, wsUpdate)
      finishRequest(N)
      postServerEvent ProcessRequestQueue         ← PQ1

PQ1: retireNextFinishedRequest → Retired
      postServerEvent (ApplyWorkspaceUpdate wsUpdate)    ← AWU
      postServerEvent ProcessRequestQueue                ← PQ2

AWU: posts each folder update as a separate event
      postServerEvent (WorkspaceFolderUpdates(uri, fns)) ← WFU
      postServerEvent PushDiagnosticsBacklogUpdate

mailbox queue at this point: [PQ2, WFU, PDBU]
                               ↑ runs first
PQ2: canActivateRequest → diagnostic is ReadOnly, no running write → Activated
      sends RequestContext reply to diagnostic handler
      postServerEvent ProcessRequestQueue

WFU: updatedWf = fns |> List.fold (|>) wf   ← new Solution committed HERE
      workspace <- workspaceFolderUpdated updatedWf
```

`PQ2` activates the diagnostic handler **before** `WFU` commits the new `Solution`.  The
diagnostic handler then calls `PostAndAsyncReply(LoadWorkspaceFolder)` which resolves against
whatever workspace state is current at the time `ProcessSolutionAwaiters` fires.  If `WFU` has
not yet run, it sees the old `project.Version` and returns the cached result.

This race was present before the STJ commit too, but was masked: on the old code path the
`textDocument/didChange` notification was **silently failing** (not appearing in the request
stats), so the workspace was never updated at all and both diagnostic calls returned identical
results — the test happened to pass because it only checked the first call's 3-item result, not
the second call's 0-item result.  After the STJ commit `didChange` started working correctly,
exposing the race.

---

## Proposed solutions

### Option A — Apply `FolderUpdates` inline in `ApplyWorkspaceUpdate` (recommended)

Instead of posting `WorkspaceFolderUpdates` as a separate mailbox event, apply the update
functions synchronously inside the `ApplyWorkspaceUpdate` handler — using the `workspace`
mutable local that already exists there — and only then post `ProcessRequestQueue`.

This re-orders the mailbox sequence to:

```
AWU: applies folder updates inline → workspace is current
     postServerEvent PushDiagnosticsBacklogUpdate
     postServerEvent ProcessRequestQueue   ← moved here, after workspace is committed

PQ: Activated(diagnostic) → handler reads the already-updated Solution ✅
```

Changes required:

- **`ServerStateLoop.fs` / `ApplyWorkspaceUpdate`**: replace
  ```fsharp
  wsUpdate.FolderUpdates
  |> Map.toSeq
  |> Seq.iter (fun (wfUri, wfUpdates) -> postServerEvent (WorkspaceFolderUpdates(wfUri, wfUpdates)))
  do postServerEvent PushDiagnosticsBacklogUpdate
  ```
  with an inline fold over `workspace` (the mutable local), then post `PushDiagnosticsBacklogUpdate`
  and `ProcessRequestQueue` afterwards.

- **`ServerStateLoop.fs` / `ProcessRequestQueue → Retired`**: remove the `postServerEvent
  ProcessRequestQueue` that currently follows `postServerEvent (ApplyWorkspaceUpdate ...)`.
  `ProcessRequestQueue` is now posted by `ApplyWorkspaceUpdate` itself, guaranteeing it runs
  after the workspace is up-to-date.

The `WorkspaceFolderUpdates` event handler itself remains — other code paths (`WorkspaceSolutionLoadCompleted`,
`WorkspaceReloadRequested`, etc.) post it directly and still need it.

**Pros:** fixes the race definitively at the scheduler level; any future `ReadOnly` handler is
guaranteed to see the effects of any preceding `ReadWrite` handler, not just for `didChange`.

**Cons:** `ApplyWorkspaceUpdate` grows slightly in responsibility; the symmetry between
`WorkspaceFolderUpdates`-as-event and `WorkspaceFolderUpdates`-as-inline is mildly surprising.

---

### Option B — Move `ProcessRequestQueue` to the tail of `WorkspaceFolderUpdates`

Keep `WorkspaceFolderUpdates` as a separate event, but stop posting `ProcessRequestQueue` from
`Retired` and instead post it from the `WorkspaceFolderUpdates` handler (after the last folder
is updated).

Problem: there may be multiple `WorkspaceFolderUpdates` events (one per folder in a
multi-folder workspace), and we only want to post `ProcessRequestQueue` once, after the last
one.  This would require tracking how many folder-update events are outstanding, which adds
bookkeeping complexity.  Not recommended.

---

### Option C — Add a new `Applying` request phase

Introduce a fourth `RequestPhase` value `Applying` (between `Running` and `Finished`).  The
request transitions `Running → Applying` when the handler posts `LeaveRequestContext`, and
`Applying → Finished` only after its `wsUpdate` has been committed (i.e. after `WorkspaceFolderUpdates`
processes the last update function).

Update `canActivateRequest` to treat `Applying` like `Running` for the purposes of the
`hasRunningWrite` guard.

This is the most architecturally pure version of the "don't finishRequest until the update is
in state" idea, but it requires the most structural change: new phase variant, new transitions,
new guard logic, and a mechanism to identify which `WorkspaceFolderUpdates` event corresponds to
which request.

**Pros:** makes the lifecycle semantics explicit and self-documenting.
**Cons:** significantly more code; the simpler Option A achieves the same observable guarantee.

---

## Recommendation

**Option A.**  It is the smallest correct fix: a handful of lines moved from
`ProcessRequestQueue → Retired` into `ApplyWorkspaceUpdate`, with no new types, no new events,
and no new invariants to maintain.  The plan file should be updated to "Applied" once the
change is landed and `testPullDiagnosticsWork` passes.

---

## Verification

```bash
dotnet test tests/CSharpLanguageServer.Tests/ \
  --filter "FullyQualifiedName~DiagnosticTests" \
  --logger "console;verbosity=normal"
```

`testPullDiagnosticsWork` must pass.  The two pre-existing non-passing tests are unrelated:
- `testPushDiagnosticsWork` — flaky push-diagnostics timing test (`[<Retry(3)>]`)
- `testWorkspaceDiagnosticsWhileSolutionIsLoading` — intentionally fails to document a known race
