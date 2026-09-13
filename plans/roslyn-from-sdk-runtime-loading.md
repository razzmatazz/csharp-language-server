# Loading Roslyn Assemblies from the Installed .NET SDK at Runtime

**Status:** Not started. Planning only — no prototype has been built yet.

**Origin:** Extracted and generalized from the "Roslyn-from-SDK question" /
"`ExcludeAssets=runtime` pattern" sections of `plans/razor-sdk-300-investigation.md`, which
hit this as a side effect of debugging Razor source-generator breakage. This document turns
that investigation into a standalone, actionable plan so the architectural question can be
tracked and decided on its own terms instead of being buried in a bug writeup.

---

## Current architecture (baseline, as of Roslyn/Microsoft.CodeAnalysis.* `5.3.0`)

`csharp-ls` runs as a **single in-process** server — there is no separate "build host" or
"language server" split of its own, and it does not use an `AssemblyLoadContext` per solution
or per project. Two independent pieces are loaded very differently:

1. **Roslyn itself** (`Microsoft.CodeAnalysis`, `.CSharp`, `.CSharp.Features`,
   `.Workspaces.MSBuild`, …) — referenced as ordinary `PackageReference`s pinned by
   `RoslynPackageVersion` in `Directory.Packages.props`, and shipped as normal DLLs in the
   `csharp-ls` output/publish folder. This version is fixed at build time and is completely
   independent of whatever .NET SDK is installed on the end-user's machine.

2. **MSBuild itself** (`Microsoft.Build`, `Microsoft.Build.Framework`,
   `Microsoft.Build.Utilities.Core`) — referenced with `ExcludeAssets="runtime"` (compiled
   against, but not shipped). At startup, `Roslyn/Solution.fs: initializeMSBuild` calls
   `MSBuildLocator.QueryVisualStudioInstances` + `MSBuildLocator.RegisterInstance`, which hooks
   `AssemblyLoadContext.Default.Resolving` so that when the CLR fails to find
   `Microsoft.Build*.dll` in the output folder, it loads them from whichever SDK instance was
   selected — this is the [documented, Microsoft-sanctioned pattern](https://learn.microsoft.com/en-us/visualstudio/msbuild/find-and-use-msbuild-versions)
   for exactly this problem. This is genuinely "pulling libs from the MSBuild-resolved SDK at
   runtime" — but only for MSBuild's own ~3 slow-moving assemblies, never for Roslyn.

3. **Project evaluation itself runs out-of-process already, for free.** Since
   `Microsoft.CodeAnalysis.Workspaces.MSBuild` 4.9+ (csharp-ls is on 5.3.0),
   `MSBuildWorkspace.OpenSolutionAsync`/`OpenProjectAsync` spawn a separate **`BuildHost`**
   process (`BuildHost-net472` or `BuildHost-netcore`, chosen per-project) that does the actual
   MSBuild evaluation and talks back over a named pipe / RPC. This was an upstream Roslyn
   change (unifying VS's in-proc path and VS Code's out-of-proc path — see
   [roslyn#76832](https://github.com/dotnet/roslyn/pull/76832)) that csharp-ls inherits simply
   by depending on a recent `Microsoft.CodeAnalysis.Workspaces.MSBuild`; no code in this repo
   implements or manages it. So the "out of process?" half of the original question is already
   answered upstream — yes, for the evaluation step — independent of anything decided here.

4. **`SolutionFile.Parse`/`.slnf` filtering in `Solution.fs`** is the one place csharp-ls calls
   `Microsoft.Build.Construction` types directly in-process (not via BuildHost), reflectively
   (`GetMethod(..., BindingFlags.NonPublic)`) and defensively, precisely because those types
   are "whichever assembly `MSBuildLocator` found in the installed SDK" and can drift.

**What is *not* SDK-resolved today:** Roslyn's own compiler/workspace/IDE-services assemblies.
Those are always the NuGet-pinned `RoslynPackageVersion`, regardless of which .NET SDK is on
the machine or which SDK `MSBuildLocator` picked.

---

## Problem this would solve

**Primary problem: a *future* SDK, installed on a user's machine, breaks an *already-released,
older* csharp-ls build.** Each .NET SDK feature band ships its own
`Microsoft.CodeAnalysis.Razor.Compiler.dll` (and other SDK-bundled analyzers/generators), built
against a specific `Microsoft.CodeAnalysis` assembly version. Once a user upgrades their SDK
past whatever `RoslynPackageVersion` a given csharp-ls release was pinned to at build time,
`AnalyzerFileReference.GetGenerators()` throws `ReflectionTypeLoadException` internally, Roslyn
swallows it, and `GetSourceGeneratedDocumentsAsync()` silently returns zero documents — Razor
support goes dark with no error surfaced anywhere, on a csharp-ls build that worked fine before
the user's SDK upgrade. This has already recurred **three times** against the same pinned
version (SDK 10.0.300 → Roslyn 5.5.0, SDK 10.0.400 → Roslyn 5.9.0, documented in
`plans/razor-sdk-300-investigation.md`) and is expected to keep recurring on every future SDK
band that bumps its bundled Roslyn ahead of the next public NuGet release — i.e. this isn't a
one-off compatibility gap to patch, it's an ongoing tax: *every* csharp-ls release is only
forward-compatible with SDKs up to whatever was current-and-published-on-NuGet at the time it
shipped, and silently degrades (not fails loudly) beyond that. It also affects any other
SDK-bundled analyzer with the same "requires a specific `Microsoft.CodeAnalysis` assembly
version" shape, not just Razor.

Loading Roslyn itself from the installed SDK (instead of NuGet) would make csharp-ls always
lock-step with whatever Razor/analyzer generator that same SDK ships, eliminating this whole
class of forward-breakage permanently instead of requiring a manual `RoslynPackageVersion` bump
every time it recurs — a released csharp-ls binary would keep working with SDKs released after
it, not just SDKs current as of its own release.

**Secondary, lower-priority concern: the fix itself could introduce the mirror-image
problem — breaking *older* SDKs.** If Roslyn is redirected to whatever `MSBuildLocator` selects
at runtime, an SDK *older* than what a given csharp-ls source tree was written/compiled against
could supply a Roslyn assembly missing APIs csharp-ls's own code calls (`MissingMethodException`
/ `TypeLoadException` at runtime instead of a compile-time error), trading today's
forward-compat gap for a new backward-compat gap. This is real but considered less important
than the primary problem for now — see the corresponding item in "Risks" and the matching Phase
1 spike step below, both flagged as lower priority to fully resolve rather than blocking.

---

## Mechanism identified (not yet prototyped)

The investigation found a viable mechanism, by direct analogy to what csharp-ls already does
for MSBuild:

- Every installed .NET SDK ships a **complete, version-matched copy of the full Roslyn
  workspace API surface** — not just the compiler — inside its bundled `dotnet-format` global
  tool (`sdk/<ver>/DotnetTools/dotnet-format/`): `Microsoft.CodeAnalysis.Workspaces.MSBuild.dll`
  (+ `.Contracts.dll`), `.CSharp.Workspaces.dll`, `.CSharp.Features.dll`, `.Workspaces.dll`,
  even `Microsoft.Build.dll`/`.Framework.dll`, plus the `BuildHost-net472`/`BuildHost-netcore`
  subfolders. Confirmed for SDKs 10.0.101 / 10.0.300 / 10.0.400 via direct `AssemblyName`
  inspection — versions matched exactly what each SDK's Razor generator itself requires.
- All these SDK-bundled assemblies share the same public key token
  (`31bf3856ad364e35`) as the NuGet-published `Microsoft.CodeAnalysis.*` packages — so a
  redirect would not cross a strong-name boundary.
- The same `PackageReference ... ExcludeAssets="runtime"` +
  `AssemblyLoadContext.Default.Resolving` pattern already used for `Microsoft.Build*` could in
  principle be extended to the `Microsoft.CodeAnalysis.*` packages, redirecting them to the
  `dotnet-format` folder of whichever SDK `MSBuildLocator` selected. Because this hooks the
  **default** load context (not a separate isolated one), there would be exactly one loaded
  copy of each assembly — avoiding the "type identity hazard" of mixing NuGet-loaded and
  SDK-loaded `Solution`/`Compilation` instances, *provided the redirect is total* (no partial
  adoption — see risks below).

---

## Precedent from other C# LSP implementations

Both other real-world C# language servers were checked directly and land on the **same split
csharp-ls already uses** — Roslyn bundled/pinned, MSBuild dynamically resolved from whatever SDK
is installed via `MSBuildLocator`. Neither does runtime Roslyn-from-SDK loading.

- **`Microsoft.CodeAnalysis.LanguageServer`** (the official Roslyn LSP behind VS Code's C#
  extension / C# Dev Kit, also wrapped by third-party tools like `roslyn.nvim`, `csharp.nvim`,
  `SofusA/csharp-language-server`): ships its own Roslyn, self-contained in the sense of
  bundling the DLLs, but framework-dependent (needs a `dotnet` runtime already on the machine).
  It is not part of the .NET SDK install and not published to public NuGet as a general-purpose
  package — third-party wrappers download a specific OS/arch build directly. Each package is
  built from the exact same `dotnet/dotnet` monorepo commit as the Roslyn compiler/IDE-services
  NuGet packages for that release, so its bundled Roslyn is *by construction* never mismatched
  with itself — a different mitigation strategy than "load from SDK": tight, same-commit
  coupling plus frequent releases, rather than a runtime redirect. For MSBuild, it uses
  `MSBuildLocator` exactly like csharp-ls does (confirmed call chain in a crash log:
  `LanguageServerProjectSystem.TryEnsureMSBuildLoadedAsync → MSBuildLocator.GetInstances →
  DotNetSdkLocationHelper.GetDotNetBasePaths`), and — because it depends on the same
  `Microsoft.CodeAnalysis.Workspaces.MSBuild` package family — gets the identical out-of-process
  `BuildHost` split for project evaluation that csharp-ls does; that mechanism was largely built
  *for* this server's own out-of-process use case in the first place (see
  [roslyn#76832](https://github.com/dotnet/roslyn/pull/76832)).
- **OmniSharp**: identical NuGet-pinned-Roslyn + `MSBuildLocator`-for-MSBuild split, via its own
  `OmniSharp.MSBuild.Discovery.MSBuildLocator` wrapper (log output has the same shape as
  csharp-ls's `initializeMSBuild`: `Located N MSBuild instance(s)` / `Registered MSBuild
  instance: ...`). OmniSharp additionally ships its own bundled "StandAlone" MSBuild as a
  last-resort fallback when no VS/SDK instance is found — and that fallback is a **real-world,
  already-happened instance of risk #2 below**: when `MSBuildLocator` picks the bundled
  stand-alone MSBuild instead of the actual installed .NET Core SDK's MSBuild, project `Sdk=`
  resolution breaks because the bundled copy has no `Sdks` folder at
  `MSBuildSDKsPath` — [OmniSharp/omnisharp-roslyn#1613](https://github.com/OmniSharp/omnisharp-roslyn/issues/1613)
  — fixed only by a `UseLegacySdkResolver` opt-in that shells out to the `dotnet` CLI to find
  the real SDK path. This is direct, shipped-product evidence for "two SDKs (or an SDK vs. a
  bundled fallback) can disagree," not just a theoretical risk.

**Reading on the primary problem this plan is about (forward breakage on already-released
builds):** neither precedent actually solves it by loading Roslyn from the SDK — Roslyn LSP
avoids it via same-commit builds + release cadence tightly tracking the SDK/Roslyn train (an
option not really available to a third-party, independently-released project like csharp-ls),
and OmniSharp has historically been affected by the same class of SDK/analyzer version-skew
issues csharp-ls sees with Razor (see e.g. version-mismatch discussions in
[dotnet/msbuild#7832](https://github.com/dotnet/msbuild/issues/7832), which is the same
"Roslyn ships with MSBuild/SDK, analyzer ships with SDK, versions drift apart" shape as the
Razor generator problem here, just for CodeStyle analyzers instead of Razor).

---

## Risks / open concerns (why this is not a quick toggle)

1. **Bigger blast radius than MSBuild.** MSBuildLocator redirects ~3 slow-moving assemblies.
   Redirecting Roslyn means redirecting 10+ (`Microsoft.CodeAnalysis[.CSharp[.Workspaces|
   .Features]]`, `.Workspaces[.MSBuild[.Contracts]]`, `.Features`, `.Elfie`, `.Scripting`,
   `.ExternalAccess.RazorCompiler`, …) **plus** their own transitive dependencies, which the SDK
   bundle carries its own copies of (`Humanizer`, `System.Composition.*`,
   `System.Reflection.MetadataLoadContext`, `Newtonsoft.Json`/`System.Text.Json`, …) — any of
   which could be at a different version than what csharp-ls itself depends on. A
   [MSBuildLocator maintainer thread](https://github.com/microsoft/MSBuildLocator/issues/127)
   documents exactly this failure mode even for the narrow MSBuild case, requiring transitive
   deps to be routed into a *second*, separate `AssemblyLoadContext` while keeping only the
   primary assembly in `Default`.
2. **Two independently-resolved SDKs can disagree.** `MSBuildLocator.RegisterInstance` picks
   the *engine* SDK (used for the `Default` load-context redirect target). The MSBuild `.NET SDK
   resolver` inside a design-time build picks the *project* SDK independently, by walking up
   from the project directory for `global.json` — these are not guaranteed to be the same SDK
   (this exact mismatch is what originally triggered the recurring Razor breakage). Any
   Roslyn-from-SDK redirect would need to decide which of the two SDKs' Roslyn to bind to, and
   that decision interacts with which one the Razor/analyzer DLL itself was compiled against.
   OmniSharp hit a variant of this in production — see "Precedent" above.
3. **Undocumented, unsupported implementation detail.** The `dotnet-format` bundle is not a
   documented or supported extensibility point — Microsoft could relocate, rename, trim, or
   remove it from a future SDK without notice, unlike NuGet packages with SemVer guarantees.
   Depending on it means re-verifying the layout on every new SDK major/feature band.
4. **All-or-nothing.** Because of the type-identity hazard, this only works if csharp-ls stops
   shipping *any* NuGet-loaded copy of the redirected assemblies — a partial/gradual rollout is
   not viable; it's a single cutover.
5. **No known production precedent, including from the team best positioned to do it.**
   `Microsoft.CodeAnalysis.LanguageServer` (the official Roslyn LSP behind VS Code's C#
   extension / C# Dev Kit) is a separately-versioned, self-contained build with its own pinned
   Roslyn — it does **not** load Roslyn from the installed SDK, despite Microsoft owning both
   Roslyn and the SDK's Razor generator. It *does* use `MSBuildLocator` for the MSBuild engine
   only — i.e., today's csharp-ls split (MSBuild from SDK, Roslyn pinned) already matches what
   Microsoft's own production LSP server does. This is a meaningful signal that the
   effort/risk of Roslyn-from-SDK loading has not been judged worthwhile even internally.
6. **(Lower priority) Mirror-image backward-compat breakage.** As noted in "Problem this would
   solve": redirecting to whatever SDK is installed cuts both ways. A machine with only an
   *older* SDK than the Roslyn API surface csharp-ls's own source was written against would
   supply a Roslyn assembly missing methods/types csharp-ls calls — a `MissingMethodException`
   or `TypeLoadException` at runtime, on a codebase that compiled fine, instead of today's
   caught-at-build-time `PackageReference` version floor. Practically this matters less than
   risk items 1–5: csharp-ls could still declare a documented *minimum* supported SDK version
   (analogous to `RoslynPackageVersion` today, just reframed as a floor instead of an exact
   pin), and this only bites installations below that floor — a smaller, more controllable
   surface than the current *unbounded* forward-compat gap this plan exists to close. Not a
   blocker, but Phase 1 should at least observe what actually happens (clean exception vs.
   silent degradation vs. crash) so the failure mode is known before committing.

---

## Decision so far

**Not adopted.** The lower-effort mitigation — bumping `RoslynPackageVersion` in
`Directory.Packages.props` once the SDK's required Roslyn version reaches public stable NuGet —
has resolved the recurrence twice already and remains the near-term strategy. Roslyn-from-SDK
loading stays on the table as a candidate *permanent* fix, gated on a time-boxed spike (below)
before any architectural commitment, per the "Iteration Style" note in `AGENTS.md" — but this
is exactly the kind of structural change that should be scoped and proven in isolation first,
not driven by `dotnet test` churn in the main tree.

---

## Plan

### Phase 0 — Trigger / re-evaluation criteria (do this instead of a spike, for now)

Don't start Phase 1 speculatively. Revisit this plan when *any* of:
- A **released** csharp-ls build (not just the dev tree) is reported broken by a user who
  upgraded their .NET SDK after installing that csharp-ls version — i.e. the primary problem
  actually reaches an end user, not just CI/local dev tracking SDK previews. This is the
  framing that matters most: today's mitigation (bumping `RoslynPackageVersion` before the next
  *release*) doesn't help someone already running an older release against a newly-upgraded
  SDK.
- The SDK/NuGet Roslyn version gap recurs a third time in dev/CI (i.e., the situation described
  in `plans/razor-sdk-300-investigation.md`'s "Update (2026-08)" happens again), **or**
- A new .NET SDK ships with its required Roslyn version *not yet available on public NuGet even
  as prerelease*, making the "bump `RoslynPackageVersion`" mitigation unavailable for an
  extended period, **or**
- Another SDK-bundled analyzer/generator (beyond Razor) is found to hit the same
  `GetGenerators() == 0` failure mode, widening the blast radius of doing nothing.

### Phase 1 — Time-boxed spike (standalone, outside the main tree)

Goal: answer "does this actually work end-to-end, and is the transitive-dependency graph
clean?" — cheaply, without touching `csharp-ls` source.

1. Write a **standalone console probe** (not part of `CSharpLanguageServer.fsproj`) that:
   - Runs `MSBuildLocator.RegisterInstance` as today, to pick an SDK.
   - Locates that SDK's `DotnetTools/dotnet-format/` folder.
   - Hooks `AssemblyLoadContext.Default.Resolving` to redirect `Microsoft.CodeAnalysis*`
     (and their transitive deps) to that folder.
   - Calls `MSBuildWorkspace.Create()`, opens a real solution (reuse an existing test fixture,
     e.g. `aspnetProject`), and calls `GetSourceGeneratedDocumentsAsync()` on the Razor-using
     project — confirming Razor generation actually works purely via the SDK-loaded Roslyn.
2. Explicitly test the transitive-dependency question: does the probe hit
   `FileLoadException`/`ReflectionTypeLoadException` for any of `Humanizer`,
   `System.Composition.*`, `System.Reflection.MetadataLoadContext`, or the JSON library, given
   csharp-ls's own dependency graph (post `system-text-json-migration.md`) also references some
   of these transitively? If so, prototype the "route transitive deps through a second,
   non-default `AssemblyLoadContext`" workaround from the MSBuildLocator issue thread.
3. Test the two-SDK-disagreement scenario (risk #2 above) deliberately: a fixture with no
   `global.json`, multiple SDKs installed, MSBuildLocator engine SDK ≠ project-SDK-resolver's
   chosen SDK — confirm which Roslyn version actually ends up governing generator loading, and
   whether the redirect needs to key off the *project's* resolved SDK rather than the *engine*
   SDK MSBuildLocator picked.
4. (Lower priority, risk #6) Point the probe at the *oldest* supported SDK instead of the
   newest, and confirm what actually happens when csharp-ls source uses a Roslyn API newer than
   what that SDK's `dotnet-format` bundle provides — clean exception at the call site, a load
   failure at startup, or something silent. This just needs to be observed once, not solved in
   this phase; it informs whether Phase 3 needs a minimum-SDK-version guard.
5. Record findings (works / doesn't / partial) directly in this file's "Findings" section
   below — do not implement anything in `src/` during this phase.

**Explicit non-goal for Phase 1:** no change to `CSharpLanguageServer.fsproj`,
`Directory.Packages.props`, or `Solution.fs`. The probe lives outside the tracked build.

### Phase 2 — Go / no-go decision

Only after Phase 1 findings are recorded. Go criteria (all must hold):
- The probe's `GetSourceGeneratedDocumentsAsync()` result matches the NuGet-Roslyn baseline on
  at least two different SDK feature bands.
- No unresolved transitive-dependency conflicts, or a working, documented mitigation exists.
- A clear, testable answer to which SDK (engine vs. project-resolved) governs the redirect,
  covering the `global.json`-absent case that caused the original Razor recurrence.

If "no-go": close this plan with the finding recorded, keep the `RoslynPackageVersion`-bump
mitigation as the permanent strategy, and revisit only if the SDK/NuGet cadence problem gets
materially worse.

### Phase 3 — Implementation (only if Phase 2 is "go")

Not detailed yet — to be scoped once Phase 1/2 results are in. Expected shape, for reference
only:
- Switch all `Microsoft.CodeAnalysis.*` `PackageReference`s used by
  `CSharpLanguageServer.fsproj` to `ExcludeAssets="runtime"`, mirroring the existing
  `Microsoft.Build*` treatment.
- Extend `initializeMSBuild` (`Roslyn/Solution.fs`) with a matching
  `AssemblyLoadContext.Default.Resolving` hook for the Roslyn assembly set, sourced from the
  same SDK instance already selected for MSBuild (pending the Phase 1 finding on which SDK
  should govern this).
- Add a `$/csharp/debugInfo` field reporting the actual bound `Microsoft.CodeAnalysis` version
  and its source path, so version mismatches are diagnosable instead of silent (this is
  independently useful — see `plans/TODO.md`'s "Analyzer support improvements" and
  Fix option 3 in `plans/razor-sdk-300-investigation.md`).
- CI coverage across at least two SDK feature bands (matrix), since this is precisely the axis
  that broke silently before.
- A rollback story: since the SDK-bundled `dotnet-format` layout is unsupported/undocumented,
  the resolver hook must degrade gracefully (fall back to the NuGet-shipped assemblies actually
  present in the output dir, if `ExcludeAssets="runtime"` implementation ships them as a
  fallback, or fail fast with a clear diagnostic) if the expected `dotnet-format` folder/layout
  is missing on a given SDK install.

---

## Findings

_(empty — populate here once Phase 1 is executed)_

---

## References

- `plans/razor-sdk-300-investigation.md` — origin of this analysis; full history of the
  recurring Razor/SDK version-mismatch bug this would solve.
- `docs/codebase-architecture.md` §2 (Key Dependencies) — current Roslyn/MSBuild package
  pinning.
- `src/CSharpLanguageServer/Roslyn/Solution.fs: initializeMSBuild` — existing MSBuildLocator
  usage this plan would extend.
- [roslyn#76832](https://github.com/dotnet/roslyn/pull/76832) — Roslyn's own out-of-process
  `BuildHost` unification (already inherited by csharp-ls via `Microsoft.CodeAnalysis.Workspaces.MSBuild`
  5.3.0; independent of this plan).
- [MSBuildLocator#127](https://github.com/microsoft/MSBuildLocator/issues/127) — precedent for
  transitive-dependency conflicts when redirecting a package family via `AssemblyLoadContext`.
- [Find and use MSBuild versions](https://learn.microsoft.com/en-us/visualstudio/msbuild/find-and-use-msbuild-versions) —
  documented pattern csharp-ls already uses for MSBuild, and would extend to Roslyn.
- [OmniSharp/omnisharp-roslyn#1613](https://github.com/OmniSharp/omnisharp-roslyn/issues/1613) —
  real-world instance of risk #2 (bundled-fallback MSBuild vs. installed SDK disagreeing on
  which one governs `Sdk=` resolution).
- [dotnet/msbuild#7832](https://github.com/dotnet/msbuild/issues/7832) — same "Roslyn ships
  with MSBuild/SDK, analyzer ships with SDK, versions drift apart" shape as this plan's Razor
  problem, for CodeStyle analyzers instead.
- [SofusA/csharp-language-server](https://github.com/SofusA/csharp-language-server),
  [msiarko/roslyn-ls](https://github.com/msiarko/roslyn-ls) — third-party wrappers around
  `Microsoft.CodeAnalysis.LanguageServer`, illustrating how it's distributed/versioned
  independently of the .NET SDK.
