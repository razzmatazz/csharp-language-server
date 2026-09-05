# Support Loose `.cs` Files Without a `.sln`/`.csproj` (#188)

Tracking issue: [#188](https://github.com/razzmatazz/csharp-language-server/issues/188)
"Autocompletion without .csproj / .sln files" — the reporter wants to open a folder of
plain `.cs` files (no project/solution) and still get completion/hover/etc. A follow-up
comment extends the ask to .NET 10 **file-based apps** (`dotnet run app.cs`, `#!`
shebangs, `#:` directives) introduced in the .NET 10 SDK.

These are really two flavours of the same underlying problem — "there is no
`.csproj`/`.sln` on disk" — so this plan handles both with one mechanism: when no
project/solution is discovered, synthesize one or more **`.csproj` files** in a temp
directory and feed them through the existing project-loading path. No new Roslyn
concepts are needed; `MSBuildWorkspace` already knows how to open a `.csproj`, we just
need to manufacture one when the user hasn't.

**Important revision:** the .NET 10 SDK already ships an authoritative tool for exactly
this translation — [`dotnet project convert`](https://learn.microsoft.com/en-us/dotnet/core/tools/dotnet-project-convert)
turns a file-based app's `#:` directives into a real `.csproj` on disk. Rather than
hand-writing (and maintaining, as directive syntax evolves across SDK releases) our own
`#:` directive parser and MSBuild-XML generator, §3/§4 below shell out to this command
and only patch the one path it needs redirected. This removes almost all of the
implementation risk this plan originally carried — we're not reverse-engineering
`VirtualProjectBuildingCommand`, we're just invoking the CLI command the SDK team already
built and maintains for this exact purpose. Hand-rolled `.csproj` generation is only
still needed for the plain "no directives at all" bucket (§2a), where there is no
directive translation to delegate in the first place.

---

## 1 — Current behaviour (why it fails today)

`solutionFindAndLoadOnDir` (`Roslyn/Solution.fs`) is the fallback used whenever no
`--solution` CLI override is given:

1. Looks for `*.sln` / `*.slnx` under the workspace folder → loads via
   `solutionTryLoadOnPath`.
2. Otherwise looks for `*.csproj` / `*.fsproj` → loads via `solutionTryLoadFromProjectFiles`.
3. **Otherwise raises `Exception "no or .csproj/.fsproj or sln files found on " + dir`.**

That exception propagates out of `workspaceFolderSolutionLoad` as `None`, which sets
`wf.Solution = Defunct "..."`. Every handler that needs a solution
(`GetWorkspaceFolderReadySolution`, `workspaceFolderDocument`, etc.) then has nothing to
work with — hence no completion, no hover, nothing. This is the exact failure the issue
reports.

---

## 2 — Two use cases, one mechanism

### 2a. Plain "loose files" folder (the original issue)

A directory with `.cs` files and no directives at all — someone just wants to poke
around C# without ceremony. Expectation: files can see each other (a `class Foo` in
`a.cs` should resolve from `b.cs` in the same folder), i.e. one shared project.

### 2b. File-based apps (the follow-up comment)

Per the [.NET 10 file-based apps feature](https://learn.microsoft.com/en-us/dotnet/core/sdk/file-based-apps),
each entry-point `.cs` file (optionally starting with a `#!` shebang line) can carry
`#:` directives — `#:sdk`, `#:package`, `#:property`, `#:project`, `#:include` — that the
SDK expands into an in-memory "virtual" `.csproj` before invoking MSBuild
(`Microsoft.DotNet.Cli.Commands.Run.VirtualProjectBuildingCommand`). Crucially, **each
file with directives is its own isolated project** — the SDK sets
`EnableDefaultCompileItems=false` and only compiles that one entry-point file (plus
anything pulled in via `#:include`), specifically so that several file-based apps can
live side by side in one folder without seeing each other.

### Partitioning rule

When no `.sln`/`.csproj`/`.fsproj` is found under a workspace folder, scan for `.cs`
files (same `node_modules`-exclusion / `SearchOption.AllDirectories` convention as the
existing `.csproj`/`.fsproj` scan) and partition them:

- Any file whose first non-blank lines contain a `#!` shebang or a `#:` directive line →
  gets its **own** virtual project (mirrors real file-based-app semantics; `#:package`,
  `#:sdk`, etc. are honored).
- Every remaining plain file → grouped into **one shared "loose files" virtual project**
  per workspace folder, so simple exploratory scripts keep seeing each other (this is a
  deliberate simplification vs. real `dotnet run`, chosen to match what issue #188
  actually asked for).

Both kinds of virtual project are just `.csproj` files written to a temp directory, then
handed to the already-existing `solutionTryLoadFromProjectFiles` — no changes needed to
how `MSBuildWorkspace` itself is invoked.

---

## 3 — Classifying files (cheap, still hand-rolled)

We still need a *cheap, local* check to decide which of the two buckets (§2a/§2b) a file
falls into, before deciding whether it's worth shelling out to the SDK at all. This is
just a classification predicate, not a directive parser:

```fsharp
// Roslyn/FileBasedApps.fs
let hasFileLevelDirectives (text: string) : bool =
    // true if the file opens with a `#!` shebang or has at least one `#:` line
    // among its leading blank/comment lines — good enough to route to §4a vs §4b.
    // Does *not* need to understand directive contents; `dotnet project convert`
    // (§4a) is the source of truth for that.
    ...
```

---

## 4 — Generating a `.csproj` per bucket

### 4a. File-based apps (§2b) — delegate to `dotnet project convert`

For each file where `hasFileLevelDirectives` is `true`, shell out to the .NET 10 SDK's
own converter instead of re-implementing `#:` directive semantics:

```
dotnet project convert <entryFilePath> --output <scratchDir> --force
```

- `--output <scratchDir>` (a fresh, unique temp directory — see below) means the
  generated `.csproj` **and** its copy of the source land in our own scratch area, never
  next to the user's real file — `dotnet project convert` normally writes a sibling
  folder in the file's own directory, `--output` is exactly the documented escape hatch
  for redirecting that.
- `dotnet project convert` <cite>"creates a .csproj file with equivalent SDK items,
  properties, and package references based on the original file's #: directives"</cite>
  — this is precisely the translation this plan originally proposed hand-rolling in §3/§4
  of the earlier draft. Relative `#:project`/`#:include` paths are resolved by the SDK
  itself against the *original* file's directory (the tool's whole job is converting a
  file-based app in place to a project elsewhere), so no path-rewriting logic is needed
  for those.
- **One patch is still required**: `project convert` copies the source into
  `<scratchDir>/<name>.cs` rather than referencing the original path. After running the
  command, read the generated `.csproj`, and rewrite its single default `<Compile
  Include="...">` item to point at the **original absolute `entryFilePath`** instead of
  the copy. This is the one thing we still do by hand — a single string replace, not
  directive parsing — and it's what keeps `didChange`/`didSave` working against the
  file the editor actually has open (same rationale as the original draft's "reference
  the real file, don't copy it").
- Requires the installed `dotnet` to be SDK 10.0.100+ (`dotnet project convert` doesn't
  exist on older SDKs). If the process fails to start or exits non-zero (older SDK, or
  any other conversion error), log it via the existing `progressReporter`/`logMessage`
  plumbing and **skip that file** rather than failing the whole workspace load — one
  broken file-based app shouldn't prevent loading the rest of the loose files (see
  "Open questions / risks" below for exact fallback behaviour).
- Pass `--force` since we always target a fresh unique scratch dir per entry file (named
  from a stable hash of `entryFilePath`, mirroring the original draft's naming scheme),
  so a stale leftover from a previous crashed run is the only thing it would ever
  overwrite.

```fsharp
// Roslyn/FileBasedApps.fs
let convertFileBasedAppToProject
    (dotnetExe: string)      // "dotnet", or the muxer path MSBuildLocator resolved
    (scratchDir: string)
    (entryFilePath: string)
    : Async<string option> =  // returns the patched .csproj path, or None on failure
    ...
```

### 4b. Plain loose files (§2a) — still hand-rolled, but trivial

Files with **no** directives at all have nothing for `dotnet project convert` to
translate, so a minimal hand-written `.csproj` is simplest here — no SDK version drift
risk since there's no directive syntax involved:

```fsharp
let plainLooseFilesProjectXml (defaultTfm: string) (files: string list) : string =
    // <Project Sdk="Microsoft.NET.Sdk">
    //   <PropertyGroup>
    //     <TargetFramework>{defaultTfm}</TargetFramework>
    //     <OutputType>Exe</OutputType>
    //     <ImplicitUsings>enable</ImplicitUsings>
    //     <Nullable>enable</Nullable>
    //     <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
    //   </PropertyGroup>
    //   <ItemGroup>
    //     <Compile Include="{absolute path}" /> ... one per file, all absolute paths
    //   </ItemGroup>
    // </Project>
    ...
```

`defaultTfm` still needs to come from *somewhere*. Rather than tracking SDK version
numbers ourselves, reuse §4a's machinery: run `dotnet project convert` once against a
throwaway empty `.cs` file (or simply invoke `dotnet new console --dry-run`-equivalent —
whichever is cheaper to shell out to) purely to read back its generated `TargetFramework`
value, cache it for the process lifetime, and reuse it for every plain-loose-files
project. This still avoids hard-coding "net{major}.0" guesses or capturing
`MSBuildLocator`'s picked `VisualStudioInstance.Version` ourselves — one less thing to
keep in sync with SDK releases. (If .NET 10+ isn't installed at all, so neither `dotnet
project convert` nor any file-based-app support exists, fall back to a conservative
hard-coded floor, e.g. `net8.0`, purely for this plain-files bucket — file-based apps
remain entirely unavailable on older SDKs regardless.)

`writeProjectFile (virtualProjectsDir: string) (name: string) (xml: string) : string`
writes to `Path.Combine(virtualProjectsDir, name + ".csproj")` and returns the path.

---

## 5 — Wiring into `solutionFindAndLoadOnDir`

```fsharp
// Roslyn/Solution.fs
let solutionFindAndLoadOnDir progressReporter lspClient dir = async {
    ...
    match preferredSlnFile with
    | None ->
        ...
        let projFiles = ... // existing .csproj/.fsproj scan

        if projFiles.Length = 0 then
            let looseCsFiles =
                Directory.GetFiles(dir, "*.cs", SearchOption.AllDirectories)
                |> Seq.filter fileNotOnNodeModules
                |> Seq.filter fileNotUnderBinOrObj   // new helper, mirrors existing filters
                |> List.ofSeq

            match looseCsFiles with
            | [] ->
                let message = "no .sln/.slnx/.csproj/.fsproj or loose .cs files found on " + dir
                do! logMessage message
                Exception message |> raise
            | csFiles ->
                let fileBasedApps, plainFiles =
                    csFiles |> List.partition (File.ReadAllText >> hasFileLevelDirectives)

                let scratchDir = virtualProjectsDirFor dir   // temp dir, see below

                let! fileBasedAppProjects =
                    fileBasedApps
                    |> List.map (convertFileBasedAppToProject "dotnet" scratchDir)
                    |> Async.Sequential   // one conversion at a time is plenty; these are cheap, offline, static translations
                    |> Async.map (Array.choose id >> List.ofArray)

                let! sharedLooseFilesProject =
                    match plainFiles with
                    | [] -> async.Return []
                    | files -> async {
                        let! defaultTfm = defaultTfmFromSdk "dotnet" scratchDir
                        return [ plainLooseFilesProjectXml defaultTfm files
                                 |> writeProjectFile scratchDir "LooseFiles" ]
                      }

                let allVirtualProjects = fileBasedAppProjects @ sharedLooseFilesProject

                do! logMessage (sprintf "no project/solution files found on %s; treating %d loose .cs file(s) as %d project(s)"
                                        dir csFiles.Length allVirtualProjects.Length)

                match allVirtualProjects with
                | [] ->
                    // every file-based-app conversion failed and there were no plain files
                    Exception (sprintf "found %d loose .cs file(s) on %s but none could be loaded" csFiles.Length dir) |> raise
                | projects -> return! solutionTryLoadFromProjectFiles lspClient logMessage progressReport projects

        else
            ... // existing "load projFiles" path, unchanged
    | Some solutionPath -> return! solutionTryLoadOnPath lspClient solutionPath
}
```

`virtualProjectsDirFor dir` returns a stable per-workspace-folder temp path, e.g.
`Path.Combine(Path.GetTempPath(), "csharp-ls", "virtual-projects", <hash of dir>)`. Clean
it up in `workspaceFolderTeardown` (`Lsp/WorkspaceFolder.fs`) alongside the existing
`workspace.Dispose()` call, and register a best-effort delete at `ClientShutdown` the
same way the source-generator plan's shadow-copy dir cleanup is described
(`plans/source-generator-support.md`, §1 "Cleanup") — stale leftovers from unclean exits
are acceptable since they live in the system temp dir.

---

## 6 — Reacting to edits (new files, new `#:` directives)

`Handlers/Workspace.fs`'s `didChangeWatchedFiles` already distinguishes `.csproj`/`.sln`
changes (→ `WithReloadRequested`) from `.cs` changes (→ incrementally patched into the
existing `Document`/`Project` via `tryReloadDocumentOnUri` / `workspaceFolderDocumentAdd`,
no reload). For virtual projects this incremental path is mostly fine for **edits to
already-tracked files** (the `Document`'s `FilePath` still resolves via
`workspaceFolderDocument UserDocument uri`'s exact-path match in
`workspaceFolderDocumentDetails`, unaffected by where the `.csproj` itself lives).

Two edges need explicit handling, both flagged here as follow-up work rather than solved
inline in this plan:

1. **A brand-new loose `.cs` file appears.** `workspaceFolderDocumentAdd` resolves the
   owning project via `workspaceFolderProjectForPath`, which matches by *directory
   containment* between the doc and `p.FilePath`'s directory — but a virtual project's
   `.csproj` lives in the temp `virtual-projects` dir, not next to the `.cs` files, so
   containment never matches and the new file is silently dropped (logged at `Trace`
   level today). Fix: extend `LspWorkspaceFolder` with a
   `VirtualProjectRoots: Map<string (root dir), ProjectId>` populated at load time, and
   have `workspaceFolderProjectForPath` fall back to a "is this path under a known
   virtual-project root?" check when directory-containment fails. For file-based-app
   projects (root = the entry file's own directory, but restricted to *that* file only
   by design) this should instead trigger a `WithReloadRequested` so a brand-new sibling
   file gets its own fresh virtual project rather than being silently absorbed.
2. **An existing plain file gains a `#:` directive (or vice versa) on save.** This
   changes which bucket (§2) the file belongs to and therefore needs a full solution
   reload, not an incremental text patch. Add a check in the `.cs` branch of
   `didChangeWatchedFiles`: if `wf` was loaded from virtual projects (new
   `LspWorkspaceFolder` flag, e.g. `IsVirtualProjectWorkspace: bool`) *and*
   `hasFileLevelDirectives` disagrees between the old and new file text, call
   `WithReloadRequested` instead of the normal incremental-update path.

Both are edge cases (compared to "completion doesn't work at all today"), so scoping the
first implementation to §§1–5 (get a working project-load path in place) and tackling
reload-triggering refinements as fast follow-ups is reasonable.

---

## 7 — Config toggle

Add `csharp.standaloneFilesEnabled: bool option` to `CSharpConfiguration` (`Types.fs`),
defaulting to `true` (i.e., always attempt this fallback unless a user opts out — e.g.
someone with a folder of `.cs` templates that are never meant to compile as one unit).
Thread it through `mergeCSharpConfiguration` the same way `debug`/`solutionPathOverride`
already are. When `false`, `solutionFindAndLoadOnDir` keeps today's behaviour (raise on
zero project files, ignoring loose `.cs` files entirely).

---

## Files to change

| File | Change |
|---|---|
| `Roslyn/FileBasedApps.fs` (new) | `hasFileLevelDirectives`, `convertFileBasedAppToProject` (shells out to `dotnet project convert`, patches the `<Compile>` path), `plainLooseFilesProjectXml`, `defaultTfmFromSdk`, `writeProjectFile` |
| `Roslyn/Solution.fs` | Add `virtualProjectsDirFor`; extend `solutionFindAndLoadOnDir`'s `projFiles.Length = 0` branch to build and load per-bucket projects instead of raising immediately |
| `Lsp/WorkspaceFolder.fs` | Add `VirtualProjectRoots` / `IsVirtualProjectWorkspace` to `LspWorkspaceFolder`; extend `workspaceFolderProjectForPath` fallback; clean up the `virtual-projects` temp dir in `workspaceFolderTeardown` |
| `Handlers/Workspace.fs` | In `didChangeWatchedFiles`'s `.cs` branch, detect directive-bucket changes on virtual-project workspaces and request a reload instead of an incremental patch |
| `Types.fs` | Add `standaloneFilesEnabled` to `CSharpConfiguration` + `mergeCSharpConfiguration` |
| `docs/features.md` | Document the new fallback behaviour and the config toggle |
| `README.md` | Mention loose-file / file-based-app support alongside existing feature list |

Notably **no** new directive-parsing/XML-generation module of real complexity is needed —
`Roslyn/FileBasedApps.fs` is mostly a thin `Process.Start "dotnet" "project convert ..."`
wrapper plus one path-rewriting string replace for §2b, and a small always-static XML
template for §2a.

## What does NOT change

| File / area | Why |
|---|---|
| `Roslyn/WorkspaceServices.fs`, `CSharpLspHostServices` | Virtual projects load through the same `MSBuildWorkspace.OpenProjectAsync` used today — no new workspace services needed |
| `solutionTryLoadFromProjectFiles` | Reused as-is; it already accepts an arbitrary list of `.csproj` paths and builds one `MSBuildWorkspace`/`Solution` from them |
| `DocumentSync.fs` | `didOpen`/`didChange`/`didClose` for the entry `.cs` files work unmodified since the generated `.csproj` points at the real file path, not a copy |
| `--solution` CLI flag / `SolutionPathOverride` | Explicit override still short-circuits straight to `solutionTryLoadOnPath`, bypassing all of this |
| Any `#:` directive parser / MSBuild-XML templating for file-based apps | Not needed — `dotnet project convert` (§4a) is the authoritative translator; we only patch one path in its output |

---

## 8 — Test fixtures

Two new fixtures under `tests/CSharpLanguageServer.Tests/Fixtures/`, following the
existing "copy fixture → temp dir → spawn server" pattern (§7.5 of
`docs/codebase-architecture.md`):

- `looseFilesNoProject/` — a bare directory with two plain `.cs` files, no
  `.sln`/`.csproj` anywhere, where one file references a type defined in the other, to
  assert cross-file completion/go-to-definition works (validates §2a).
- `fileBasedApp/` — a single `.cs` file with a `#:package` and a `#:property` directive
  and no project file, to assert the package's types resolve in completion (validates §2b).
  Needs both a .NET 10+ SDK (for `dotnet project convert` itself) and network access for
  NuGet restore during the test run (same caveat that applies to any fixture using real
  `#:package`/`PackageReference` restore — prefer a directive referencing a package
  already in the local NuGet cache/lock, or skip in offline CI/on pre-10 SDKs, to be
  worked out at implementation time).

New test file `StandaloneFilesTests.fs`, added to `CSharpLanguageServer.Tests.fsproj`
near the other integration test files.

---

## Open questions / risks (flag before implementing)

- **Requires .NET 10.0.100+ SDK on `PATH`.** `dotnet project convert` doesn't exist on
  older SDKs, so §2b (file-based apps) is unavailable if only an older SDK is installed
  — confirm at implementation time what a clean "not supported, here's why" message
  should look like (probably a one-time `window/logMessage`, not a failure per file).
  §2a (plain loose files, no `#:` directives) has no such requirement since it never
  calls `dotnet project convert` at all.
- **Process-spawn cost.** Each file-based-app entry pays one `dotnet project convert`
  invocation (fast — static translation, no restore) on solution load, plus whatever
  NuGet restore cost `MSBuildWorkspace.OpenProjectAsync` itself pays for `#:package`
  references, same as `dotnet run app.cs` would. Surface the latter via the existing
  `workspaceFolderSolutionLoad`'s `ProgressReporter` so it isn't mistaken for a hang.
  For folders with many file-based apps, consider capping parallelism or batching if
  sequential conversion proves too slow in practice — start with the simplest sequential
  implementation (§5) and measure before optimizing.
- **Which `dotnet` to invoke.** Simplest is the `dotnet` resolved from `PATH` (the server
  already assumes a working `dotnet`/SDK install to run at all via `MSBuildLocator`);
  confirm whether pinning to the specific muxer path backing the `VisualStudioInstance`
  `initializeMSBuild` selected is worth the extra plumbing, or whether relying on `PATH`
  is consistent enough in practice (it's what a user's own terminal `dotnet run` would
  resolve to as well).
- **Silent partial failure.** If some file-based-app conversions fail (stale scratch
  dir, malformed directives, non-.NET-10 SDK) while others succeed, those files are
  currently proposed to be silently dropped from the loaded solution (§4a). Decide
  whether a `window/showMessage` per failed file is warranted, or whether that's too
  noisy for, say, a folder with several experimental/broken scripts.
- **Multiple loose file-based apps in one folder** must not be merged into the shared
  loose-files project (§2 partitioning already excludes them, and `dotnet project
  convert` inherently produces one project per invocation), otherwise their
  `#:package`s would leak into files that never asked for them.
