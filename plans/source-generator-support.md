# Source Generator Support

Tracking issues: #25 (expose generated code to editor), #312 (generator DLL locking)

Two independent problems. Both stem from source generators already running inside the
server process via MSBuildWorkspace/Roslyn, but they can be implemented separately.

---

## 1 — DLL Locking (#312)

### Problem

MSBuildWorkspace loads all `<Analyzer>` / source-generator DLLs into the server process.
Roslyn's `DefaultAnalyzerAssemblyLoader` keeps a file handle open on each DLL for the
lifetime of the process. On Windows, `dotnet build` cannot overwrite a locked DLL, so
rebuilding a user-authored generator fails while the server is running.

(The Razor generator ships inside the SDK and is never rebuilt by the user — it is not
affected.)

### Fix: shadow-copy on load

Add a custom `IAnalyzerAssemblyLoader` that copies each DLL to a temp directory before
loading it:

```fsharp
// src/CSharpLanguageServer/Roslyn/WorkspaceServices.fs
type ShadowCopyAnalyzerAssemblyLoader(shadowDir: string) =
    let loaded = ConcurrentDictionary<string, Assembly>()

    interface IAnalyzerAssemblyLoader with
        member _.AddDependencyLocation(_path) = ()
        member _.LoadFromPath(path) =
            loaded.GetOrAdd(path, fun p ->
                let dir = Path.Combine(shadowDir, Guid.NewGuid().ToString("N"))
                Directory.CreateDirectory(dir) |> ignore
                let dest = Path.Combine(dir, Path.GetFileName(p))
                File.Copy(p, dest)
                Assembly.LoadFrom(dest))
```

### Wiring

`WorkspaceServicesInterceptor` already intercepts every `GetService` call on the
workspace's `HostWorkspaceServices` (via Castle DynamicProxy) and returns custom
implementations when Roslyn asks for a service the server doesn't provide. It already
silently handles `ISourceGeneratorTelemetryCollectorWorkspaceService` and
`ISourceGeneratorTelemetryReporterWorkspaceService`.

The loader can be injected the same way. `IAnalyzerAssemblyLoaderProvider` is an internal
Roslyn interface — determine its full type name at runtime and intercept it in
`WorkspaceServicesInterceptor.Intercept`, returning a provider that yields our shadow-copy
loader. Alternatively, if Roslyn injects `IAnalyzerAssemblyLoader` directly into
`AnalyzerFileReference`, intercept at that point instead. The exact injection surface
needs runtime investigation against Roslyn 5.3.0.

Roslyn's own `ShadowCopyAnalyzerAssemblyLoader` is `internal sealed` — a hand-rolled
implementation is required.

`Assembly.LoadFrom` loads into the default `AssemblyLoadContext`. This matches OmniSharp's
approach. If isolation or unload/reload support is needed later, switch to a custom
`AssemblyLoadContext`.

### Cleanup

Register a `Shutdown` handler to delete the shadow directory. Stale copies from unclean
exits are acceptable — they live in the system temp directory.

### Files to change

| File | Change |
|---|---|
| `Roslyn/WorkspaceServices.fs` | Add `ShadowCopyAnalyzerAssemblyLoader`; add a service-intercept branch in `WorkspaceServicesInterceptor` |
| `Roslyn/Solution.fs` | Pass shadow dir to `CSharpLspHostServices` if the loader needs to be instantiated there |

---

## 2 — Expose Generated Documents to the Editor (#25)

### Problem

Source-generated `.g.cs` files exist inside the Roslyn `Compilation` as
`SourceGeneratedDocument` objects. They are not at a stable disk path — they live under
`obj/` and may not exist after `dotnet clean`. The editor cannot open them, and
go-to-definition into generated code silently returns nothing because
`Location.fromRoslynLocation` (in `Conversions.fs`) checks `File.Exists` on the mapped
path and returns `None` when the file is absent.

### Overview

Three things are needed:

1. A **virtual URI scheme** for generated documents
2. A **content-serving path** through the existing `csharp/metadata` endpoint
3. **Location translation** so handlers emit virtual URIs instead of broken `file://` paths

No new LSP methods, no new handler registrations, no changes to `Lsp/Server.fs`,
`DocumentSync.fs`, `Types.fs`, or `Program.fs`. Always-on — no feature flag.

### 2.1 Virtual URI scheme

Follow the existing `csharp:/` convention (single-slash, matching
`workspaceFolderMetadataSymbolSourceViewUri` which produces
`csharp:/<projectPath>/decompiled/<symbol>.cs`). Define:

```
csharp:/generated/<projectFilePath>/<generatorAssemblyName>/<generatorTypeName>/<hintName>
```

All components URI-encoded. This tuple maps 1:1 to a `SourceGeneratedDocumentIdentity`:

```fsharp
doc.Identity.Generator.AssemblyName   // e.g. "MyGenerators"
doc.Identity.Generator.TypeName       // e.g. "MyGenerators.FooGenerator"
doc.Identity.HintName                 // e.g. "Foo.g.cs"
```

Add `encodeGeneratedDocumentUri` / `parseGeneratedDocumentUri` helpers alongside
`workspaceFolderMetadataSymbolSourceViewUri` / `workspaceFolderParseMetadataSymbolSourceViewUri`
in `Lsp/WorkspaceFolder.fs`.

### 2.2 Serve content via `csharp/metadata`

The existing flow already does what we need: the client receives a `csharp:/…` URI from
`textDocument/definition`, calls `csharp/metadata` with that URI, and opens the returned
text as a read-only buffer.

`CSharpMetadata.handle` currently parses the URI via
`workspaceFolderParseMetadataSymbolSourceViewUri`, finds the project, gets the
compilation, looks up the type by metadata name, and decompiles it. Add a parallel path:
if the URI matches the generated-document scheme, parse it via
`parseGeneratedDocumentUri`, call `project.GetSourceGeneratedDocumentsAsync()`, find the
matching document by identity, and return `doc.GetTextAsync()` directly — no
decompilation needed.

`CSharpMetadataResponse` already has the right shape
(`{ ProjectName; AssemblyName; SymbolName; Source }`) — populate from `doc.Identity`.

### 2.3 Translate locations

The key dispatch point is `workspaceFolderResolveSymbolLocation`, which matches on
`(l.IsInMetadata, l.IsInSource)`:

```fsharp
| true, _ ->   // metadata → decompile (if useMetadataUris enabled)
| false, true -> // source → Location.fromRoslynLocation
| _, _ ->      // catch-all → empty
```

Source-generated files land in the `false, true` arm but then fail the `File.Exists`
check inside `Location.fromRoslynLocation`. Add a check before (or at the top of) that
arm: resolve the location's `SourceTree` via `solution.GetDocument(tree)` — if it
returns a `SourceGeneratedDocument` (a public subclass of `Document` since Roslyn 4.0),
emit a virtual URI instead. This is the single most surgical change; all
definition/references/etc. handlers flow through this function.

The proposed helper:

```fsharp
// Roslyn/Conversions.fs, in module Location
let fromSourceGeneratedDocument
    (encodeUri: SourceGeneratedDocumentIdentity -> string -> string)
    (doc: SourceGeneratedDocument)
    (span: LinePositionSpan)
    : Location =
    { Uri = encodeUri doc.Identity doc.Project.FilePath
      Range = Range.fromLinePositionSpan span }
```

**Handlers that produce locations** (and will benefit without per-handler changes, since
they go through `workspaceFolderResolveSymbolLocation` or `Location.fromRoslynLocation`):

- Definition, References, Implementation, TypeDefinition, CallHierarchy, DocumentHighlight

Declaration is currently a stub (`notImplemented`).

### 2.4 `LspWorkspaceFolderDocumentType` extension

Add a `GeneratedDocument` case alongside the existing `UserDocument | DecompiledDocument
| AnyDocument`. Extend `workspaceFolderDocumentDetails` with a branch that parses the
generated-document URI, calls `project.GetSourceGeneratedDocumentsAsync()`, and matches
by identity.

### 2.5 Semantic model and symbol resolution

`workspaceFolderDocumentSemanticModel` and `workspaceFolderDocumentSymbol` both have
Razor-specific branches guarded by `uri.EndsWith ".cshtml"` that obtain a
`(compilation, SyntaxTree)` pair instead of working from a regular `Document`. Source-
generated URIs need the same treatment, but unlike Razor (which reverse-engineers the
generator's naming convention via `solutionGetRazorDocumentForPath`) source generators
expose identity directly via `GetSourceGeneratedDocumentsAsync()`.

These two functions are good candidates to generalise: a shared helper
`workspaceFolderVirtualDocumentSyntaxTree uri wf` returning
`(Compilation * SyntaxTree) option` could serve both `.cshtml` and `csharp:/generated/…`
cases, replacing the duplicated `EndsWith ".cshtml"` guards.

### 2.6 Diagnostics (optional follow-up)

`Diagnostic.handleWorkspaceDiagnostic` obtains diagnostics via
`project.GetCompilationAsync()` → `compilation.GetDiagnostics()`, grouped by
`d.Location.GetMappedLineSpan().Path`. Diagnostics in source-generated files will have
`obj/` paths that may not exist. These could be mapped to virtual URIs. Lower priority —
generated code is rarely where the user acts on a diagnostic.

### Files to change

| File | Change |
|---|---|
| `Lsp/WorkspaceFolder.fs` | Add `encodeGeneratedDocumentUri` / `parseGeneratedDocumentUri`; add `GeneratedDocument` to `LspWorkspaceFolderDocumentType`; extend `workspaceFolderDocumentDetails`; add source-generated check in `workspaceFolderResolveSymbolLocation`; generalise `workspaceFolderDocumentSemanticModel` and `workspaceFolderDocumentSymbol` |
| `Handlers/CSharpMetadata.fs` | Add branch to serve generated-document URIs |
| `Roslyn/Conversions.fs` | Add `Location.fromSourceGeneratedDocument` helper |

---

## What does NOT change

| File / area | Why |
|---|---|
| `DocumentSync.fs` | Generated documents are read-only — no `didOpen`/`didChange`/`didClose` |
| `Types.fs` / document selectors | No new language ID or filter |
| `Program.fs` / feature flags | Always-on |
| `Lsp/Server.fs` | No new handlers or endpoints |
| Razor path (`solutionGetRazorDocumentForPath`, `solutionFindSymbolForRazorDocumentPath`) | Purely additive; Razor stays on its own code path |

---

## Razor comparison

Razor support solved several of the same sub-problems. The two features are mirror
images: Razor maps *editor positions → generated tree* (`.cshtml` is the real file,
`.g.cs` is internal); source generators map *generated tree → virtual URI* (`.g.cs` is
what the editor opens).

**Reused / extended:**

| Existing code | How generators extend it |
|---|---|
| `workspaceFolderResolveSymbolLocation` (three match arms) | Add source-generated check in `IsInSource` arm |
| `LspWorkspaceFolderDocumentType` (`DecompiledDocument` case) | Add `GeneratedDocument` case |
| `workspaceFolderDocumentSemanticModel` (Razor `.cshtml` branch) | Generalise into shared virtual-doc helper |
| `workspaceFolderDocumentSymbol` (Razor `.cshtml` branch) | Same generalisation |
| `CSharpMetadata.handle` (decompiled branch) | Add generated-doc branch |

**Not reused:**

| Razor mechanism | Why generators don't need it |
|---|---|
| `solutionGetRazorDocumentForPath` (reverse-engineers naming convention) | `GetSourceGeneratedDocumentsAsync()` exposes identity directly |
| `AdditionalDocument` sync in `DocumentSync.fs` | Generated documents are read-only |
| `GetMappedLineSpan()` position translation | Generated files use their own coordinate space |
| `documentSelectorForCSharpAndRazorDocuments` | No new language ID |
| `--features razor-support` flag | Always-on |

---

## Validation notes

_Last validated: 2025-04-11 against codebase at Roslyn 5.3.0 / net10.0_

**Source code** — all referenced file paths, functions, and types confirmed in the
codebase. Key references: `WorkspaceServicesInterceptor` (WorkspaceServices.fs:299),
`CSharpLspHostServices` (WorkspaceServices.fs:350),
`workspaceFolderResolveSymbolLocation` (WorkspaceFolder.fs:278),
`LspWorkspaceFolderDocumentType` (WorkspaceFolder.fs:73),
`Location.fromRoslynLocation` (Conversions.fs:41),
`CSharpMetadataResponse` (Types.fs:69, alias for `CSharpMetadataInformation`).

**Roslyn APIs** — `SourceGeneratedDocument`, `GetSourceGeneratedDocumentsAsync`,
`IAnalyzerAssemblyLoader`, `AnalyzerFileReference` are all public in 5.3.0.
`SourceGeneratedDocument.Identity` is public since 4.4.
`ShadowCopyAnalyzerAssemblyLoader` is `internal sealed`.
`IAnalyzerAssemblyLoaderProvider` is internal.

**Docs to update post-implementation:**

- `docs/features.md` — add source-generator section (also: existing URI example
  `csharp:/metadata/projects/…` is outdated vs actual
  `csharp:/<projectPath>/decompiled/<symbol>.cs`)
- `docs/codebase-architecture.md` — note analyzer/generator loading in
  `Roslyn/WorkspaceServices.fs` section
- `README.md` — no changes unless a config toggle is added
