# Plan: Fix `csharp:` URI round-trip on Windows

**Tracks:** GitHub issue #319 (sub-problem 2)
**Affects:** `workspaceFolderParseCSharpDocumentUri`
in `src/CSharpLanguageServer/Lsp/WorkspaceFolder.fs`

---

## Background

When `useMetadataUris` is enabled, `textDocument/definition` on an external symbol returns a
virtual `csharp:/<projectFilePath>/decompiled/<symbolName>.cs` URI. The client then sends that
URI back in a `csharp/metadata` request; the server must parse it to recover `projectFilePath`
and `symbolMetadataName` to look up the right project and decompile the symbol.

Two functions are involved:

- **Constructor** — `workspaceFolderMetadataSymbolSourceViewUri` builds the URI from
  `project.FilePath` and the symbol name. The same `Uri → string → Substring(8)` idiom is
  also used in `encodeGeneratedDocumentUri` for source-generated document URIs.
- **Parser** — `workspaceFolderParseCSharpDocumentUri` decodes it back.

---

## What the FSI experiments confirmed

### URI construction (current code — must not change)

```fsharp
let projectFile =
    project.FilePath            // "/Users/bob/proj/MyApp.csproj"  (macOS)
    |> Uri                      // file:///Users/bob/proj/MyApp.csproj
    |> string                   // "file:///Users/bob/proj/MyApp.csproj"
    |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
                                // "Users/bob/proj/MyApp.csproj"   ← leading '/' stripped
    |> _.TrimEnd('/')

sprintf "csharp:/%s/decompiled/%s.cs" projectFile ...
// macOS   → "csharp:/Users/bob/proj/MyApp.csproj/decompiled/System.String.cs"
// Windows → "csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs"
```

On **Windows** `project.FilePath` = `C:\path\to\MyApp.csproj`:

```
Uri(...)      → file:///C:/path/to/MyApp.csproj
Substring(8)  → "C:/path/to/MyApp.csproj"    ← colon unencoded, no leading '/'
sprintf       → "csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs"
```

### Why the constructor must not change

FSI confirmed that switching from `string |> Substring(8)` to `.AbsolutePath` in the
constructor breaks macOS. `Uri("/Users/bob/proj/MyApp.csproj").AbsolutePath` = `/Users/bob/...`
(leading `/` included). Passing that to `sprintf "csharp:/%s/..."` produces `csharp://Users/...`
— a double-slash URI. .NET's `Uri` class then interprets `Users` as the **authority (host)**
component, silently dropping it from the path:

```
Uri("csharp://Users/bob/proj/MyApp.csproj/decompiled/System.String.cs").LocalPath
  → "/bob/proj/MyApp.csproj/decompiled/System.String.cs"   ← "Users" eaten as host!
```

The tab label in VSCode is unaffected (last segment is still `System.String.cs`) but the path
is completely wrong and the round-trip breaks on macOS. The constructor's current format —
`csharp:/<path>/decompiled/<symbol>.cs` with a single leading slash — is load-bearing for
correct URI semantics and must be preserved.

### URI parsing (current code — where the bug lives)

```fsharp
let u = Uri uri
let path = u.LocalPath
let decompiledPrefix =
    sprintf ".csproj%sdecompiled%s"
        (string Path.DirectorySeparatorChar)
        (string Path.DirectorySeparatorChar)
```

`Uri.LocalPath` is platform-dependent. On macOS, for a Windows-style URI like
`csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs`, it converts forward slashes
to **backslashes**:

```
Uri("csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs").LocalPath
  → "C:\path\to\MyApp.csproj\decompiled\System.String.cs"     (backslashes on macOS!)
```

`decompiledPrefix` on macOS uses `Path.DirectorySeparatorChar` = `/`, so
`.IndexOf(".csproj/decompiled/")` on a string full of backslashes returns `−1` → parse fails
→ `csharp/metadata` returns `None` → client gets an empty buffer.

| Host OS | URI                          | `LocalPath`                  | `decompiledPrefix`    | `IndexOf` |
|---------|------------------------------|------------------------------|-----------------------|-----------|
| macOS   | `csharp:/Users/bob/…`        | `/Users/bob/…` (forward `/`) | `.csproj/decompiled/` | found ✓   |
| macOS   | `csharp:/C:/path/…`          | `C:\path\…`  (backslashes!)  | `.csproj/decompiled/` | **−1 ✗**  |
| Windows | `csharp:/C:/path/…`          | `C:\path\…`  (backslashes)   | `.csproj\decompiled\` | found ✓   |

In contrast, `Uri.AbsolutePath` always preserves the slash style from the URI string itself,
with no OS-specific transforms:

```
Uri("csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs").AbsolutePath
  → "C:/path/to/MyApp.csproj/decompiled/System.String.cs"    (forward slashes everywhere)
```

### The accidental correctness on macOS for macOS paths

`Uri("/Users/bob/proj/MyApp.csproj").ToString()` = `"file:///Users/bob/proj/MyApp.csproj"`.
`Substring(8)` strips `"file:///"`, which also removes the leading `/`, producing
`"Users/bob/proj/MyApp.csproj"`. The resulting URI is `csharp:/Users/bob/...`.
`Uri.LocalPath` then re-adds the leading `/`, giving `"/Users/bob/..."`.
The strip in the constructor and the re-add in the parser cancel out — correctness by
coincidence.

---

## Root causes

1. **`Uri.LocalPath` is platform-sensitive.** For URIs whose path looks like a Windows drive
   path (`C:/...`), .NET converts forward slashes to backslashes in `LocalPath` regardless of
   host OS. This is the direct cause of the parse failure on Windows paths.

2. **`decompiledPrefix` uses `Path.DirectorySeparatorChar`.** This means it only matches the
   slash style of `LocalPath` on the *current* OS. Since the URIs always use forward slashes
   (RFC 3986 §3.3), the prefix should also always use `/`.

---

## Proposed fix — parser only

The fix is entirely in `workspaceFolderParseCSharpDocumentUri`. The constructor and all
call sites stay unchanged.

### 1. Replace `u.LocalPath` with `u.AbsolutePath`

`AbsolutePath` preserves the slash style as written in the URI, giving a stable,
forward-slash path on all platforms.

### 2. Use a literal `/` in the prefix strings

```fsharp
// Before
let decompiledPrefix =
    sprintf ".csproj%sdecompiled%s"
        (string Path.DirectorySeparatorChar)
        (string Path.DirectorySeparatorChar)
let generatedPrefix =
    sprintf ".csproj%sgenerated%s"
        (string Path.DirectorySeparatorChar)
        (string Path.DirectorySeparatorChar)

// After
let decompiledPrefix = ".csproj/decompiled/"
let generatedPrefix  = ".csproj/generated/"
```

### 3. The project lookup in `CSharpMetadata.fs`

After parsing, `projectFilePath` will be the raw `AbsolutePath` segment — forward slashes,
no leading `/` on Windows (`C:/path/...`), with leading `/` on Unix (`/Users/...`).

`project.FilePath` as Roslyn stores it uses the OS path separator. The comparison must
normalize both sides:

```fsharp
// In CSharpMetadata.fs, the existing lookup:
solution.Projects |> Seq.tryFind (fun p -> p.FilePath = projectFilePath)

// Needs to become:
solution.Projects
|> Seq.tryFind (fun p ->
    p.FilePath.Replace('\\', '/') = projectFilePath.Replace('\\', '/'))
```

> The `Map.tryFind (project.FilePath, symbolMetadataName)` lookup in
> `workspaceFolderDecompiledDocumentFromMetadata` uses the key as Roslyn originally gave it,
> so that cache is unaffected. Only the project-discovery lookup needs normalisation.

---

## Worked example after the fix

### macOS, macOS path (must keep working)

```
URI       : csharp:/Users/bob/proj/MyApp.csproj/decompiled/System.String.cs
AbsPath   : /Users/bob/proj/MyApp.csproj/decompiled/System.String.cs
prefix    : .csproj/decompiled/                              ← found ✓
extracted : /Users/bob/proj/MyApp.csproj
FilePath  : /Users/bob/proj/MyApp.csproj
match     : "/Users/bob/proj/MyApp.csproj" = "/Users/bob/proj/MyApp.csproj" ✓
```

### macOS or Windows receiving a Windows-path URI (currently broken)

```
URI       : csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs
AbsPath   : C:/path/to/MyApp.csproj/decompiled/System.String.cs
prefix    : .csproj/decompiled/                              ← found ✓
extracted : C:/path/to/MyApp.csproj
FilePath  : C:\path\to\MyApp.csproj
match     : "C:/path/to/MyApp.csproj" = "C:/path/to/MyApp.csproj"
            (after Replace('\\', '/') on FilePath)          ✓
```

---

## Files to change

| File | Change |
|------|--------|
| `src/CSharpLanguageServer/Lsp/WorkspaceFolder.fs` | `workspaceFolderParseCSharpDocumentUri`: use `u.AbsolutePath`; use literal `/` in `decompiledPrefix` and `generatedPrefix` |
| `src/CSharpLanguageServer/Handlers/CSharpMetadata.fs` | Normalize path separators when looking up the project by `FilePath` |

The constructor functions (`workspaceFolderMetadataSymbolSourceViewUri`,
`encodeGeneratedDocumentUri`) and all test files are **unchanged** — the URI format emitted
by the server does not change.

---

## Verification

### Local (macOS) — confirm no regression

```
dotnet test --filter FullyQualifiedName~CSharpMetadataTests
```

The existing tests do a full end-to-end round-trip: `textDocument/typeDefinition` returns a
`csharp:` URI, then `csharp/metadata` is called with it. They must continue to pass.

### CI (GitHub Actions) — confirm the Windows fix

`.github/workflows/test.yaml` already runs `dotnet test` on both `ubuntu-24.04` and
`windows-latest` on every push and PR (`fail-fast: false`). No workflow changes are needed.

On the `windows-latest` runner, Roslyn hands back a real `C:\...` project file path. The
constructor embeds it as `C:/...` in the `csharp:` URI. The client sends that URI back to
`csharp/metadata`. With the current buggy parser it returns `None`; with the fix the
`CSharpMetadataTests` round-trip tests must pass. **The Windows CI leg is the primary
validation for this fix** — open a PR and watch those two existing tests go green on Windows.

---

## What this does NOT fix

Issue #1 from the same bug report: clients that do not advertise
`experimental.csharp.metadataUris = true` still get `[]` from `textDocument/definition`
because `useMetadataUris` defaults to `false`. That is a separate design decision and is
tracked separately.
