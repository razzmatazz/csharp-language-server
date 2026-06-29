# Investigation: `csharp:` URI round-trip on Windows

**Tracks:** GitHub issue #319 (sub-problem 2 — Windows encoding)
**Status: NOT A BUG — see conclusion**

---

## What was suspected

thomashilke (issue #319) reported that on Windows the `:` in drive-letter paths is left
unencoded in the `csharp:` URI, and that the server fails to find the matching project when
`csharp/metadata` is called back with that URI.

---

## What the FSI experiments and CI showed

### URI construction (current code)

```fsharp
let projectFile =
    project.FilePath            // "C:\Users\...\MyApp.csproj"  (Windows)
    |> Uri                      // file:///C:/Users/.../MyApp.csproj
    |> string                   // "file:///C:/Users/.../MyApp.csproj"
    |> fun s -> if s.StartsWith "file:///" then s.Substring(8) else s
                                // "C:/Users/.../MyApp.csproj"  ← colon unencoded, no leading '/'
    |> _.TrimEnd('/')

sprintf "csharp:/%s/decompiled/%s.cs" projectFile ...
// → "csharp:/C:/Users/.../MyApp.csproj/decompiled/System.String.cs"
```

### URI parsing — why it works on Windows

`Uri.LocalPath` is platform-sensitive: for a `C:/`-style path embedded in a `csharp:` URI
it returns **backslashes** on every platform:

```
Uri("csharp:/C:/path/to/MyApp.csproj/decompiled/System.String.cs").LocalPath
  macOS  → "C:\path\to\MyApp.csproj\decompiled\System.String.cs"
  Windows→ "C:\path\to\MyApp.csproj\decompiled\System.String.cs"
```

On Windows, `decompiledPrefix` uses `Path.DirectorySeparatorChar = '\'`:

```
decompiledPrefix = ".csproj\decompiled\"
```

And Roslyn's `project.FilePath` also uses backslashes on Windows. So all three sides of the
comparison (`LocalPath`, `decompiledPrefix`, `project.FilePath`) use backslashes
consistently → `IndexOf` finds the prefix → the path extracted matches `project.FilePath`
→ the round-trip succeeds.

This is confirmed by the CI test suite: `CSharpMetadataTests` run on `windows-latest` in
`.github/workflows/test.yaml` and **currently pass**.

### Where the analysis went wrong

The only scenario where the parser breaks is when a **non-Windows** runtime receives a
Windows-style URI (i.e. `csharp:/C:/...`). On macOS/Linux:
- `LocalPath` gives backslashes (same as Windows)
- `Path.DirectorySeparatorChar = '/'`
- `decompiledPrefix = ".csproj/decompiled/"` (forward slashes)
- `IndexOf(".csproj/decompiled/")` on a backslash string → **−1**

But this cross-platform case (a Linux/macOS server receiving a URI with a Windows drive-letter
path) does not occur in normal usage. The server always runs on the same OS as the project
files.

### macOS round-trip — also works, but for different reasons

On macOS, `project.FilePath` starts with `/` (e.g. `/Users/bob/proj/MyApp.csproj`).

- Constructor: `Substring(8)` strips the leading `/`, embedding `"Users/bob/..."` in the URI  
- Parser: `LocalPath` re-adds the leading `/`, giving back `"/Users/bob/..."`  
- The strip and re-add cancel out — correctness by coincidence, but it holds reliably

---

## Conclusion

The URI round-trip is **not broken** on either Windows or macOS for normal (same-platform)
usage. The `CSharpMetadataTests` passing on `windows-latest` CI confirms this.

The reported symptoms in issue #319 (empty definitions, empty buffer) are caused by
**issue #1** — the `useMetadataUris` opt-in gate — not by a URI parsing bug.
Clients that do not advertise `experimental.csharp.metadataUris = true` get `[]` from
`textDocument/definition` because `useMetadataUris` defaults to `false`, so the metadata
URI is never constructed or returned in the first place.

This plan is closed. The Windows encoding concern raised by thomashilke does not manifest
as a runtime bug in practice.

---

## Follow-up: documentation and eglot config key correction

Discovered while investigating issue #319: the documented (and commonly attempted) eglot
workspace configuration for enabling `useMetadataUris` uses the wrong key path.

**Wrong** (as seen in the wild):
```elisp
(setq eglot-workspace-configuration
      '((experimental
           (csharp
             (metadataUris . t)))))
```

**Correct:**
```elisp
(setq eglot-workspace-configuration
      '((csharp
           (useMetadataUris . t))))
```

### Why

The server's `TryPullCSharpConfig` (in `Lsp/Client.fs`) requests the `"csharp"` section via
`workspace/configuration` and deserializes it directly into `CSharpConfiguration`, whose
field is named `useMetadataUris`. The `experimental.csharp.metadataUris` path is only read
from `ClientCapabilities.experimental` (in `ServerStateLoop.fs`) — something the LSP client
would have to populate in the `initialize` request's capabilities, not via workspace
configuration.

### TODO

- [ ] Update `README` / docs to show the correct `eglot-workspace-configuration` snippet
- [ ] Reply to / close issue #319 (https://github.com/razzmatazz/csharp-language-server/issues/319)
      noting that the correct key is `csharp.useMetadataUris`, not `experimental.csharp.metadataUris`
- [ ] Provide an eglot extension (or a `lsp-mode` equivalent) — either as a documented
      snippet or as a shipped helper — that teaches eglot/lsp-mode how to open `csharp:`
      URIs returned by `textDocument/definition`.  Neither client handles these URIs
      out-of-the-box: eglot has no built-in `csharp:` scheme handler, and lsp-mode
      likewise does not open decompiled-metadata buffers for this server.  The fix is a
      small Elisp hook (e.g. overriding `eglot-find-file-hook` or advising
      `eglot--uri-to-path` / `lsp--uri-to-path`) that calls `csharp/metadata` and
      opens the response in a read-only buffer, making go-to-definition on library
      symbols work end-to-end.
