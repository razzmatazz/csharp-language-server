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

Note: the server accepts **either** activation path (`workspace/configuration` pull **or**
`ClientCapabilities.experimental.csharp.metadataUris = true`) and merges them with
`Option.orElse`, so setting the workspace-configuration key is the easiest client-side fix.

---

## Client investigation findings

### eglot URI handling

`eglot-uri-to-path` (eglot ≥ 1.16, bug#58790) returns any non-`file://` URI **unchanged**
and explicitly delegates to `file-name-handler-alist`. There is no hook, no generic, and no
per-scheme dispatch table — `file-name-handler-alist` is the **only** sanctioned extension
point for custom URI schemes.

The canonical pattern (same as what `eglot-java` uses for `jdt://`):

```elisp
(add-to-list 'file-name-handler-alist
             (cons "\\`csharp://" #'csharp-ls--file-name-handler))

(defun csharp-ls--file-name-handler (operation &rest args)
  (cond
    ((eq operation 'find-file-noselect)
     ;; issue csharp/metadata, write source to a temp buffer, return it
     ...)
    (t (let ((inhibit-file-name-handlers
              (cons #'csharp-ls--file-name-handler inhibit-file-name-handlers))
             (inhibit-file-name-operation operation))
         (apply operation args)))))
```

Neither doom emacs nor any installed package in this setup wires up a `csharp://` handler
for eglot. The `eglot-java` package (which would be the reference implementation for
`jdt://`) is not installed; there is only a MELPA recipe stub.

The `eglot--uri-to-path` / `eglot-uri-to-path` advice approach (mentioned in older docs)
is **obsolete** — the aliases were deprecated in eglot 1.16 and the function is not a
generic, so per-server overrides via `cl-defmethod` are not possible.

### lsp-mode — `useMetadataUris` is never enabled (bug)

`lsp-csharp.el` already ships a working `csharp:` URI handler
(`lsp-csharp--cls-metadata-uri-handler`, registered via `:uri-handlers`) and the
`CSharpMetadataResponse` destructuring matches the server's wire format exactly. However,
**the handler is effectively dead code** because `useMetadataUris` is never turned on:

- **`workspace/configuration` pull path:** lsp-mode never calls
  `lsp-register-custom-settings` for any `csharp.*` key, so the server receives `{}` for
  the `"csharp"` section and deserializes `useMetadataUris` as `None` (defaults to
  `false`).
- **`experimental` capabilities path:** the `csharp-ls` client registration has no
  `:initialized-fn` or experimental-capabilities block that would inject
  `experimental: { csharp: { metadataUris: true } }` into the `initialize` request.

Fix options (either suffices; both is belt-and-suspenders):

1. Add a `defcustom lsp-csharp-cls-use-metadata-uris` (default `t`) and register it:
   ```elisp
   (lsp-register-custom-settings
    '(("csharp.useMetadataUris" lsp-csharp-cls-use-metadata-uris t)))
   ```
2. Add an `:initialized-fn` to the `lsp-register-client` call that sends
   `experimental: { csharp: { metadataUris: true } }`.

There is no `lsp-csharp-cls-use-metadata-uris` defcustom today — it needs to be added.
The solution-file setting (`lsp-csharp-solution-file`) is passed as a `-s` CLI flag, not
via `workspace/configuration`, so it is unaffected.

---

## TODO

- [ ] Update `README` / docs to show the correct `eglot-workspace-configuration` snippet
- [ ] Reply to / close issue #319 (https://github.com/razzmatazz/csharp-language-server/issues/319)
      noting that the correct key is `csharp.useMetadataUris`, not `experimental.csharp.metadataUris`
- [ ] **eglot recipe:** write and document a `file-name-handler-alist` handler for
      `csharp://` URIs (modelled on the `jdt://` pattern used by `eglot-java`). The handler
      must call `csharp/metadata` via `jsonrpc-request` on `eglot-current-server`, write
      the returned source to a temp/cache file, and return a read-only buffer. Pair with
      the correct `eglot-workspace-configuration` snippet to enable `useMetadataUris`.
- [ ] **lsp-mode fix:** file a bug / patch against `lsp-mode` upstream:
      add `defcustom lsp-csharp-cls-use-metadata-uris` (default `t`) and call
      `lsp-register-custom-settings` so `csharp.useMetadataUris` is sent to the server —
      activating the already-present `:uri-handlers` machinery.
- [ ] Provide client-specific configuration recipes (eglot and lsp-mode) in docs —
      self-contained snippets a user can drop into their config to get working
      go-to-definition on decompiled library symbols.
