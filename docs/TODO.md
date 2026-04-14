# TODO

## `completionItem/resolve` crash on sentinel `-1` positions (Neovim et al.)

**Affects:** `completionItem/resolve` when a client sends `"line": -1, "character": -1` in the
echoed `textEdit` field (reproducible with Neovim's built-in LSP client).

### What happens

The server crashes with `-32603 Internal error: JsonSerializationException`. The blame chain is:

1. `Position.Line` / `Position.Character` are typed as `uint32` in Ionide's generated
   `Types.cg.fs` (faithfully following the LSP meta model's `uinteger` primitive).
2. Newtonsoft.Json throws an overflow exception deserializing `-1` into `uint32`.
3. `ErasedUnionConverter.tryReadAllMatchingFields` in Ionide's `JsonUtils.fs` wraps the entire
   `json.ToObject` call in a `with _ -> None` catch-all, so the overflow is silently swallowed.
4. Both union cases of `U2<TextEdit, InsertReplaceEdit>` return `None` → converter throws
   "Could not create an instance of the type 'U2\`2'" → surfaces as `-32603` to the client.

Note: the LSP spec is self-contradictory here — it types the field as `uinteger` but
simultaneously states *"If a line number is negative, it defaults to 0."*

### Recommended fix (short term — no upstream changes needed)

Add a `sanitize: JToken -> JToken` pre-processing step to the `wrapHandler` call for
`completionItem/resolve` in `Server.fs`. The sanitizer walks into `textEdit.range` and clamps
any negative `line`/`character` integers to `0` before deserialization, matching the defaulting
behaviour the spec describes in prose.

See `plans/completion-resolve-textedit-crash.md` for the full design, code sketches, and
discussion of the upstream Option 2 fix (changing Ionide to use `int` instead of `uint32`
and splitting the `try/with` in `tryReadAllMatchingFields`).

## Upstream bug: `memoriseByHash` hash collision in Ionide.LanguageServerProtocol

**Affects:** test ordering in `CSharpLanguageServer.Tests.fsproj`

### Background

`Ionide.LanguageServerProtocol` uses a process-level memoization cache (`UnionInfo.get` in
`OptionConverter.fs`) to avoid re-computing F# union reflection data for each type. The cache is
backed by a `ConcurrentDictionary<int, UnionInfo>` keyed on `Type.GetHashCode()` — **without
ever verifying equality on lookup**:

```fsharp
// Ionide.LanguageServerProtocol/src/OptionConverter.fs
let inline memoriseByHash (f: 'a -> 'b) : 'a -> 'b =
  let d = ConcurrentDictionary<int, 'b>()
  fun key ->
    let hash = key.GetHashCode()
    match d.TryGetValue(hash) with
    | (true, value) -> value          // <-- returns cached value for ANY type with this hash
    | _ ->
      let value = f key
      d.TryAdd(hash, value) |> ignore
      value
```

This is also used for `OptionConverter.canConvert` and `OptionConverter.getInnerType`.

### The failure

When `JsonRpcTests.fs` is compiled before `SourceGeneratorTests.fs` in the `.fsproj`, the two
test modules run in parallel (the assembly is marked `[<Parallelizable(ParallelScope.All)>]`).
`JsonRpcTests.fs` exercises `Newtonsoft.Json` directly with `JObject`/`JToken` types, which
causes those types to be hashed and inserted into the shared `memoriseByHash` caches. Some of
those hashes collide with LSP union types (specifically `U2<Location, Location[]>` — the
`Declaration` type returned by `textDocument/definition`).

When `SourceGeneratorTests.fs` later calls `deserialize<Declaration option>`, `UnionInfo.get`
returns the wrong cached `CaseInfo` for the colliding type, causing a
`JsonSerializationException` server-side, which surfaces in the tests as:

```
Internal error: JsonSerializationException
```

When `SourceGeneratorTests.fs` is compiled first the collision does not occur in practice
because the source generator tests populate the cache with LSP types before the JSON-RPC tests
can insert colliding entries for Newtonsoft types.

### Workaround

Keep `SourceGeneratorTests.fs` before `JsonRpcTests.fs` in the `.fsproj` compile order. This
is the current order and it avoids the collision in practice.

### Proper fix

The fix belongs upstream in `Ionide.LanguageServerProtocol`. `memoriseByHash` should be
replaced with `memorise` (which uses the full `Type` reference as the dictionary key, giving
correct equality semantics) everywhere it is called with `Type` arguments:

- `UnionInfo.get`
- `OptionConverter.canConvert`
- `OptionConverter.getInnerType`
- `ErasedUnionConverter.canConvert`
- `SingleCaseUnionConverter.canConvert`

**Upstream repo:** https://github.com/ionide/Ionide.LanguageServerProtocol

## Upstream `JsonRpc.fs` + `docs/jsonrpc.md`

The `JsonRpc.fs` transport implementation and its companion documentation (`docs/jsonrpc.md`) are
general-purpose and not specific to `csharp-ls`. They should be extracted and contributed upstream
— either to `Ionide.LanguageServerProtocol` or as a standalone NuGet package — so that other F#
LSP server authors can benefit from the symmetric JSON-RPC 2.0 transport without duplicating it.

**Upstream repo:** https://github.com/ionide/Ionide.LanguageServerProtocol

## Replace Newtonsoft.Json with System.Text.Json for LSP serialization/deserialization

`csharp-ls` currently uses `Newtonsoft.Json` (via `Ionide.LanguageServerProtocol`) for all LSP
message serialization and deserialization. Several bugs in `TODO.md` and `plans/` trace directly
to Newtonsoft quirks (`uint32` overflow, `memoriseByHash` hash collision, `ErasedUnionConverter`
catch-all swallowing errors, etc.).

The goal is to reimplement (or adopt an upstream reimplementation of) the `serialize` /
`deserialize` helpers — and the converters they depend on (`OptionConverter`, `ErasedUnionConverter`,
`SingleCaseUnionConverter`) — using `System.Text.Json` instead of `Newtonsoft.Json`. Key
considerations:

- `System.Text.Json` is already a .NET runtime dependency (no extra NuGet weight).
- F# discriminated-union and option handling requires custom `JsonConverter<'T>` implementations
  equivalent to the existing Ionide converters — these should be written to avoid the
  `memoriseByHash` collision class of bugs (use exact `Type` equality as dictionary key, not hash).
- The `JToken`/`JObject`/`JValue` API surface used in `JsonRpc.fs` and handlers must be replaced
  with `JsonElement` / `JsonDocument` or a thin wrapper.
- The change is best done in a branch that keeps the Newtonsoft path alive behind a compile flag
  until the new path has full test coverage.
- If Ionide upstream adopts `System.Text.Json` first, prefer pulling that in rather than
  maintaining a fork.

**Upstream tracker:** https://github.com/ionide/Ionide.LanguageServerProtocol

## Analyzer support improvements

See `plans/analyzer-support.md` for the full design. Three specific improvements are needed on
top of the base implementation:

### (a) Disable analyzers by default; make them configurable via `workspace/configuration`

Running `CompilationWithAnalyzers` unconditionally on every diagnostic request adds latency that
not all users want. Analyzers should be **off by default** and enabled via a workspace
configuration key, e.g.:

```json
{
  "csharp": {
    "analyzerEnabled": true
  }
}
```

The server should send a `workspace/configuration` request on startup (and re-request on
`workspace/didChangeConfiguration`) to read this key, storing the result in the workspace-folder
state alongside other settings. All three diagnostic paths (`Handlers/Diagnostic.fs` `handle`,
`getWorkspaceDiagnosticReports`, `Runtime/PushDiagnostics.fs` `resolveDocumentDiagnostics`) should
gate analyzer execution on this flag.

### (b) Disable analyzers in tests by default; restore full concurrency

The test harness (`Tooling.fs`) starts the server with a fixed set of flags. Analyzer runs add
significant latency and non-determinism to integration tests that are not specifically testing
analyzer behavior. Analyzer support should be **disabled in the default test server configuration**
(e.g. via the workspace/configuration mechanism above, or a dedicated CLI flag), and only enabled
explicitly in tests that actually exercise analyzer output — i.e. the tests in `AnalyzerTests.fs`
described in `plans/analyzer-support.md`.

Once analyzers are disabled by default, the `activeClientsSemaphore` concurrency cap in
`Tooling.fs` — currently `min Environment.ProcessorCount 4` — should be raised back to
`Environment.ProcessorCount` (full nproc). The `4` cap was introduced because each active test
client runs an LSP server that in turn runs analyzers, making the per-test CPU cost proportional
to the number of analyzers; with analyzers off the cost drops back to the level where running one
server per logical core is safe.

### (c) Hash `configuration.analyzerEnabled` into `partialResultId`

The workspace pull-diagnostic path (`getWorkspaceDiagnosticReports`) generates `partialResultId`
tokens used by clients to diff incremental results. The token computation must include the
`analyzerEnabled` setting so that toggling analyzers on or off invalidates any cached result the
client holds. Without this, a client that has a stale "analyzers off" result would not request a
fresh report after the user enables analyzers, and vice versa.
