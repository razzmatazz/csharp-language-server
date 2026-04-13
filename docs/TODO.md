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
