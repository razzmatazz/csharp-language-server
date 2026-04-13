# TODO

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
