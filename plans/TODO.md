# TODO

## Suppress type inlay hints when type is apparent from the initializer

**Affects:** `textDocument/inlayHint` — `InlayHint.fs`, `toInlayHint` function.

For `var` declarations where the right-hand side already makes the type obvious to the reader,
the type inlay hint is redundant noise. Example:

```csharp
var someValue = getIntAbove10();  // showing `: int` here adds no value
```

The `VariableDeclarationSyntax` match arm in `toInlayHint` does not currently inspect the
initializer expression at all — it emits a hint for every `var x = <anything>` as long as the
type resolves without error.

### Recommended fix

Add an `isTypeApparentFromExpression` helper (mirroring Roslyn's own
`CSharpInlineTypeHintsService` suppression logic) and call it on the initializer before emitting
the hint. Suppress when the initializer is one of:

- `InvocationExpressionSyntax` — method/delegate call (`getIntAbove10()`)
- `ObjectCreationExpressionSyntax` — `new Foo(...)`
- `CastExpressionSyntax` — `(int)x`
- `LiteralExpressionSyntax` — `42`, `"hi"`, `true`, `null`
- `DefaultExpressionSyntax` — `default(T)` / `default`
- `TypeOfExpressionSyntax` — `typeof(T)`
- `SizeOfExpressionSyntax` — `sizeof(T)`
- `BinaryExpressionSyntax` with `AsExpression` kind — `expr as T`
- `ParenthesizedExpressionSyntax` — recurse into inner expression

The same suppression should be applied to the `DeclarationExpressionSyntax`,
`SingleVariableDesignationSyntax`, and `ForEachStatementSyntax` arms where an initializer is
accessible.

The existing TODO comment in the file already notes:
> `// TODO: Support the configuration whether or not to show some kinds of inlay hints.`

---

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

## Abstract and upstream `RequestScheduling.fs`

`RequestScheduling.fs` implements LSP request-queue semantics (read-only vs. read-write
concurrency, per-document and per-workspace locking, cancellation propagation) in a way that is
reusable by any F# LSP server. Before upstreaming, the module should be reviewed for
`csharp-ls`-specific assumptions (e.g. Roslyn `Solution` references) and abstracted over them so
the scheduling logic itself is dependency-free.

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

### Analyzer-supplied code fixes and other analyzer features

Roslyn analyzers can ship paired `CodeFixProvider` implementations that offer fixes for their
diagnostics (e.g. "add accessibility modifier" for IDE0040, "remove unused member" for IDE0051).
These fixes surface in editors as `textDocument/codeAction` responses. Investigate whether the
current code-action handler already picks them up via the Roslyn host services layer, or whether
it only sees compiler-supplied fixes. If analyzer code fixes are absent, wire them in — the
mechanism is `CodeFixContext` / `ICodeFixProvider.RegisterCodeFixesAsync`, driven by the same
`CompilationWithAnalyzers` diagnostic results used in the diagnostic pipeline. Also check whether
any other analyzer-adjacent features (e.g. `CodeRefactoringProvider`-based refactorings,
`ICodeStyleService` suggestions) are missing and worth adding at the same time.
