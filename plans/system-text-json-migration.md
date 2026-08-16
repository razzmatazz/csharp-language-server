# Migrate from Newtonsoft.Json to System.Text.Json

## Status: Complete

`Server.serialize`/`Server.deserialize` — the serializer csharp-ls actually uses for all LSP
payloads — are now backed by native `System.Text.Json` (STJ) converters instead of Newtonsoft.
`dotnet build` is clean and `dotnet test` passes in full.

The final architecture **diverged from the original plan below** in one important way: instead
of collapsing `LSPAny` into a raw `type LSPAny = JsonElement` alias, `origin/main` had
independently picked up upstream Ionide's `LSPAny` **wrapper class** design
(`ionide/LanguageServerProtocol` PR #74 — a class wrapping `JToken`, with a `.JsonElement`
bridge accessor, `fromJToken`/`fromJsonElement` factories, and a Newtonsoft `LSPAnyConverter`)
before this migration landed. Rather than reverting that, the STJ conversion was built **on top
of** the existing wrapper. See "Final architecture" below for what actually shipped; the
"Original Plan" section further down is kept for historical context but several of its phases
(2, 3, 4 as originally written) turned out to be unnecessary or were already satisfied by a
different mechanism.

---

## Final architecture

`LSPAny` (`Types.fs`) is unchanged in shape from upstream: a class wrapping a `JToken`, with

- `.JToken: JToken` — the original Newtonsoft-backed accessor.
- `.JsonElement: JsonElement` — bridges to STJ by round-tripping the raw JSON text.
- `LSPAny.fromJToken` / `LSPAny.fromJsonElement` — construction factories.
- `LSPAnyConverter` (Newtonsoft `JsonConverter`, `[<JsonConverter(typeof<LSPAnyConverter>)>]` on
  the type) — lets `LSPAny` round-trip through Newtonsoft.
- **New:** `LSPAnyJsonConverter` (STJ `JsonConverter<LSPAny>`) — lets `LSPAny` round-trip
  through STJ too, via the same `.JsonElement`/`fromJsonElement` bridge.

`Server.serialize<'t>`/`Server.deserialize<'t>` (`LanguageServerProtocol.fs`) now go through a
new `lspSerializerOptions: JsonSerializerOptions`, registering:

- `UnitConverterFactory`, `FSharpOptionConverterFactory`, `EnumMemberConverterFactory`,
  `ErasedUnionConverterFactory`, `SingleCaseUnionConverterFactory` — STJ equivalents of the
  Newtonsoft converters, added in `OptionConverter.fs`/`JsonUtils.fs`.
- `JTokenJsonConverterFactory` — lets a raw `Newtonsoft.Json.Linq.JToken`/`JObject` value (still
  used by a couple of test helpers that bypass the F# type system) pass through the STJ pipeline
  as pre-formed JSON, mirroring the pass-through behaviour Newtonsoft's serializer gave `JToken`
  natively.
- `LSPAnyJsonConverter`.

```fsharp
let deserialize<'t> (value: LSPAny) : 't =
    System.Text.Json.JsonSerializer.Deserialize<'t>(value.JsonElement, lspSerializerOptions)

let serialize<'t> (o: 't) : LSPAny =
    System.Text.Json.JsonSerializer.SerializeToElement(o, lspSerializerOptions)
    |> LSPAny.fromJsonElement
```

Because the public signature of `serialize`/`deserialize` is unchanged (`LSPAny -> 't` /
`'t -> LSPAny`), **no call sites anywhere in csharp-ls needed to change** — `Handlers/*.fs`,
`Lsp/Server.fs`, `Lsp/Client.fs`, `Runtime/ServerStateLoop.fs`, and the tests already bridge via
`LSPAny.fromJsonElement`/`.JsonElement`/`.fromJToken` directly at their own call sites (this was
already true on `origin/main` independently of this migration — there are no standalone
`jeToJToken`/`jtokenToJe` shim functions in `Util.fs`; `LSPAny` itself is the bridge).

**Deliberately untouched (stays 100% Newtonsoft):**

- `defaultJsonRpcFormatter`, `Server.start*`, and the `Client` module in
  `LanguageServerProtocol.fs` — csharp-ls's actual runtime transport is the custom STJ-based
  `Runtime/JsonRpc.fs` / `Lsp/Server.fs` / `Lsp/Client.fs`, not `Ionide...Server.start*`, so this
  code path is effectively unused by csharp-ls itself. It's kept working (not deleted) because
  `Ionide.LanguageServerProtocol` is a general-purpose vendored library that other consumers may
  still rely on for the `StreamJsonRpc`-based `Server.start*` API.
- `JsonRpc.fs` envelope types (`Request`/`Notification`/`Response`/`Error`) — `Params`/`Result`/
  `Data` remain `JToken option`, since they only feed the Newtonsoft wire path above.
- `Types.cg.fs`, `ClientServer.cg.fs`, `GenerateTypes.fs` — unchanged from `origin/main`. Since
  `LSPAny` stays a hand-written wrapper class (not a generated type alias), none of the
  originally-planned generator changes (STJ `open` statements, dropping the
  `StringEnumConverter` attribute, `JToken`→`JsonElement` AST renames) were needed. Enums still
  carry `[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` (Newtonsoft) for the wire
  path; STJ handles the same enums via the globally-registered `SingleCaseUnionConverterFactory`
  in `lspSerializerOptions`, without needing a per-type attribute.
- `Client.fs` (Ionide's `LspClient` abstract class) — `TelemetryEvent`/`WorkspaceConfiguration`
  stay `Newtonsoft.Json.Linq.JToken`-typed.

`OptionConverter.fs` and `JsonUtils.fs` now contain **both** the original Newtonsoft converters
(kept for `defaultJsonRpcFormatter`) and the new STJ converters, side by side in the same files,
disambiguated by generic arity (e.g. `ErasedUnionConverter()` — Newtonsoft — vs
`ErasedUnionConverter<'T>()` — STJ; same pattern for `SingleCaseUnionConverter`).

**Bonus fix that came along for free:** the `memoriseByHash` hash-collision bug
(`OptionConverter.fs` caching reflection data by `Type.GetHashCode()`, which can corrupt the
shared cache on collisions/parallel test runs) is fully eliminated. Every converter — Newtonsoft
*and* STJ — now uses `memorise` (keyed by exact `Type`), not `memoriseByHash`; the latter no
longer exists anywhere in the codebase.

---

## Original Plan (historical — kept for context)

The sections below describe the plan as originally written, before it was discovered that
`origin/main` already carried an `LSPAny` wrapper-class design. Phase 1 (the serializer rewrite)
is essentially what shipped, adapted to work through the `LSPAny` wrapper instead of a raw
`JsonElement` alias. **Phases 2, 3, and 4 as written below were not carried out** — Phase 2
(codegen/`JsonRpc.fs`/`Client.fs` changes) was superseded by keeping `LSPAny` as a wrapper class;
Phases 3 and 4 (removing shims from `csharp-ls` and updating tests) turned out to already be
satisfied on `origin/main` via the `LSPAny` bridge methods, not via the `Util.fs`
`jeToJToken`/`jtokenToJe` shim functions this plan assumed.

### Context

`src/Ionide.LanguageServerProtocol/` is a **local project reference** (not a NuGet package) that
`csharp-ls` references via `<ProjectReference>`.  We own this source and can edit it directly —
there is no copy-and-vendor step required.

### Motivation

`csharp-ls` used to serialize LSP types with Newtonsoft only. Several bugs traced directly to
the Newtonsoft serializer:

1. **`uint32` overflow** — `Position.Line`/`Character` are `uint32`; Newtonsoft throws on the
   sentinel value `-1`.  `ErasedUnionConverter`'s catch-all swallowed the exception silently,
   producing the wrong union case and a `-32603` error.  A pre-deserialization clamping
   workaround (`sanitizeCompletionItem` in `Lsp/Server.fs`) exists and is still in place —
   `CompletionTests` covers it end-to-end.

2. **`memoriseByHash` hash collision** — see "Bonus fix" above; now fully resolved.

3. **Silent exception swallowing** — the new STJ `ErasedUnionConverter<'T>` catches only
   `JsonException` per attempt (never `with _ ->`), so genuine bugs no longer disappear behind a
   silently-wrong union case.

### What was actually implemented (Phase 1, adapted)

- **`OptionConverter.fs`**: added `FSharpOptionConverter<'T>` + `FSharpOptionConverterFactory`
  (STJ). Kept the Newtonsoft `OptionConverter` for the wire path. `Converters.memorise` replaces
  `memoriseByHash` everywhere in the file.
- **`JsonUtils.fs`**: added STJ `EnumMemberConverter<'T>`/`Factory`,
  `SingleCaseUnionConverter<'T>`/`Factory`, `ErasedUnionConverter<'T>`/`Factory`, and
  `JTokenJsonConverter<'T>`/`Factory`. Kept the Newtonsoft `StrictNumberConverter`,
  `StrictStringConverter`, `StrictBoolConverter`, `ErasedUnionConverter`,
  `SingleCaseUnionConverter`, and `OptionAndCamelCasePropertyNamesContractResolver` — all still
  registered on `defaultJsonRpcFormatter`.
- **`LanguageServerProtocol.fs`**: added `lspSerializerOptions`; rewrote `serialize`/
  `deserialize` to use it via the `LSPAny` bridge (see "Final architecture" above).
  `defaultJsonRpcFormatter`, `Server.start*`, and the `Client` module untouched.
- **`Types.fs`**: added `LSPAnyJsonConverter` (STJ) alongside the existing `LSPAnyConverter`
  (Newtonsoft). `LSPAny` itself unchanged.

No changes were needed to `Types.cg.fs`, `ClientServer.cg.fs`, `GenerateTypes.fs`, `JsonRpc.fs`,
`Client.fs`, `Util.fs`, `Lsp/Server.fs`, `Lsp/Client.fs`, `Runtime/ServerStateLoop.fs`,
`Handlers/*.fs`, or the test suite — all of these already worked against the `LSPAny`-typed
`serialize`/`deserialize` signature on `origin/main`.

---

## Success Criteria — met

- `dotnet build` produces zero errors/warnings.
- `dotnet test` passes in full (292/292).
- `OptionConverter.fs`/`JsonUtils.fs` `UnionInfo.get`/`canConvert` caches use `memorise`, not
  `memoriseByHash`, everywhere (Newtonsoft and STJ converters alike).
- `Server.serialize`/`Server.deserialize` use `JsonSerializerOptions` with native STJ converters.
- Newtonsoft.Json and StreamJsonRpc remain dependencies of
  `Ionide.LanguageServerProtocol.fsproj`; `defaultJsonRpcFormatter` and `Server.start*` continue
  to function correctly and are unaffected by this change.
