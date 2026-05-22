# Migrate from Newtonsoft.Json to System.Text.Json

## Context

`src/Ionide.LanguageServerProtocol/` is a **local project reference** (not a NuGet package) that
`csharp-ls` references via `<ProjectReference>`.  We own this source and can edit it directly —
there is no copy-and-vendor step required.

Key files in `src/Ionide.LanguageServerProtocol/src/`:

| File | Role |
|---|---|
| `Types.cg.fs` | Generated — defines `LSPAny = JToken` (Newtonsoft alias); `LSPArray`, `LSPObject`; `[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` on ~7 enum DUs |
| `ClientServer.cg.fs` | Generated — `ILspClient` interface with `TelemetryEvent: LSPAny -> Async<unit>` and `WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<array<LSPAny>>` |
| `Client.fs` | `LspClient` abstract class — `TelemetryEvent` and `WorkspaceConfiguration` members are explicitly typed `Newtonsoft.Json.Linq.JToken` / `JToken[]` |
| `Server.fs` | `LspServer` abstract class (handler stubs — no Newtonsoft usage) |
| `OptionConverter.fs` | `OptionConverter`, `UnionInfo`, `memoriseByHash` (hash-collision bug lives here) |
| `JsonUtils.fs` | `ErasedUnionConverter`, `SingleCaseUnionConverter`, `Strict*Converter`, `OptionAndCamelCasePropertyNamesContractResolver` |
| `LanguageServerProtocol.fs` | `Server.serialize`/`Server.deserialize`; `defaultJsonRpcFormatter()` (uses `StreamJsonRpc.JsonMessageFormatter`); `Server.start*` family; `Client` module |
| `JsonRpc.fs` | Ionide's JSON-RPC envelope types: `Request.Params`, `Notification.Params`, `Response.Result`, `Error.Data` all typed `JToken option` |
| `Types.fs` | `ErasedUnionAttribute`, `U2`, `U3`, `U4` (no Newtonsoft usage) |
| `TypeDefaults.fs` | Default values for generated LSP types (no Newtonsoft usage) |

**`tools/MetaModelGenerator/` is a build tool** that generates `Types.cg.fs` and
`ClientServer.cg.fs` from `metaModel.json`.  It uses Newtonsoft to *parse* the meta-model JSON
(unchanged), but it also *emits* Newtonsoft-specific code into the generated files.  Those
emission points must be updated so that regenerating the files produces STJ-compatible output.
The generator itself keeps its Newtonsoft parse dependency; only the code-emission logic changes.

**What is preserved:**
`StreamJsonRpc`, `defaultJsonRpcFormatter`, `Server.start*`, and the `Client` module in
`LanguageServerProtocol.fs` are kept intact.  They are extended with a Newtonsoft
`JsonConverter<JsonElement>` so that StreamJsonRpc can still round-trip `JsonElement`-typed
fields (i.e. `LSPAny` once it becomes `JsonElement`).

---

## Current State — Phase 0 Complete ✓

The wire layer (`Runtime/JsonRpc.fs`) has already been converted to STJ.  `Util.fs` contains
the temporary bridge shims `jeToJToken` / `jtokenToJe`.  All tests pass.

```
Runtime/JsonRpc.fs    — STJ throughout (JsonElement, JsonDocument, JsonObject, Utf8JsonWriter)
Util.fs               — jeToJToken / jtokenToJe round-trip bridges (temporary, in csharp-ls)
Lsp/Server.fs         — wrapHandler calls jeToJToken before Newtonsoft deserialize;
                        jtokenToJe after Newtonsoft serialize
Lsp/Client.fs         — wraps every outbound serialize with jtokenToJe;
                        unwraps responses with jeToJToken before Newtonsoft deserialize
tests/Tooling.fs      — RpcMessageLogEntry.Message is string (raw JSON)
tests/JsonRpcTests.fs — assertions use JsonElement / TryGetProperty
```

---

## Motivation

`csharp-ls` currently serializes LSP types with Newtonsoft, and bridges across to its STJ
wire layer via `jeToJToken`/`jtokenToJe` string round-trips scattered across `Lsp/Server.fs`,
`Lsp/Client.fs`, and `Util.fs`.  Several bugs trace directly to the Newtonsoft serializer:

1. **`uint32` overflow** — `Position.Line`/`Character` are `uint32`; Newtonsoft throws on the
   sentinel value `-1`.  `ErasedUnionConverter`'s catch-all swallows the exception silently,
   producing the wrong union case and a `-32603` error.  A fragile pre-deserialization clamping
   workaround (`sanitizeCompletionItem` in `Lsp/Server.fs`) exists but covers only one call site.

2. **`memoriseByHash` hash collision** — `OptionConverter.fs` caches reflection data keyed only
   by `Type.GetHashCode()`.  Parallel test runs can corrupt the shared cache.  Worked around by
   pinning compile order in `.fsproj`.

3. **Silent exception swallowing** — `ErasedUnionConverter.tryReadAllMatchingFields` catches all
   exceptions (`with _ -> None`), making deserialization bugs invisible.

**Goal**: replace the Newtonsoft serializer with native STJ converters so that `serialize` /
`deserialize` in `LanguageServerProtocol.fs` use `JsonSerializerOptions` directly.  Change
`LSPAny = JsonElement`.  Remove the `jeToJToken`/`jtokenToJe` shims from `csharp-ls`.
Keep `StreamJsonRpc`, `defaultJsonRpcFormatter`, `Server.start*`, and the `Client` module alive
by registering a Newtonsoft `JsonConverter<JsonElement>` that lets StreamJsonRpc round-trip
`JsonElement`-typed fields through its existing Newtonsoft pipeline.

---

## Scope

### Newtonsoft/JToken usage remaining after Phase 0

| File | What remains |
|---|---|
| `Ionide.../OptionConverter.fs` | `OptionConverter`, `memoriseByHash`, reflection helpers |
| `Ionide.../JsonUtils.fs` | `ErasedUnionConverter`, `SingleCaseUnionConverter`, `Strict*Converter`, contract resolver |
| `Ionide.../LanguageServerProtocol.fs` | `defaultJsonRpcFormatter`; `Server.serialize`/`deserialize`; `Server.start*`; `Client` module |
| `Ionide.../JsonRpc.fs` | `Params`/`Result`/`Data`: `JToken option`; `[<JsonProperty>]` attributes |
| `Ionide.../Types.cg.fs` | `LSPAny = JToken`; Newtonsoft `open` statements; `[<JsonConverter(…StringEnumConverter…)>]` |
| `Ionide.../Client.fs` | `TelemetryEvent: JToken -> …`; `WorkspaceConfiguration: … -> AsyncLspResult<JToken[]>` |
| `Lsp/Server.fs` | `jeToJToken`/`jtokenToJe` in `wrapHandler`; `sanitizeCompletionItem`/`clampPositionFields` |
| `Lsp/Client.fs` | `jeToJToken`/`jtokenToJe` shims; `SelectToken` error-field extraction |
| `Util.fs` | `jeToJToken`, `jtokenToJe` |
| `Runtime/ServerStateLoop.fs` | `SelectToken(...)` JsonPath call |
| `Handlers/Debug.fs` | `(_params: JObject)` |
| `Handlers/*.fs` | `serialize`/`deserialize` call sites |
| `tests/Tooling.fs` | `JObject.Parse(m.Message)[…]`; `jeToJToken`/`jtokenToJe`; `indexJToken` |
| `tests/CompletionTests.fs` | `serialize item :?> JObject` |
| `tests/DiagnosticTests.fs` | `deserialize<…> (token: JToken)` |

---

## Approach

Three layers, now that Phase 0 (wire) is done:

**Layer 1 — Native STJ serializer in Ionide (Phase 1)**
Rewrite `OptionConverter.fs` and `JsonUtils.fs` with STJ `JsonConverter<'T>` / `JsonConverterFactory`
implementations.  Rewrite `LanguageServerProtocol.fs` `Server.serialize`/`deserialize` to use a
`JsonSerializerOptions` instance configured with those converters.  Add `jeToJToken`/`jtokenToJe`
as **internal** helpers and register a Newtonsoft `JsonConverter<JsonElement>` on
`defaultJsonRpcFormatter` so StreamJsonRpc can continue to round-trip `JsonElement`-typed fields.
`Server.start*`, `defaultJsonRpcFormatter`, and the `Client` module are otherwise untouched.

**Layer 2 — LSP type changes in Ionide (Phase 2)**
Change `LSPAny = JsonElement` in `Types.cg.fs`.  Change `JToken option` → `JsonElement option` in
`JsonRpc.fs` envelope types.  Fix the two explicit `JToken` member signatures in `Client.fs`.

**Layer 3 — Remove shims from `csharp-ls`; update tests (Phases 3–4)**
Delete `jeToJToken`/`jtokenToJe` from `Util.fs`.  Update `Lsp/Server.fs` and `Lsp/Client.fs` to
call `serialize`/`deserialize` directly with no wrapping.  Update tests.

---

## Detailed Plan

### Phase 1 — Native STJ serializer; StreamJsonRpc compat

**All edits are inside `src/Ionide.LanguageServerProtocol/src/`.**

#### 1a. Rewrite `OptionConverter.fs`

Keep the `Converters` sub-module (`memorise`, drop `memoriseByHash`).  Keep `UnionInfo`,
`CaseInfo`, `Type` helpers — they are pure reflection and need no changes.

Replace `OptionConverter` (Newtonsoft `JsonConverter`) with a STJ factory + typed converter:

```fsharp
type FSharpOptionConverter<'T>(options: JsonSerializerOptions) =
    inherit JsonConverter<'T option>()
    override _.Read(reader, _, opts) =
        if reader.TokenType = JsonTokenType.Null then None
        else Some (JsonSerializer.Deserialize<'T>(&reader, opts))
    override _.Write(writer, value, opts) =
        match value with
        | None   -> writer.WriteNullValue()
        | Some v -> JsonSerializer.Serialize(writer, v, opts)

type FSharpOptionConverterFactory() =
    inherit JsonConverterFactory()
    override _.CanConvert(t) = Type.isOption t
    override _.CreateConverter(t, opts) =
        let inner = t.GetGenericArguments()[0]
        let converterType = typedefof<FSharpOptionConverter<_>>.MakeGenericType(inner)
        Activator.CreateInstance(converterType, opts) :?> JsonConverter
```

Key: cache via `memorise` keyed by exact `Type`, not by hash, eliminating the collision bug.

#### 1b. Rewrite `JsonUtils.fs`

Remove all `open Newtonsoft.*`.  The `OptionAndCamelCasePropertyNamesContractResolver` class
is no longer needed (STJ uses `JsonNamingPolicy.CamelCase` directly); delete it.

Replace each converter:

| Old (Newtonsoft) | New (STJ) |
|---|---|
| `StrictNumberConverter` | Drop — STJ is strict by default |
| `StrictStringConverter` | Drop — STJ is strict by default |
| `StrictBoolConverter` | Drop — STJ is strict by default |
| `SingleCaseUnionConverter` | `SingleCaseUnionConverterFactory` + `SingleCaseUnionConverter<'T>`: `Write` emits `string value`; `Read` matches by case-insensitive name via `UnionInfo` |
| `ErasedUnionConverter` | `ErasedUnionConverterFactory` + `ErasedUnionConverter<'T>`: see notes below |

`ErasedUnionConverter` STJ implementation notes:

- `Utf8JsonReader` is forward-only — call `JsonDocument.ParseValue(ref reader)` at the top of
  `Read` to capture the token, then work from the resulting `JsonDocument` for all retry passes.
- Three-pass strategy preserved: primitive match → `UnionKindAttribute` match → field-subset match.
- Catch only `JsonException` per attempt, never `with _ ->`.  Any non-JSON exception propagates.
- `Write`: unwrap the single field value and delegate to `JsonSerializer.Serialize`.

#### 1c. Rewrite `LanguageServerProtocol.fs` — `Server` module

Add `open System.Text.Json` and `open System.Text.Json.Serialization`.

Build the STJ options instance:

```fsharp
let lspSerializerOptions =
    let opts = JsonSerializerOptions(
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        PropertyNamingPolicy  = JsonNamingPolicy.CamelCase)
    opts.Converters.Add(FSharpOptionConverterFactory())
    opts.Converters.Add(ErasedUnionConverterFactory())
    opts.Converters.Add(SingleCaseUnionConverterFactory())
    opts
```

Replace `serialize`/`deserialize`:

```fsharp
let deserialize<'t> (element: JsonElement) : 't =
    JsonSerializer.Deserialize<'t>(element, lspSerializerOptions)

let serialize<'t> (o: 't) : JsonElement =
    JsonSerializer.SerializeToElement(o, lspSerializerOptions)
```

**StreamJsonRpc compat — `JsonElementConverter`**

`defaultJsonRpcFormatter` uses Newtonsoft to serialize LSP types when `Server.start*` is
called.  Once `LSPAny = JsonElement`, any LSP type that contains an `LSPAny`-typed field
will present a `JsonElement` to Newtonsoft, which does not know how to handle it.  Fix this
by adding a Newtonsoft `JsonConverter<JsonElement>` and registering it on the formatter:

```fsharp
// Internal bridge helpers — used only by JsonElementConverter below
let internal jeToJToken (je: JsonElement) : JToken =
    JToken.Parse(je.GetRawText())

let internal jtokenToJe (token: JToken) : JsonElement =
    use doc = JsonDocument.Parse(token.ToString(Formatting.None))
    doc.RootElement.Clone()

/// Newtonsoft converter that lets StreamJsonRpc round-trip JsonElement fields.
type JsonElementConverter() =
    inherit JsonConverter<JsonElement>()
    override _.WriteJson(writer, value, _serializer) =
        jeToJToken(value).WriteTo(writer)
    override _.ReadJson(reader, _t, _existing, _serializer) =
        jtokenToJe (JToken.ReadFrom(reader))
```

Register it in `defaultJsonRpcFormatter`:

```fsharp
let defaultJsonRpcFormatter () =
    let fmt = new JsonMessageFormatter()
    // ... existing settings ...
    fmt.JsonSerializer.Converters.Add(JsonElementConverter())   // ← new line
    fmt
```

`Server.start*`, `requestHandling`, `serverRequestHandling`, `defaultRequestHandlings`,
`startWithSetupCore`, and the `Client` module are **untouched**.

#### 1d. Update `Ionide.LanguageServerProtocol.fsproj`

No package references change — Newtonsoft and StreamJsonRpc remain.  If the project targets
.NET 6 or later, `System.Text.Json` is in-box and needs no `<PackageReference>`.  Confirm TFM.

#### Invariant after Phase 1

- `dotnet build` produces zero errors.
- `dotnet test` passes in full.
- `OptionConverter.fs` and `JsonUtils.fs` have zero `open Newtonsoft.*` references.
- `LanguageServerProtocol.fs` `Server.serialize`/`deserialize` use STJ.
- `defaultJsonRpcFormatter` registers `JsonElementConverter`; `Server.start*` still compiles
  and works.
- The `jeToJToken`/`jtokenToJe` shims in `Lsp/Server.fs` and `Lsp/Client.fs` still compile
  (those files are unchanged in this phase).

---

### Phase 2 — Update code generator; regenerate `Types.cg.fs` / `ClientServer.cg.fs`

`Types.cg.fs` and `ClientServer.cg.fs` are **generated files** — edits to them are overwritten
the next time the generator runs.  The correct fix is to update `GenerateTypes.fs` so that
regeneration produces STJ-compatible output, then regenerate.  Also update the two hand-written
Ionide files (`JsonRpc.fs`, `Client.fs`) that contain explicit `JToken` types.

#### 2a. `GenerateTypes.fs` — update code emission

Six targeted changes, all in `src/Ionide.LanguageServerProtocol/tools/MetaModelGenerator/`:

**① Replace the `JToken` AST node alias with `JsonElement`**

```fsharp
// Before
open Newtonsoft.Json.Linq
let JToken = LongIdent(nameof JToken)

// After  (remove the Newtonsoft open; rename the binding)
let JsonElement = LongIdent "JsonElement"
```

All three sites that reference `JToken` in the generator (`createField` empty-structure
fallback ×2, `createTypeAlias` LSPAny special-case ×1) now automatically emit `JsonElement`.

**② `createTypeAlias` LSPAny special-case** (line ~649)

```fsharp
// Before
if alias.Name = "LSPAny" then
    JToken, []

// After
if alias.Name = "LSPAny" then
    JsonElement, []
```

**③ Empty `StructureLiteralType` fallbacks** (two sites in `createField` and `createTypeAlias`)

```fsharp
// Before
JToken, None, []   // in createField
JToken, []         // in createTypeAlias

// After
JsonElement, None, []
JsonElement, []
```

These are now handled automatically by renaming the binding in ①, but list them explicitly for
review during implementation.

**④ Generated file header `open` statements** (in `generateType`)

```fsharp
// Before
Open("Newtonsoft.Json")
Open("Newtonsoft.Json.Linq")

// After
Open("System.Text.Json")
```

**⑤ `StringEnumConverter` attribute on closed string enums** (in `createEnumeration`)

```fsharp
// Before
enum.attribute (Attribute("JsonConverter(typeof<Converters.StringEnumConverter>)"))

// After — drop the attribute entirely
enum   // no .attribute(...) call
```

The global `SingleCaseUnionConverterFactory` registered on `lspSerializerOptions` handles
these DUs by reflection; no per-type attribute is needed.  Verify each annotated type is a
zero-field DU (all closed string enums from the meta-model are).

**⑥ `JsonProperty(NullValueHandling = NullValueHandling.Include)` attribute** (in `createField`
`OrType` arm, emitted for nullable union fields that must not be omitted)

```fsharp
// Before
Some(Attribute "JsonProperty(NullValueHandling = NullValueHandling.Include)")

// After
Some(Attribute "JsonInclude")
```

`[<JsonInclude>]` (STJ) marks a property for inclusion even when it would otherwise be skipped.
For fields where the intent was specifically `NullValueHandling.Include` (i.e. the field must
appear in the JSON even when null), also add a per-property `JsonIgnore(Condition =
JsonIgnoreCondition.Never)` override — but first audit whether any generated field actually
reaches this code path and what the LSP spec requires.

#### 2b. Regenerate the generated files

After updating the generator, run it to produce fresh `Types.cg.fs` and `ClientServer.cg.fs`:

```bash
cd src/Ionide.LanguageServerProtocol/tools/MetaModelGenerator
dotnet run -- types \
  --meta-model-path ../../data/3.17.0/metaModel.json \
  --output-file-path ../../src/Types.cg.fs
dotnet run -- client-server \
  --meta-model-path ../../data/3.17.0/metaModel.json \
  --output-file-path ../../src/ClientServer.cg.fs
```

Verify the diff: `Types.cg.fs` should have `open System.Text.Json`, `type LSPAny = JsonElement`,
and no `StringEnumConverter` attributes.  `ClientServer.cg.fs` should be unchanged (the client/
server generator emits no Newtonsoft-specific code).

#### 2c. `JsonRpc.fs` (Ionide envelope types — hand-edited)

This file is **not** generated; edit it directly.

- Remove `open Newtonsoft.Json.Linq`.
- Change field types: `Params: JToken option` → `Params: JsonElement option` in `Request` and
  `Notification`; `Result: JToken option` → `Result: JsonElement option` in `Response`;
  `Data: JToken option` → `Data: JsonElement option` in `Error`.
- Retain `[<JsonProperty("jsonrpc")>]` and `[<JsonProperty(NullValueHandling = …)>]` —
  `Server.start*` and the `Client` module still serialize these envelope types through Newtonsoft
  on the StreamJsonRpc path, so the attributes remain meaningful.
- Keep the `Requests` sub-module (uses `StreamJsonRpc.LocalRpcException`).
- The `Client` module's `messageHandler` accesses `request.Params` and `notification.Params`.
  After the field type changes to `JsonElement option`, update those access sites accordingly —
  the `JsonElementConverter` registered on the `Client` module's own `jsonSerializer` handles
  round-tripping `JsonElement` values through Newtonsoft.

#### 2d. `Client.fs` (Ionide `LspClient` abstract class — hand-edited)

Change the two explicitly `JToken`-typed abstract members:

- `abstract member TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>`
  → `abstract member TelemetryEvent: LSPAny -> Async<unit>`
- `abstract member WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken[]>`
  → `abstract member WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<LSPAny[]>`

Update the `interface ILspClient with` block identically.

#### Invariant after Phase 2

- `dotnet build` produces zero errors; `dotnet test` passes in full.
- `Types.cg.fs`: `LSPAny = JsonElement`; `open System.Text.Json`; no `StringEnumConverter`
  attributes; no `Newtonsoft` references.
- `ClientServer.cg.fs`: unchanged (generator emits no Newtonsoft code for this file).
- `JsonRpc.fs`: `Params`/`Result`/`Data` are `JsonElement option`.
- `Client.fs`: no explicit `Newtonsoft.Json.Linq.JToken` in public member signatures.
- `LSPAny`-typed fields in all LSP record types now hold `JsonElement` values; the
  `JsonElementConverter` on `defaultJsonRpcFormatter` handles them transparently on the
  StreamJsonRpc path.

---

### Phase 3 — Remove shims; update `Lsp/Server.fs`, `Lsp/Client.fs`, handlers

#### 3a. `Util.fs`

Delete `jeToJToken` and `jtokenToJe`.  Confirm no remaining callers (there should be none after
the edits below).  Delete `nullJE`/`emptyObjectJE`/`emptyArrayJE` if only used by shim paths.

#### 3b. `Lsp/Server.fs`

- `wrapHandler` params path: remove `jeToJToken` — `deserialize` now takes `JsonElement` directly.
- `wrapHandler` result path: remove `jtokenToJe` — `serialize` now returns `JsonElement` directly.
- `sanitize: JToken -> JToken` → `sanitize: JsonElement -> JsonElement`.
- `clampPositionFields` / `sanitizeCompletionItem`: rewrite mutation using
  `System.Text.Json.Nodes` (`JsonObject` / indexer assignment) instead of `JObject`.  The
  existing `CompletionTests` verify correctness end-to-end.  Consider whether this workaround
  can be removed entirely now that `ErasedUnionConverter` only catches `JsonException` — if the
  STJ converter handles out-of-range `uint32` cleanly, remove the sanitizer and add a targeted
  `PositionConverter` instead.
- Remove `open Newtonsoft.*`.

#### 3c. `Lsp/Client.fs`

- Remove `jtokenToJe (serialize x)` — `serialize x` returns `JsonElement` directly.
- Remove `jeToJToken(response)` — `deserialize` takes `JsonElement` directly.
- Replace `SelectToken("code")` / `SelectToken("message")` with
  `JsonElement.TryGetProperty "code"` / `TryGetProperty "message"`.
- Remove `open Newtonsoft.*`.

#### 3d. `Runtime/ServerStateLoop.fs`

Replace `cc.Experimental |> Option.map _.SelectToken("...")` with an explicit
`JsonElement.TryGetProperty` chain.

#### 3e. `Handlers/Debug.fs`

Replace `(_params: JObject)` with `(_params: JsonElement)`.

#### 3f. Handler call sites (`Handlers/*.fs`)

`serialize`/`deserialize` resolve via `open Ionide.LanguageServerProtocol.Server` — same `open`
as before, same function names.  Because they now accept/return `JsonElement` and
`LSPAny = JsonElement`, the following patterns compile unchanged:

- `registerOptions |> serialize |> Some` — returns `JsonElement`; field is `LSPAny option`; ✓
- `Data = lensData |> serialize |> Some` — same; ✓
- `deserialize<CSharpCodeActionResolutionData> data` — `data: LSPAny` = `JsonElement`; ✓
- `Arguments = [| arg |> serialize |]` — `LSPAny[]` = `JsonElement[]`; ✓

Verify each handler compiles; no call-site edits should be needed.

#### Invariant after Phase 3

- Zero references to `JToken`, `JObject`, `JValue`, `jeToJToken`, `jtokenToJe` in
  `src/CSharpLanguageServer/`.
- `dotnet build` zero errors; `dotnet test` passes in full.

---

### Phase 4 — Update tests

#### `tests/Tooling.fs`

- Delete `indexJToken` helper.
- Replace `JObject.Parse(m.Message)[…]` accesses with
  `JsonDocument.Parse(m.Message).RootElement.TryGetProperty(…)`.
- Remove `jeToJToken`/`jtokenToJe` call sites.
- Remove `open Newtonsoft.*`.

#### `tests/JsonRpcTests.fs`

- Replace remaining `JObject`/`JValue` message construction with `JsonObject`/`JsonValue`
  (`System.Text.Json.Nodes`) or raw JSON strings.
- Remove `open Newtonsoft.*`.

#### `tests/CompletionTests.fs`

- `serialize item :?> JObject` no longer compiles (`serialize` returns `JsonElement`).
  Replace the mutation with: `JsonSerializer.SerializeToNode(item, lspSerializerOptions)` cast
  to `JsonObject`, mutate via indexer, convert back with
  `JsonSerializer.SerializeToElement(node, lspSerializerOptions)`.
  Or, if `sanitizeCompletionItem` was removed in Phase 3, replace with a direct unit test of
  `PositionConverter`.

#### `tests/DiagnosticTests.fs`

- `deserialize<WorkspaceDiagnosticReport> (token: JToken)` → pass `JsonElement` directly.

#### `tests/LocaleTests.fs`

- Fully-qualified `Ionide.LanguageServerProtocol.Server.serialize` still resolves; no change
  needed unless it also does `JToken`-specific operations on the result.

#### Invariant after Phase 4

- `dotnet build` zero errors; `dotnet test` passes in full.
- Zero `open Newtonsoft.*` in `tests/` (Newtonsoft remains a transitive dep but is not opened).
- `SourceGeneratorTests` compile-order pin can be removed — `memoriseByHash` is gone from
  `OptionConverter.fs`.

---

## Files Affected

### Already done (Phase 0)
```
src/CSharpLanguageServer/Runtime/JsonRpc.fs              STJ wire layer complete
src/CSharpLanguageServer/Util.fs                         jeToJToken / jtokenToJe shims (temporary)
src/CSharpLanguageServer/Lsp/Server.fs                   wrapHandler shim bridge (temporary)
src/CSharpLanguageServer/Lsp/Client.fs                   outbound shim bridge (temporary)
tests/CSharpLanguageServer.Tests/Tooling.fs              RpcMessageLogEntry.Message: string
tests/CSharpLanguageServer.Tests/JsonRpcTests.fs         JsonElement-based assertions
```

### Phase 1 (Ionide sources — serializer rewrite + StreamJsonRpc compat)
```
src/Ionide.LanguageServerProtocol/src/OptionConverter.fs     REWRITE — STJ FSharpOptionConverter +
                                                              factory; drop memoriseByHash
src/Ionide.LanguageServerProtocol/src/JsonUtils.fs           REWRITE — STJ ErasedUnion +
                                                              SingleCase + factories; drop Strict*
                                                              converters + contract resolver
src/Ionide.LanguageServerProtocol/src/LanguageServerProtocol.fs  NEW lspSerializerOptions + STJ
                                                              serialize/deserialize; ADD
                                                              jeToJToken/jtokenToJe (internal) +
                                                              JsonElementConverter registered on
                                                              defaultJsonRpcFormatter; Server.start*
                                                              and Client module untouched
```

### Phase 2 (generator update + regeneration + hand-edited Ionide files)
```
src/Ionide.LanguageServerProtocol/tools/MetaModelGenerator/GenerateTypes.fs
                                                             ① rename JToken binding → JsonElement
                                                             ④ generated opens: Newtonsoft → STJ
                                                             ⑤ drop StringEnumConverter attribute
                                                             ⑥ JsonProperty NullValueHandling →
                                                                JsonInclude / JsonIgnore(Never)
src/Ionide.LanguageServerProtocol/src/Types.cg.fs            REGENERATED (do not hand-edit)
src/Ionide.LanguageServerProtocol/src/ClientServer.cg.fs     REGENERATED (verify no diff)
src/Ionide.LanguageServerProtocol/src/JsonRpc.fs             Params/Result/Data: JToken option →
                                                              JsonElement option; update Client module
                                                              Params accesses
src/Ionide.LanguageServerProtocol/src/Client.fs              TelemetryEvent/WorkspaceConfiguration
                                                              explicit JToken → LSPAny
```

### Phase 3 (csharp-ls sources)
```
src/CSharpLanguageServer/Util.fs                         DELETE jeToJToken / jtokenToJe
src/CSharpLanguageServer/Lsp/Server.fs                   remove shims; rewrite sanitize/clamp
                                                         with System.Text.Json.Nodes
src/CSharpLanguageServer/Lsp/Client.fs                   remove shims; TryGetProperty for errors
src/CSharpLanguageServer/Runtime/ServerStateLoop.fs      SelectToken → TryGetProperty chain
src/CSharpLanguageServer/Handlers/Debug.fs               JObject → JsonElement param
src/CSharpLanguageServer/Handlers/*.fs                   verify compile (most need no changes)
```

### Phase 4 (tests)
```
tests/CSharpLanguageServer.Tests/Tooling.fs              remove indexJToken; JObject.Parse →
                                                         JsonDocument.Parse
tests/CSharpLanguageServer.Tests/JsonRpcTests.fs         JObject/JValue → JsonObject/JsonValue
tests/CSharpLanguageServer.Tests/CompletionTests.fs      serialize :?> JObject → JsonObject
                                                         mutation via STJ Nodes
tests/CSharpLanguageServer.Tests/DiagnosticTests.fs      deserialize JToken arg → JsonElement arg
```

---

## Risks & Open Questions

- **`ErasedUnionConverter` retry with `Utf8JsonReader`** — `Utf8JsonReader` is a forward-only
  value type; retrying across multiple union arms requires capturing the raw token first.  Use
  `JsonDocument.ParseValue(ref reader)` at the top of `Read` and work from the document for all
  subsequent passes.

- **`JsonElement` lifetime in `jtokenToJe`** — the internal bridge helper calls `.Clone()` before
  returning, so the result is lifetime-independent.  `JsonSerializer.SerializeToElement` also
  returns a self-owned element.  No additional `.Clone()` calls are needed at handler call sites,
  but ownership should be documented on the internal helpers.

- **StreamJsonRpc `JsonElementConverter` round-trip fidelity** — the converter serializes
  `JsonElement` by writing via `JToken.WriteTo` (lossless) and deserializes by reading a
  `JToken` and converting via string round-trip.  This is the same string round-trip used in the
  existing Phase 0 shims; no new fidelity risk is introduced.

- **`[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` removal** — safe only once
  `SingleCaseUnionConverterFactory` is registered globally in `lspSerializerOptions`.  The
  generator change drops this attribute from all closed string enums.  Verify that all such
  enums are zero-field DUs (true for all LSP meta-model enumerations) before merging.

- **Generator round-trip** — after updating `GenerateTypes.fs`, always regenerate and commit
  `Types.cg.fs` in the same PR.  A stale generated file is a build break waiting to happen.
  Consider adding a CI step that re-runs the generator and asserts no diff.

- **`JsonRpc.fs` `Client` module `Params` access** — `Client.messageHandler` reads
  `request.Params` and `notification.Params`.  After these fields become `JsonElement option`,
  the `Client` module's internal Newtonsoft path must handle `JsonElement` values — either via
  `JsonElementConverter` (already registered on its own `jsonSerializer`) or by updating
  `notificationHandling` to work with `JsonElement` directly.

- **`sanitizeCompletionItem` removal opportunity** — `ErasedUnionConverter` in the new STJ
  implementation catches only `JsonException`, not all exceptions.  If out-of-range `uint32`
  now produces a clear `JsonException` (or is handled by a dedicated `PositionConverter`), the
  pre-deserialization sanitizer can be deleted.  The existing `CompletionTests` gate this.

- **`U2`/`U3`/`U4` deserialize ambiguity** — "first non-throwing wins" can produce wrong results
  when two arms have overlapping shapes.  This is the same fundamental ambiguity as the existing
  Newtonsoft implementation; document it but do not change the strategy.

- **`MetaModelGenerator` Newtonsoft parse dependency** — the generator uses Newtonsoft to
  deserialize `metaModel.json` (in `Program.fs` and `MetaModel.fs`).  This parse dependency
  is unrelated to the code-emission changes in Phase 2 and is left as-is.  The generator's
  own `Newtonsoft.Json` `<PackageReference>` stays.

---

## Success Criteria

- `dotnet build` produces zero errors.
- `dotnet test` passes in full, including `CompletionTests`, `JsonRpcTests`, `SourceGeneratorTests`.
- `SourceGeneratorTests` no longer needs to be pinned before `JsonRpcTests` in compile order
  (`memoriseByHash` removed from `OptionConverter.fs`).
- Zero references to `JToken`, `JObject`, `JValue`, `jeToJToken`, `jtokenToJe` in
  `src/CSharpLanguageServer/` source files.
- `LSPAny = JsonElement` in `Types.cg.fs`; no Newtonsoft type appears in any public member
  signature in `Client.fs` or `ClientServer.cg.fs`.
- `jeToJToken`/`jtokenToJe` exist in exactly one place: as `internal` helpers in
  `LanguageServerProtocol.fs`, used only by `JsonElementConverter`.
- `Server.serialize`/`Server.deserialize` use `JsonSerializerOptions` with native STJ converters.
- Newtonsoft.Json and StreamJsonRpc remain as dependencies of
  `Ionide.LanguageServerProtocol.fsproj`; `defaultJsonRpcFormatter` and `Server.start*` continue
  to function correctly with `JsonElement`-typed LSP fields via `JsonElementConverter`.
