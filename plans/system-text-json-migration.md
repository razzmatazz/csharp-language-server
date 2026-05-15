# Migrate from Newtonsoft.Json to System.Text.Json

## Local Ionide Repo (read-only reference)

`~/src/Ionide.LanguageServerProtocol` is a local checkout of the upstream Ionide repo kept for
**reference only** — we do not modify it.  We consume the published NuGet package today, and the
migration ends by dropping it entirely.

Key files worth knowing for this migration:

| File | Role |
|---|---|
| `src/Types.cg.fs` | Generated — defines `LSPAny = JToken` (line 6451), `LSPArray`, `LSPObject`; `[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` on ~7 enum DUs (lines 6714, 6817, 6833, 7041, 7055, 7167, 7183) |
| `src/ClientServer.cg.fs` | Generated — `ILspClient` interface with `TelemetryEvent: LSPAny -> Async<unit>` and `WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<array<LSPAny>>` |
| `src/Client.fs` | `LspClient` abstract class implementing `ILspClient`; both `JToken`-typed members |
| `src/Server.fs` | `LspServer` abstract class (all handler stubs) |
| `src/OptionConverter.fs` | `OptionConverter`, `UnionInfo`, `memoriseByHash` (hash-collision bug lives here) |
| `src/JsonUtils.fs` | `ErasedUnionConverter`, `SingleCaseUnionConverter`, `Strict*Converter`, `OptionAndCamelCasePropertyNamesContractResolver` |
| `src/LanguageServerProtocol.fs` | `Server.serialize`/`Server.deserialize`; `defaultJsonRpcFormatter()` |
| `src/Types.fs` | `ErasedUnionAttribute`, `U2`, `U3`, `U4` |
| `src/JsonRpc.fs` | `LspResult`, `AsyncLspResult`, `Error`, `ILspServer`, `ILspClient` definitions |
| `src/TypeDefaults.fs` | Default values for generated LSP types |

---

## Motivation

`csharp-ls` currently uses `Newtonsoft.Json` (via `Ionide.LanguageServerProtocol`) for all LSP
message serialization and for the JSON-RPC wire layer.  Several bugs trace directly to Newtonsoft
quirks, and the dependency adds significant weight and runtime risk.  Known issues:

1. **`uint32` overflow** — `Position.Line`/`Character` are typed `uint32`; Newtonsoft throws
   when it encounters the sentinel value `-1` in LSP hover/completionItem params.  The exception
   is silently swallowed by `ErasedUnionConverter`'s `with _ -> None` catch-all, producing the
   wrong union case and a `-32603` error to the client.  A pre-deserialization clamping workaround
   (`sanitizeCompletionItem` in `Lsp/Server.fs`) exists but is fragile and covers only one call.

2. **`memoriseByHash` hash collision** — `OptionConverter.fs` caches reflection data keyed only by
   `Type.GetHashCode()`.  When `JsonRpcTests` and `SourceGeneratorTests` run in parallel the hash
   of `JObject`/`JToken` can collide with LSP union type hashes in the shared global cache,
   silently corrupting deserialization of `U2<Location, Location[]>`.  Worked around by fixing
   compile order in `.fsproj`.

3. **Silent exception swallowing** — `ErasedUnionConverter`'s `tryReadAllMatchingFields` catches
   all exceptions with `with _ -> None`, making deserialization bugs invisible and very hard to
   diagnose.

Goal: replace every `JToken`/`JObject`/`JValue`/`Newtonsoft.Json.*` reference with
`System.Text.Json` (`JsonElement`, `JsonDocument`, `Utf8JsonReader/Writer`, custom
`JsonConverter<'T>`) and **drop `Ionide.LanguageServerProtocol` entirely**, vendoring the LSP
types and rewriting `ILspClient`/`LspClient` to use `JsonElement` instead of `JToken`.

---

## Scope

### What `csharp-ls` uses from Newtonsoft directly

| Site | What | Notes |
|---|---|---|
| `Runtime/JsonRpc.fs` | `JObject` as wire envelope type; `JToken option` for `Params`/`WireId`; `JObject.Parse`; `JObject(JProperty(...))` constructors; `JValue.CreateNull()` | Core transport — everything flows through here |
| `Lsp/Server.fs` | `JToken -> JToken` sanitize pipeline; `Ionide.LanguageServerProtocol.Server.deserialize` / `serialize`; raw `JObject`/`JValue` field access for `clampPositionFields` / `sanitizeCompletionItem` | Deserialization bridge between wire and typed handlers |
| `Lsp/Client.fs` | `JToken` in `sendServerRequest`/`sendServerNotification` signatures; `JToken` return from `WorkspaceConfiguration`; `SelectToken` for error field extraction | Outbound call adapter |
| `Runtime/ServerStateLoop.fs` | `cc.Experimental |> Option.map _.SelectToken(...)` to read a boolean capability | Single `JToken.SelectToken` (JsonPath) usage |
| `Handlers/Debug.fs` | `(_params: JObject)` as handler param type | Intentionally untyped handler |
| `tests/Tooling.fs` | `indexJToken`, `JObject`, `JValue` construction | Test harness message tracking |
| `tests/JsonRpcTests.fs` | Pervasive `JObject`/`JValue` message construction | Unit tests for the transport |
| `tests/CompletionTests.fs` | Raw `JObject` to inject sentinel `-1` positions | Integration test for the clamping workaround |

### What `Ionide.LanguageServerProtocol` provides that we will vendor

| What | Where in Ionide | Migration action |
|---|---|---|
| `U2`, `U3`, `U4`, `ErasedUnionAttribute` | `src/Types.fs` | Copy verbatim — no Newtonsoft dependency |
| LSP record/DU types | `src/Types.cg.fs` | Copy; change `LSPAny = JToken` → `LSPAny = JsonElement`; remove `[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` attributes (our global STJ converter handles them by reflection) |
| Type defaults | `src/TypeDefaults.fs` | Copy verbatim |
| `LspResult`, `AsyncLspResult`, `ILspServer`, `ILspClient` | `src/JsonRpc.fs`, `src/ClientServer.cg.fs` | Copy; retype `TelemetryEvent` and `WorkspaceConfiguration` to use `JsonElement` instead of `JToken` |
| `LspServer` abstract class (all handler stubs) | `src/Server.fs` | Copy verbatim — no Newtonsoft dependency |
| `LspClient` abstract class | `src/Client.fs` | Copy; update the two `JToken`-typed members to `JsonElement` |
| `Mappings` module | (Ionide internal) | Copy or rewrite only the parts `csharp-ls` uses |

`Ionide.LanguageServerProtocol` also provides `StreamJsonRpc`-based server startup
(`Server.start`, `Server.startWithSetup`, etc.) that `csharp-ls` **does not use** — we have our
own `Runtime/JsonRpc.fs` transport.  Those parts can be dropped from the vendor copy.

---

## Approach

Four layers, tackled in order:

**Layer 0 — Wire layer first, with shims (Phase 0)**
Convert `Runtime/JsonRpc.fs` to STJ before touching any handler or LSP type.  A thin
`JToken`↔`JsonElement` round-trip shim in `wrapHandler` (and its mirror in `Lsp/Client.fs`)
lets all existing Newtonsoft-typed handlers keep working unchanged.  The project compiles
and all tests pass with both libraries in use.  This is the gradual-migration entry point.

**Layer A — Vendored LSP types (Phase 2)**
Copy Ionide's types into `src/CSharpLanguageServer/Lsp/` (or a new `Lsp/Protocol/` subtree),
replacing `JToken` with `JsonElement` throughout.  Once compiled, `csharp-ls` no longer imports
any symbol from `Ionide.LanguageServerProtocol`.

**Layer B — STJ serializer (Phase 1)**
Write `Runtime/LspSerializer.fs` with a `JsonSerializerOptions` instance and all converters.
This can be done before or in parallel with Layer A; it only touches our own files.

**Layer C — Wire layer cleanup (Phase 4)**
Remove the `JToken`↔`JsonElement` shims added in Phase 0 now that the handlers speak
`JsonElement` natively.  Phase 3 (old "wire layer rewrite") is subsumed by Phase 0; Phase 4
is the cleanup pass that deletes the temporary bridges.

---

## Detailed Plan

### Phase 0 — Wire layer first, with shims

**Goal**: convert `Runtime/JsonRpc.fs` to `System.Text.Json` while keeping every existing
Newtonsoft-typed handler working via a temporary round-trip shim.  After this phase
`dotnet test` passes with both Newtonsoft and STJ in the dependency closure.

#### Changes to `Runtime/JsonRpc.fs`

- Replace `open Newtonsoft.Json.Linq` with `open System.Text.Json` + `open System.Text.Json.Nodes`.
- `JsonRpcRequestContext.WireId`/`Params`: `JToken option` → `JsonElement option`.  Call
  `.Clone()` on any `JsonElement` stored in the context to prevent it from outliving the
  `JsonDocument` it came from.
- Inbound parse: `JObject.Parse json` → `JsonDocument.Parse(json).RootElement`.
- Field extraction: all `msg.SelectToken("x")` → `msg.TryGetProperty("x")` (single level);
  the `$/cancelRequest` two-level path `"params.id"` becomes two chained `TryGetProperty`
  calls.
- `OutboundMessage.Payload`: `JObject` → `JsonNode` (using `System.Text.Json.Nodes.JsonObject`
  for piecemeal construction).
- `makeError`/`makeErrorResponse`: build via `System.Text.Json.Nodes.JsonObject`.
- `JValue.CreateNull()` sentinel → a shared `JsonElement` parsed from `"null"`.
- Outbound serialisation: `msg.Payload |> string` → `msg.Payload.ToJsonString()`.
- `RpcRead`/`RpcWrite` log variants: change payload from `JObject` to `string` (raw JSON
  text).  This decouples `JsonRpcLogEntry` from any JSON-library type and keeps the DU
  stable for the rest of the migration.
- `sendJsonRpcNotification`/`sendJsonRpcCall`/`sendJsonRpcCallWithTimeout` public signatures:
  `JToken` → `JsonElement`.

#### Shim layer in `Lsp/Server.fs`

Add two private bridge helpers (deleted in Phase 4):

```fsharp
let private jeToJToken (je: JsonElement) : JToken =
    JToken.Parse(je.GetRawText())

let private jtokenToJe (token: JToken) : JsonElement =
    JsonDocument.Parse(token.ToString(Newtonsoft.Json.Formatting.None)).RootElement.Clone()
```

In `wrapHandler`:
- Params path: `jsonRpcCtx.Params |> Option.defaultWith … |> jeToJToken |> sanitize |> deserialize`
  (inserts `jeToJToken` between the wire-layer `JsonElement` and the Newtonsoft `sanitize`/
  `deserialize` pipeline).
- Result path (`unwrapResult`): wrap `serializeNullable`/`serialize` output with `jtokenToJe`
  before returning to the wire layer.

#### Shim layer in `Lsp/Client.fs`

`sendServerNotification` and `sendServerRequest_` delegates now accept `JsonElement`.
Wrap every `serialize x` call site with `jtokenToJe (serialize x)`.  Unwrap
`Result<JsonElement, JsonElement>` back to `JToken` (via `jeToJToken`) before passing to
the existing `SelectToken("code")`/`SelectToken("message")` error-extraction logic.

#### Forced changes to `tests/Tooling.fs`

These changes are mechanically required by the `JsonRpc.fs` type changes — they are not
optional cleanups:

- `RpcMessageLogEntry.Message: JObject` → `string` (raw JSON text, matching the new
  `RpcRead`/`RpcWrite` payload type).
- `makeRpcLogCallback`: pattern-match `RpcRead s`/`RpcWrite s` where `s: string`; store
  directly in the log entry.
- All `m.Message["method"]`, `m.Message["params"]`, `m.Message["id"]` accesses
  (~8 sites in `WaitForProgressEnd`, `GetProgressParams`, `ServerDidInvoke`,
  `ServerDidRespondTo`, `ClientDidSendNotification`, `ServerMessageLogContains`,
  `ServerProgressLogContains`): parse on demand — `JObject.Parse(m.Message)["..."]`
  (temporary; cleaned up in Phase 5).
- Inline call handlers in `configureRpcTransport`: return type is now
  `Result<JsonElement, JsonElement>`, so `Ok(JValue.CreateNull() :> JToken)` →
  `Ok(JsonDocument.Parse("null").RootElement)`; `workspace/configuration` result wrapped
  with `jtokenToJe`; `ctx.Params` fed through `jeToJToken` before passing to
  `buildConfigurationResponse`.
- `$/progress` notification handler: `p["value"]` (JToken indexer) →
  `p.TryGetProperty("value")` (JsonElement); replace `indexJToken` navigation with
  `TryGetProperty` chains (or a new `tryGetProp` helper).
- `sendJsonRpcNotification`/`sendJsonRpcCallWithTimeout` call sites: `JObject()` and
  `serialize x` arguments wrapped with `jtokenToJe`; `Result<JsonElement, …>` results
  unwrapped with `jeToJToken` before feeding Newtonsoft `deserialize`.

#### Forced changes to `tests/JsonRpcTests.fs`

`readMessage` return type changes from `JObject option` to `JsonElement option` (or the
caller must parse the raw bytes independently).  Update `countMessages`/`waitForMessages`
helpers and all assertion sites to use `JsonElement` / `TryGetProperty`.

#### Invariant after Phase 0

- `dotnet build` produces zero errors.
- `dotnet test` passes in full.
- `Runtime/JsonRpc.fs` has zero `open Newtonsoft.*` references.
- All other source files are unchanged or only have the shim additions described above.

---

### Phase 1 — Local `System.Text.Json` serializer

Write `src/CSharpLanguageServer/Runtime/LspSerializer.fs`.  This module owns a
`JsonSerializerOptions` instance configured with the converters listed below, and exposes:

```fsharp
val serialize   : 'T -> JsonElement
val deserialize : JsonElement -> 'T
val serializeToBytes   : 'T -> byte[]   // for use in JsonRpc writer path
val deserializeFromDoc : JsonDocument -> 'T
```

Converters to implement (matching Ionide's Newtonsoft stack):

| Converter | Behaviour to reproduce |
|---|---|
| `FSharpOptionConverter<'T>` | `None` ↔ JSON `null`; `Some x` ↔ value of `x` |
| `ErasedUnionConverter` (`U2`,`U3`,`U4`) | Serialize: write inner value unwrapped.  Deserialize: try each arm in order; first non-throwing wins.  **Only catch `JsonException`** — do not swallow all exceptions. |
| `SingleCaseUnionConverter` | Case-name string round-trip for enum-style DUs (zero-field cases).  Handles the types that Ionide annotated with `[<Converters.StringEnumConverter>]` globally without needing the attribute. |
| `LspAnyConverter` | `LSPAny = JsonElement`; read/write verbatim via `JsonElement.WriteTo` / `reader.GetRawText` |
| `CamelCaseNaming` | Use `JsonNamingPolicy.CamelCase` (built-in) |
| `WhenWritingNull` | `DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull` (replaces Newtonsoft `NullValueHandling.Ignore`) |
| Strict primitives | STJ is strict by default; no extra converters needed |

Key implementation notes:
- Use `memorise` (by exact `Type` key, not hash) to avoid the hash-collision bug from Ionide's
  `memoriseByHash`.
- `ErasedUnionConverter.ReadJson` should try primitive match first, then `UnionKindAttribute`
  match, then field-subset match — mirroring Ionide's three-pass strategy but with only
  `JsonException` caught per attempt.
- `FSharpOptionConverter` should treat `JsonValueKind.Null` as `None`; any other token
  deserializes the inner type and wraps in `Some`.

### Phase 2 — Vendor Ionide LSP types and rewrite `ILspClient`

Copy the following files from `~/src/Ionide.LanguageServerProtocol/src/` into
`src/CSharpLanguageServer/Lsp/Protocol/`:

| Ionide source | Destination | Changes |
|---|---|---|
| `Types.fs` | `Protocol/LspTypes.fs` | None — no Newtonsoft usage |
| `Types.cg.fs` | `Protocol/LspTypes.cg.fs` | Change `open Newtonsoft.Json.Linq` → `open System.Text.Json`; `LSPAny = JToken` → `LSPAny = JsonElement`; remove all `[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` attributes; remove `open Newtonsoft.Json` |
| `TypeDefaults.fs` | `Protocol/LspTypeDefaults.fs` | None — no Newtonsoft usage |
| `JsonRpc.fs` | `Protocol/LspJsonRpc.fs` | Remove Newtonsoft imports; keep `LspResult`, `AsyncLspResult`, `ILspServer`, `ILspClient` |
| `ClientServer.cg.fs` | `Protocol/LspClientServer.cg.fs` | Change `TelemetryEvent: LSPAny -> Async<unit>` and `WorkspaceConfiguration: … -> AsyncLspResult<array<LSPAny>>` — no type change needed once `LSPAny = JsonElement` |
| `Client.fs` | `Protocol/LspClient.fs` | Update `LspClient` abstract class: remove Newtonsoft imports; `TelemetryEvent`/`WorkspaceConfiguration` members now typed `JsonElement` / `JsonElement[]` naturally through `LSPAny` |
| `Server.fs` | `Protocol/LspServer.fs` | None — handler stubs have no Newtonsoft usage |

After copying, update `CSharpLanguageServer.fsproj`:
- Add all `Protocol/*.fs` files to `<Compile>` in the right order (before handlers).
- Remove `<PackageReference Include="Ionide.LanguageServerProtocol" />`.
- Remove all `open Ionide.LanguageServerProtocol.*` from `csharp-ls` source files; replace with
  the new local namespaces (keep the same namespace names if possible to minimise churn, e.g.
  `namespace Ionide.LanguageServerProtocol` in the vendored files).

`SelectToken` (JsonPath) has no direct STJ equivalent.  The one usage in `ServerStateLoop.fs`
reads a nested boolean capability; replace with explicit `JsonElement.TryGetProperty` chain.

### Phase 3 — Replace wire layer `JObject`/`JToken` with `JsonElement`

Rewrite `Runtime/JsonRpc.fs`:

- Incoming bytes → `JsonDocument.Parse` → `JsonElement` (replaces `JObject.Parse`).
- Envelope fields (`jsonrpc`, `id`, `method`, `params`, `result`, `error`) extracted via
  `JsonElement.TryGetProperty` + `GetString`/`GetRawText`/etc.
- `WireId` type: `JsonElement option` (was `JToken option`).
- `Params` type: `JsonElement option` (was `JToken option`).
- Response/error construction via `System.Text.Json.Nodes` (`JsonObject`, `JsonValue`) or
  a `Utf8JsonWriter` writing into a `MemoryStream` then parsed back to `JsonElement`.
- All `JValue.CreateNull()` → a reusable null `JsonElement` (parsed from `"null"`).
- `RpcRead`/`RpcWrite` log entries: stringify via `JsonElement.GetRawText()` or `ToString()`.

### Phase 4 — Update `Lsp/Server.fs`, `Lsp/Client.fs`, handler interfaces

- Replace `sanitize: JToken -> JToken` with `sanitize: JsonElement -> JsonElement`.
- Replace `clampPositionFields` / `sanitizeCompletionItem` with `System.Text.Json.Nodes`-based
  mutation, or remove entirely once the STJ `ErasedUnionConverter` / a `PositionConverter`
  handles out-of-range `uint32` values explicitly (returning `None` rather than overflowing).
- Replace all `Ionide.LanguageServerProtocol.Server.deserialize` / `serialize` calls with
  `LspSerializer.deserialize` / `LspSerializer.serialize`.
- `sendServerRequest` / `sendServerNotification` signatures: `JsonElement` replaces `JToken`.
- `WorkspaceConfiguration` return type: `JsonElement[]` (already correct once `LSPAny = JsonElement`).
- Error field extraction in `Lsp/Client.fs`: replace `SelectToken("code")` / `SelectToken("message")`
  with `JsonElement.TryGetProperty "code"` / `TryGetProperty "message"`.

Note on the `sanitizeCompletionItem` workaround: once `ErasedUnionConverter` only catches
`JsonException` and a `PositionConverter` or `FSharpOptionConverter` treats out-of-range `uint32`
as `None`, the `-1` sentinel produces a clear exception rather than a wrong union case.  The
pre-deserialization sanitizer can then be removed and verified by the existing `CompletionTests`.

### Phase 5 — Update tests

- `tests/Tooling.fs`: replace `indexJToken`, `JObject`/`JValue` construction with
  `JsonElement`/`JsonDocument`-based helpers.
- `tests/JsonRpcTests.fs`: rewrite message construction using `System.Text.Json.Nodes`
  (`JsonObject`, `JsonValue`) or raw JSON strings + `JsonDocument.Parse`.
- `tests/CompletionTests.fs`: the `-1`-injection test either adapts to the new wire format or
  is replaced by a direct unit test of the `PositionConverter`.

### Phase 6 — Drop `Ionide.LanguageServerProtocol` and `Newtonsoft.Json`

- Remove `<PackageReference Include="Ionide.LanguageServerProtocol" />` from the main project.
- Remove `<PackageReference Include="Newtonsoft.Json" />` from `Directory.Packages.props` and
  all project files.
- Remove `<PackageReference Include="StreamJsonRpc" />` if it was only a transitive dep of
  Ionide and is no longer needed (verify — `csharp-ls` does not use `StreamJsonRpc` directly).
- `dotnet build` must produce zero `open Newtonsoft.*` or `open Ionide.*` warnings.

---

## Files Affected

### Phase 0 only
```
src/CSharpLanguageServer/Runtime/JsonRpc.fs              STJ wire layer; RpcRead/Write → string
src/CSharpLanguageServer/Lsp/Server.fs                   add jeToJToken/jtokenToJe shims in wrapHandler
src/CSharpLanguageServer/Lsp/Client.fs                   wrap sendServerRequest/Notification with shims
tests/CSharpLanguageServer.Tests/Tooling.fs              RpcMessageLogEntry.Message → string; handler
                                                         return types → JsonElement; sendJsonRpc* callers
tests/CSharpLanguageServer.Tests/JsonRpcTests.fs         readMessage return type; assertion sites
```

### Phases 1–6 (cumulative)
```
src/CSharpLanguageServer/Lsp/Protocol/LspTypes.fs        NEW — vendored from Ionide Types.fs
src/CSharpLanguageServer/Lsp/Protocol/LspTypes.cg.fs     NEW — vendored Types.cg.fs, LSPAny=JsonElement
src/CSharpLanguageServer/Lsp/Protocol/LspTypeDefaults.fs NEW — vendored TypeDefaults.fs
src/CSharpLanguageServer/Lsp/Protocol/LspJsonRpc.fs      NEW — vendored JsonRpc.fs (LspResult etc.)
src/CSharpLanguageServer/Lsp/Protocol/LspClientServer.cg.fs  NEW — vendored ILspClient/ILspServer
src/CSharpLanguageServer/Lsp/Protocol/LspClient.fs       NEW — vendored LspClient (JsonElement typed)
src/CSharpLanguageServer/Lsp/Protocol/LspServer.fs       NEW — vendored LspServer stubs
src/CSharpLanguageServer/Runtime/LspSerializer.fs        NEW — local STJ serializer + converters
src/CSharpLanguageServer/Runtime/ServerStateLoop.fs      replace SelectToken
src/CSharpLanguageServer/Lsp/Server.fs                   remove shims; use LspSerializer directly
src/CSharpLanguageServer/Lsp/Client.fs                   remove shims; JsonElement-native
src/CSharpLanguageServer/Handlers/Debug.fs               replace JObject param
src/CSharpLanguageServer/CSharpLanguageServer.fsproj     add Protocol/*.fs, remove Ionide ref
tests/CSharpLanguageServer.Tests/Tooling.fs              remove JObject.Parse calls; full STJ cleanup
tests/CSharpLanguageServer.Tests/JsonRpcTests.fs         complete STJ rewrite of message construction
tests/CSharpLanguageServer.Tests/CompletionTests.fs      adapt or replace -1 injection test
Directory.Packages.props                                 remove Newtonsoft + Ionide + StreamJsonRpc
```

---

## Risks & Open Questions

- **Vendored file maintenance** — when a new LSP spec version ships in Ionide, we must manually
  port the diff to our vendored copies.  To keep this manageable: preserve the original Ionide
  file header comments with the source commit hash so diffs are easy to compute.

- **`StreamJsonRpc` transitive dep** — Ionide pulls in `StreamJsonRpc` which pulls in
  `Newtonsoft.Json`.  Once Ionide is dropped, verify whether any other remaining dep still
  references Newtonsoft; if not, it disappears entirely.

- **`Mappings` module** — Ionide's `Mappings.fs` (if `csharp-ls` uses it) may reference Ionide
  internals.  Audit usages and either vendor or inline the needed functions.

- **`[<JsonConverter(typeof<Converters.StringEnumConverter>)>]` removal** — these attributes are
  Newtonsoft-specific.  Removing them is safe because our global STJ `SingleCaseUnionConverter`
  handles the same DUs by reflection.  Verify each annotated type is actually a zero-field DU
  (the precondition for `SingleCaseUnionConverter`).

- **`JsonElement` lifetime / disposal** — `JsonDocument` must be disposed; `JsonElement` values
  from a disposed document become invalid.  The wire layer must document ownership clearly, or use
  `JsonElement.Clone()` where elements escape the document's lifetime.

- **`U2`/`U3`/`U4` deserialize ambiguity** — the "first non-throwing wins" strategy can produce
  wrong results when two arms have overlapping shapes.  This is the same fundamental ambiguity as
  Newtonsoft's `tryReadAllMatchingFields`; document it but do not change the strategy.

- **Performance** — `System.Text.Json` is generally faster than Newtonsoft; no regression
  expected.  Benchmark if needed.

---

## Success Criteria

- `dotnet build` produces zero references to `Newtonsoft.Json` or `Ionide.LanguageServerProtocol`
  in `csharp-ls` source files.
- `dotnet test` passes in full, including `CompletionTests`, `JsonRpcTests`, `SourceGeneratorTests`.
- `SourceGeneratorTests` no longer needs to be pinned before `JsonRpcTests` in compile order.
- The `sanitizeCompletionItem` workaround is either removed or reduced to a targeted
  `PositionConverter` with explicit out-of-range handling.
- `Newtonsoft.Json` and `Ionide.LanguageServerProtocol` are not `<PackageReference>` entries in
  any `csharp-ls` project file.
- `ILspClient.TelemetryEvent` and `ILspClient.WorkspaceConfiguration` are typed `JsonElement` /
  `JsonElement[]` with no conversion shims anywhere in the codebase.
