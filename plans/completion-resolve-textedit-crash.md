# Fix: `completionItem/resolve` crash on invalid `textEdit` positions

## Background

When a client sends a `completionItem/resolve` request, it echoes back the
`CompletionItem` it received from the server's earlier `textDocument/completion`
response.  Some editors (e.g. Neovim's built-in LSP client) populate the
`textEdit` field with sentinel values such as `"line": -1, "character": -1`
when the completion item has no real text edit range.

The server crashes with:

```
System.Exception: Could not create an instance of the type 'U2`2'
   at Ionide.LanguageServerProtocol.JsonUtils.ErasedUnionConverter.ReadJson ...
   at Ionide.LanguageServerProtocol.JsonUtils.OptionConverter.ReadJson ...
```

## Root Cause

The crash is caused by two cooperating issues.

### Issue 1 — `uint32` cannot hold `-1` (overflow)

`Position` in `Types.cg.fs` (Ionide.LanguageServerProtocol) is:

```fsharp
type Position = {
  Line: uint32
  Character: uint32
}
```

When the client sends `"line": -1, "character": -1`, Newtonsoft.Json throws an
overflow exception trying to deserialize `-1` into `uint32`.

### Issue 2 — `ErasedUnionConverter.tryReadAllMatchingFields` swallows the real error

`CompletionItem.TextEdit` is typed as `U2<TextEdit, InsertReplaceEdit> option`.
`ErasedUnionConverter.ReadJson` tries each union case via
`tryReadAllMatchingFields`, which is:

```fsharp
let tryReadAllMatchingFields (json: JToken) (targetType: Type) =
  try
    ...
    if fields |> Seq.forall (fun f -> props |> Seq.contains f) then
      json.ToObject(targetType, serializer) |> Some   // <-- throws on uint32 overflow
    else
      None
  with _ ->   // <-- overflow exception is silently swallowed here
    None
```

Resolution order for `U2<TextEdit, InsertReplaceEdit>`:

1. **`TextEdit`** (`{ Range, NewText }`) — field names `newText` + `range`
   match the JSON exactly, so the structural check passes.  `json.ToObject`
   then throws because `-1` overflows `uint32`.  The `with _ -> None` swallows
   it → returns `None`.

2. **`InsertReplaceEdit`** (`{ NewText, Insert, Replace }`) — the JSON has
   `range`, which is not a property of `InsertReplaceEdit` (`insert`/`replace`
   are), so the field-subset check correctly returns `None`.

Both cases fail → `ErasedUnionConverter` throws "Could not create an instance
of the type 'U2\`2'" → `OptionConverter` re-throws it → `wrapHandler` in
`Server.fs` propagates it as a -32603 Internal Error to the client.

## Affected Code

| Location | File | Detail |
|---|---|---|
| Ionide (library) | `src/JsonUtils.fs` | `tryReadAllMatchingFields` catch-all hides real deserialization failures |
| Ionide (library) | `src/Types.cg.fs` | `Position.Line` / `Position.Character` typed as `uint32` |
| csharp-ls | `src/CSharpLanguageServer/Lsp/Server.fs` | `wrapHandler` — `deserialize` is called on raw params with no pre-processing |
| csharp-ls | `src/CSharpLanguageServer/Handlers/Completion.fs` | `resolve` fn — does not use `TextEdit` but still crashes on deserialization |

## Proposed Fix

### Option 1 — Fix in csharp-language-server only (no Ionide changes required)

The crash originates from `-1` being sent in the `line`/`character` fields of
the `textEdit` range.  Per the LSP spec, negative position values are valid
and default to 0.  The sanitizer should therefore clamp them rather than
remove `textEdit` entirely — this is more correct and preserves the field in
case downstream code ever needs it.

Add a `callHandlerSanitized` variant of `callHandler` in `Server.fs` that
accepts a `JToken -> JToken` pre-processing function applied between the
params lookup and `deserialize`:

```fsharp
// In Server.fs — new wrapHandler overload with a sanitizer parameter
let wrapHandlerWithSanitizer
    (unwrapResult: LspResult<_> -> 'r)
    (requestMode: RequestMode)
    (sanitize: JToken -> JToken)
    fn
    jsonRpcCtx = async {
    let mutable wsUpdate = LspWorkspaceUpdate.Empty

    try
        let! ctx =
            stateActor.PostAndAsyncReply(fun rc ->
                EnterRequestContext(jsonRpcCtx.RequestOrdinal, jsonRpcCtx.MethodName, requestMode, rc))

        let fnParams =
            jsonRpcCtx.Params
            |> Option.defaultWith (fun _ -> Newtonsoft.Json.Linq.JValue.CreateNull())
            |> sanitize       // <-- new step
            |> deserialize

        let! result, wsUpdate' = fn ctx fnParams
        wsUpdate <- wsUpdate'

        return unwrapResult result
    finally
        stateActor.Post(LeaveRequestContext(jsonRpcCtx.RequestOrdinal, wsUpdate))
}

let callHandlerSanitized requestMode sanitize fn : JsonRpcCallHandler =
    wrapHandlerWithSanitizer unwrapResult requestMode sanitize fn
```

Then register `completionItem/resolve` with a sanitizer that walks into the
`textEdit.range` object and clamps any negative `line`/`character` values to 0:

```fsharp
/// Clamp negative position integers to 0 in-place on a JObject range node.
let private clampPosition (pos: JObject) =
    for name in [ "line"; "character" ] do
        match pos.TryGetValue(name) with
        | true, (:? JValue as v) when v.Type = JTokenType.Integer && v.Value<int>() < 0 ->
            v.Value <- 0
        | _ -> ()

/// Walk a completionItem JToken and clamp sentinel -1 positions to 0.
/// The LSP spec says negative position values default to 0.
let sanitizeCompletionItem (token: JToken) =
    match token with
    | :? JObject as obj ->
        match obj.TryGetValue("textEdit") with
        | true, (:? JObject as textEdit) ->
            match textEdit.TryGetValue("range") with
            | true, (:? JObject as range) ->
                for corner in [ "start"; "end" ] do
                    match range.TryGetValue(corner) with
                    | true, (:? JObject as pos) -> clampPosition pos
                    | _ -> ()
            | _ -> ()
        | _ -> ()
        obj :> JToken
    | _ -> token

|> Map.add "completionItem/resolve" (callHandlerSanitized ReadOnly sanitizeCompletionItem Completion.resolve)
```

This is a targeted, zero-dependency fix entirely within csharp-language-server
that aligns with the LSP spec's own defaulting behaviour.

### Option 2 — Fix in Ionide.LanguageServerProtocol

Requires changes to the upstream library and a version bump in
`Directory.Packages.props`.

#### Fix A — Split the `try/with` in `tryReadAllMatchingFields` (`JsonUtils.fs`)

Only the field-name enumeration should be guarded.  The actual
`json.ToObject` call must be allowed to propagate real exceptions so failures
are visible and the "no matching case" error message is never misleading.

```fsharp
// Before
let tryReadAllMatchingFields (json: JToken) (targetType: Type) =
  try
    ...
    if fields |> Seq.forall (fun f -> props |> Seq.contains f) then
      json.ToObject(targetType, serializer) |> Some
    else
      None
  with _ ->
    None

// After
let tryReadAllMatchingFields (json: JToken) (targetType: Type) =
  let structurallyMatches =
    try
      let fields = json.Children<JProperty>() |> Seq.map (fun f -> f.Name.ToLowerInvariant())
      let props  = targetType.GetProperties() |> Seq.map (fun p -> p.Name.ToLowerInvariant())
      fields |> Seq.forall (fun f -> props |> Seq.contains f)
    with _ ->
      false

  if structurallyMatches then
    json.ToObject(targetType, serializer) |> Some   // let real errors propagate
  else
    None
```

#### Fix B — Accept negative position values (`Types.cg.fs`)

The LSP spec states that negative line/character values default to 0, so they
are valid input.  Change `Position` to use `int` instead of `uint32`:

```fsharp
type Position = {
  Line: int       // was uint32
  Character: int  // was uint32
}
```

`Types.cg.fs` is auto-generated; the generator template in
`tools/MetaModelGenerator/` also needs updating to emit `int` for the
`uinteger` LSP primitive type (or at least for `Position` specifically).

With Fix B in place, `-1` deserializes without overflow, `TextEdit` is
selected correctly as `U2.C1`, and the `completionItem/resolve` call succeeds.
Fix A is independently valuable as a robustness improvement but is not
sufficient on its own.

### Recommendation

**Implement Option 1 now** as an immediate fix — it is self-contained, safe,
and reviewable without coordinating an upstream library release.  File an
issue / PR against Ionide.LanguageServerProtocol for Option 2 in parallel so
the root cause is addressed upstream.

## Testing

1. Start `csharp-ls --rpclog /tmp/rpc.log` against any C# project.
2. Trigger completion in an editor that sends sentinel `-1` positions in
   `textEdit` (Neovim with the built-in LSP client reproduces this).
3. Confirm `completionItem/resolve` returns a valid result with `Detail` /
   `Documentation` populated and no `-32603` error in the log.

## Note on the LSP spec contradiction

`uinteger` is the correct type for `Position.line` and `Position.character`
per the LSP 3.17 meta model (`metaModel.json`), and the Ionide generator maps
it faithfully to `uint32`:

```fsharp
// tools/MetaModelGenerator/MetaModel.fs
| Uinteger -> "uint32"
```

However, the spec's own documentation for `Position.line` reads:

> *"If a line number is negative, it defaults to 0."*

So the spec explicitly acknowledges that negative values can arrive as input,
but types the field as unsigned — making it impossible to represent them in a
strongly-typed deserializer without overflow.  The blame chain is:

**LSP spec contradiction → `uint32` in generated types → overflow on `-1`
→ `tryReadAllMatchingFields` silently swallows it → "Could not create an
instance of the type 'U2\`2'"**

The sanitizer implemented in Option 1 (clamping to 0 before deserialization)
is therefore the most correct response available: it enforces the defaulting
behaviour the spec describes in prose, even though the spec's own type system
cannot express it.  Changing `Position` fields to `int` in Ionide (Option 2,
Fix B) would accept negative wire values but would diverge from the meta model,
which is a trade-off worth noting in any upstream PR.

## Related

- LSP spec §3.17 Position: "If a line number is negative, it defaults to 0."
- LSP meta model `data/3.17.0/metaModel.json` — `Position.line` / `Position.character` typed as `uinteger`
- `ErasedUnionConverter` general design: `src/JsonUtils.fs` lines 140–256
- `OptionConverter`: `src/OptionConverter.fs` lines 1–150
