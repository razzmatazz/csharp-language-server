# Fix: server→client request errors crash `handleInitialized` (issue #16360)

## Background

[GitHub issue #16360](https://github.com/anthropics/claude-code/issues/16360) reports
that `csharp-ls` does not work with the Claude Code built-in LSP client.  The Claude
Code LSP client does not handle the three server→client requests that `csharp-ls`
sends during initialization:

- `client/registerCapability`
- `workspace/configuration`
- `window/workDoneProgress/create`

When a client does not implement those requests it returns a JSON-RPC error response
(typically `-32601 Method not found`).  That error triggers two problems on the
`csharp-ls` side: a crash in the shared request helper and missing capability checks
that should have prevented the calls in the first place.

## Root Cause

There are two cooperating bugs.

### Bug 1 — `failwith "TODO"` in `sendServerRequest` (`Lsp/Client.fs`)

`sendServerRequest` is the shared helper that wraps every outbound server→client
request.  Its `Result.Error` branch was never implemented:

```fsharp
let sendServerRequest m (p: JToken) : AsyncLspResult<'TResult> = async {
    let! result = sendServerRequest_ m p

    return
        match result with
        | Result.Ok jtoken -> jtoken |> deserialize |> Result.Ok
        | Result.Error err -> failwith "TODO"     // ← unconditionally throws
}
```

When a client returns any error response, this throws an exception that unwinds the
caller's async workflow.  In `handleInitialized`, each of the three outbound calls is
wrapped in its own `try/with`, so the exception is caught and logged — but the
`LspWorkspaceUpdate` being built is silently lost:

```fsharp
// handleInitialized — workspace/configuration block
let mutable wsUpdate = LspWorkspaceUpdate.Empty
try
    let! workspaceCSharpConfig = lspClient.WorkspaceConfiguration(...)
    ...
    wsUpdate <- wsUpdate.WithSettingsChange(newConfig)  // never reached
with ex ->
    logger.LogWarning(...)
// wsUpdate is still Empty regardless of what the client returned
```

The server does not crash, but configuration is silently dropped and dynamic
capability registration is skipped, leaving the server in an incomplete state.

### Bug 2 — Missing capability checks before sending the requests

Two of the three server→client requests are sent unconditionally, without first
checking whether the client has declared support for them.  Sending a request to a
client that never advertised support for it is a protocol violation and produces the
unnecessary error responses that trigger Bug 1.

| Request | Governing capability flag (LSP spec §3.6.0+) | Currently checked? |
|---|---|---|
| `client/registerCapability` | No single flag; only send if `getDynamicRegistrations` returns a non-empty list (each handler gates on per-feature `dynamicRegistration`) | ❌ — call is sent even when list is empty |
| `workspace/configuration` | `workspace.configuration?: boolean` → `ClientCapabilities.Workspace.Configuration: bool option` | ❌ — sent unconditionally |
| `window/workDoneProgress/create` | `window.workDoneProgress?: boolean` → `ClientCapabilities.Window.WorkDoneProgress: bool option` | ✅ — already gated in `ProgressReporter.Begin` |

Both missing checks are trivially expressible with the existing Ionide types.

## Affected Code

| File | Location | Detail |
|---|---|---|
| `Lsp/Client.fs` | `sendServerRequest`, `Result.Error` branch | `failwith "TODO"` — never implemented |
| `Lsp/Client.fs` | TODO comment | Misleading — implies capability gating belongs inside `CSharpLspClient` |
| `Handlers/LifeCycle.fs` | `handleInitialized` | Sends `client/registerCapability` and `workspace/configuration` unconditionally |
| `Lsp/ProgressReporter.fs` | `Begin` | Already gates `window/workDoneProgress/create` correctly |

## Design: Where Capability Checks Belong

`CSharpLspClient` carries a TODO comment:

```fsharp
// TODO: Send notifications / requests to client only if client support it
```

This implies `CSharpLspClient` itself should grow into a capability-aware gating
layer.  That would be the wrong design, for three reasons:

### Notifications never need gating

`WindowShowMessage`, `WindowLogMessage`, `TelemetryEvent`,
`TextDocumentPublishDiagnostics`, `LogTrace`, `Progress` — these are fire-and-forget
notifications.  The LSP spec does not require the client to advertise support before
the server sends them; clients simply ignore what they don't handle.

### `CSharpLspClient` does not know `ClientCapabilities`

`CSharpLspClient` is constructed in `configureRpcTransport` **before `initialize` is
processed**, so `ClientCapabilities` is not available at construction time.  Adding
a mutable capabilities field or a `unit -> ClientCapabilities` accessor would conflate
two concerns (transport and negotiation) in a single class.

### Some checks are inseparable from payload construction

`client/registerCapability` is the clearest example: the per-feature
`dynamicRegistration` flags determine *which* registrations to include in the
payload, not just *whether* to send.  The capability check and the payload
construction are the same operation.  Putting the gate inside
`CSharpLspClient.ClientRegisterCapability` would mean re-examining the payload to
decide whether to send it, which is backwards.

### The right rule

**The check belongs where `ClientCapabilities` is already naturally available at the
moment the call is made.**

| Request | Nature of check | Where it belongs |
|---|---|---|
| `client/registerCapability` | Inseparable from payload construction | Call site (`handleInitialized`) |
| `workspace/configuration` | Simple boolean: `Workspace.Configuration` | Call site (`handleInitialized`) |
| `window/workDoneProgress/create` | Simple boolean: `Window.WorkDoneProgress` | `ProgressReporter.Begin` ✅ already done |
| `workspace/workspaceFolders` | Simple boolean: `Workspace.WorkspaceFolders` | Call site (wherever it is called) |
| `workspace/applyEdit` | Simple boolean: `Workspace.ApplyEdit` | Call site (wherever it is called) |
| `workspace/semanticTokens/refresh` | Simple boolean: `Workspace.SemanticTokens.RefreshSupport` | Call site (wherever it is called) |
| `window/showMessageRequest` | Simple boolean: `Window.ShowMessage` | Call site (wherever it is called) |

For simple boolean cases that have no dedicated wrapper yet, the `ProgressReporter`
pattern is the right model: a small, purpose-built object constructed *after*
`initialize` that holds the relevant capability flag and gates the call.

### Conclusion

The TODO comment in `Client.fs` should be **removed and replaced** with a clear
statement that `CSharpLspClient` is a pure transport adapter and that capability
gating is the responsibility of callers.

## Proposed Fix

### Step 1 — Implement the `Result.Error` branch in `sendServerRequest`

Convert the raw `JToken` error payload returned by the JSON-RPC transport into a
typed `JsonRpc.Error` value and propagate it as `Result.Error`:

```fsharp
let sendServerRequest m (p: JToken) : AsyncLspResult<'TResult> = async {
    let! result = sendServerRequest_ m p

    return
        match result with
        | Result.Ok jtoken -> jtoken |> deserialize |> Result.Ok
        | Result.Error errToken ->
            let code    = errToken.SelectToken("code")    |> Option.ofObj |> Option.map _.Value<int>()    |> Option.defaultValue -32603
            let message = errToken.SelectToken("message") |> Option.ofObj |> Option.map _.Value<string>() |> Option.defaultValue "Unknown error"
            let data    = errToken.SelectToken("data")    |> Option.ofObj
            Result.Error { Code = code; Message = message; Data = data }
}
```

This is a minimal, safe change: the `JsonRpc.Error` record (`Code`, `Message`,
`Data`) is already imported via `Ionide.LanguageServerProtocol.JsonRpc`, so no new
dependencies are needed.

### Step 2 — Guard `client/registerCapability` on a non-empty registrations list

In `handleInitialized`, skip the `ClientRegisterCapability` call entirely when
`getDynamicRegistrations` produces an empty list:

```fsharp
let registrations =
    getDynamicRegistrations context.Config context.ClientCapabilities

if registrations.IsEmpty then
    logger.LogDebug("handleInitialized: no dynamic registrations, skipping client/registerCapability")
else
    let registrationParams = { Registrations = registrations |> List.toArray }
    try
        match! lspClient.ClientRegisterCapability registrationParams with
        | Ok _ -> ()
        | Error error ->
            logger.LogWarning("handleInitialized: dynamic cap registration has failed with {error}", error)
    with ex ->
        logger.LogWarning("handleInitialized: dynamic cap registration has failed with {error}", string ex)
```

### Step 3 — Guard `workspace/configuration` on `ClientCapabilities.Workspace.Configuration`

In `handleInitialized`, skip the `WorkspaceConfiguration` call when the client has
not declared support for it:

```fsharp
let configurationSupported =
    context.ClientCapabilities.Workspace
    |> Option.bind _.Configuration
    |> Option.defaultValue false

if not configurationSupported then
    logger.LogDebug("handleInitialized: client does not support workspace/configuration, skipping")
else
    try
        let! workspaceCSharpConfig = lspClient.WorkspaceConfiguration(...)
        ...
    with ex ->
        logger.LogWarning(...)
```

### Step 4 — Replace the TODO comment in `Client.fs`

Remove the misleading TODO and replace it with a comment clarifying the design:

```fsharp
// Note: CSharpLspClient is a pure transport adapter. It does not gate calls on
// ClientCapabilities — that is the responsibility of callers, who have access to
// capabilities via RequestContext. See ProgressReporter for the reference pattern.
```

### Step 5 — Verify remaining call sites handle `Error` correctly

After Step 1, each of the three call sites receives a proper `LspResult` instead of
an exception.  All three are already wrapped in `try/with` blocks:

1. **`client/registerCapability`** — `Ok _` is matched and discarded; `Error` is
   already matched and logged as a warning.  ✓ No change needed beyond Steps 1–2.

2. **`workspace/configuration`** — only the `Ok` path assigns `wsUpdate`.  If the
   client errors (e.g. not supported despite advertising it), we skip the settings
   merge and carry on with defaults.  ✓ No change needed beyond Steps 1 and 3.

3. **`window/workDoneProgress/create`** (in `ProgressReporter.Begin`) — already
   gated on `WorkDoneProgress` capability; the `Error _` branch already sets
   `canReport <- false`.  ✓ No change needed.

### Step 6 — Add tests

Add a unit or integration test that verifies `handleInitialized` completes
successfully when the client either does not advertise support for the two optional
requests, or returns `-32601 Method not found` despite advertising support.

**The existing test suite already runs with no `dynamicRegistration` flags set.**
`defaultClientCapabilities` in `Tooling.fs` leaves every `DynamicRegistration` field
as `None` and `Workspace.Configuration = None`.  This means all existing tests
exercise the static-capability path only.  Adding an explicit test for the
capability-check guards in Steps 2 and 3 is therefore straightforward: use
`emptyClientCapabilities` (already defined in `Tooling.fs`) as the
`ClientCapabilities` in a new `LspClientProfile`, run `LoadSolution`, and assert:

- `initialize` and `initialized` complete without the server process exiting.
- No `client/registerCapability` or `workspace/configuration` entry appears in the
  `ClientRpcCall` log (both methods are already logged by `configureRpcTransport` in
  `Tooling.fs`, so absence is detectable).
- A subsequent request (e.g. `textDocument/hover`) returns a valid response,
  confirming the server reached a working state with default config.

A complementary unit test using an in-process `CSharpLspClient` with a mock
`sendServerRequest_` that returns `Error` payloads can verify that after Step 1,
`sendServerRequest` returns `Result.Error { Code = -32601; ... }` instead of
throwing.

### Note: per-handler `registration` unit tests are not needed

The integration test above (all flags absent → `client/registerCapability` not sent)
provides aggregate coverage: if any handler's `registration` function ignores its
flag and returns `Some`, the registrations list would be non-empty and the test would
fail.  Per-handler unit tests for each `registration` function are not warranted —
the pattern is mechanical and uniform across all ~30 handlers.

There is however a **pre-existing gap** not addressed by this plan: no test runs with
any `dynamicRegistration = true` flag, so the `Some { Id = ...; Method = ...;
RegisterOptions = ... }` branches of all handler `registration` functions are
currently unexercised.  Fixing that gap (e.g. a `dynamicClientCapabilities` profile
that enables all flags and verifies the `Registrations` array content in
`client/registerCapability`) is out of scope here but worth tracking separately.

## Testing Checklist

- [ ] `sendServerRequest` returns `Result.Error` (not throws) when the transport
      returns an error JToken
- [ ] `client/registerCapability` is not sent when all per-feature
      `dynamicRegistration` flags are absent/false (empty registrations list)
- [ ] `workspace/configuration` is not sent when
      `ClientCapabilities.Workspace.Configuration` is absent/false
- [ ] `handleInitialized` completes and returns a non-empty `LspWorkspaceUpdate`
      when all three outbound calls receive error responses (robustness fallback)
- [ ] Server reaches a working state (can serve hover/completion) after an
      initialization sequence where the client does not advertise support for any of
      the three requests
- [ ] TODO comment in `Client.fs` replaced with transport-adapter clarification
- [ ] Existing tests still pass

## Out of Scope

- The missing handlers in Claude Code's LSP client are a bug in Claude Code, not in
  `csharp-ls`.  This plan only makes `csharp-ls` degrade gracefully.
- Capability gating for server→client requests that are not yet called at runtime
  (`workspace/workspaceFolders`, `workspace/applyEdit`,
  `workspace/semanticTokens/refresh`, `window/showMessageRequest`) — these should
  follow the same call-site gating pattern when they are wired up.
- Testing the positive `dynamicRegistration = true` path for each handler's
  `registration` function — a separate, pre-existing gap.
