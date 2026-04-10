# Fix: server→client request errors crash `handleInitialized` (issue #16360)

## Background

[GitHub issue #16360](https://github.com/anthropics/claude-code/issues/16360) reports that
`csharp-ls` does not work with the Claude Code built-in LSP client.  The root cause
described there is that Claude Code's LSP client does not handle the three server→client
requests that `csharp-ls` sends during initialization:

- `client/registerCapability`
- `workspace/configuration`
- `window/workDoneProgress/create`

When a client does not implement those requests it returns a JSON-RPC error response
(typically `-32601 Method not found`).  That error is what triggers the bug on the
`csharp-ls` side.

## Root Cause

`Lsp/Client.fs` contains `sendServerRequest`, the shared helper that wraps all
outbound server→client requests:

```fsharp
let sendServerRequest m (p: JToken) : AsyncLspResult<'TResult> = async {
    let! result = sendServerRequest_ m p

    return
        match result with
        | Result.Ok jtoken -> jtoken |> deserialize |> Result.Ok
        | Result.Error err -> failwith "TODO"     // ← the bug
}
```

The `Result.Error` branch has never been implemented — it unconditionally throws.
When a client returns an error (e.g. `-32601`) to any of the three outbound calls in
`handleInitialized`, this exception unwinds the entire `handleInitialized` async
workflow before the `LspWorkspaceUpdate` carrying `WithSettingsChange` is ever
returned, leaving the server in an incomplete state.

Because `handleInitialized` wraps each call in its own `try/with`, the thrown
exception is caught and logged as a warning — but the partial `wsUpdate` constructed
up to that point is still lost in the `workspace/configuration` branch:

```fsharp
// handleInitialized — workspace/configuration block
let mutable wsUpdate = LspWorkspaceUpdate.Empty
try
    let! workspaceCSharpConfig = lspClient.WorkspaceConfiguration(...)
    ...
    wsUpdate <- wsUpdate.WithSettingsChange(newConfig)  // never reached
with ex ->
    logger.LogWarning(...)
// wsUpdate == Empty regardless of what the client returned
```

The exception from `failwith "TODO"` is caught by the `try/with` around each call
site in `handleInitialized`, so the server does not crash — but configuration is
silently dropped and dynamic capability registration is skipped.

The same bug would affect `window/workDoneProgress/create` inside
`ProgressReporter.Begin` on any client that does not support progress tokens, though
`ProgressReporter` already gates the call on `clientCapabilities.Window.WorkDoneProgress`.

## Affected Code

| File | Location | Detail |
|---|---|---|
| `Lsp/Client.fs` | `sendServerRequest`, `Result.Error` branch | `failwith "TODO"` — never implemented |
| `Handlers/LifeCycle.fs` | `handleInitialized` | Depends on `sendServerRequest` returning a proper `LspResult` |
| `Lsp/ProgressReporter.fs` | `Begin` | Depends on `sendServerRequest` returning a proper `LspResult` |

## Capability Checks — What's Missing

The `sendServerRequest` crash is the primary bug, but there is a related correctness
issue: `csharp-ls` sends two of the three server→client requests unconditionally,
without first checking whether the client has declared support for them.  Sending a
request to a client that never advertised support for it is a protocol violation that
produces unnecessary round-trips and the errors that trigger the `failwith "TODO"` in
the first place.

| Request | Governing capability flag | Currently checked? |
|---|---|---|
| `client/registerCapability` | No single flag; only send if `getDynamicRegistrations` returns a non-empty list (each handler already gates on per-feature `dynamicRegistration`) | ❌ — call is sent even when list is empty |
| `workspace/configuration` | `ClientCapabilities.Workspace.Configuration: bool option` | ❌ — sent unconditionally |
| `window/workDoneProgress/create` | `ClientCapabilities.Window.WorkDoneProgress: bool option` | ✅ — already gated in `ProgressReporter.Begin` |

Both missing checks are trivially expressible with the existing Ionide types.

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
`getDynamicRegistrations` produces an empty list.  A client that sets
`dynamicRegistration = false` for every feature will cause every handler's
`registration` function to return `None`, resulting in an empty list — there is
nothing to register and the call should not be made.

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
not declared support for it.  The governing flag is
`ClientCapabilities.Workspace.Configuration: bool option`:

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

### Step 4 — Verify remaining call sites handle `Error` correctly

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

### Step 5 — Add a test

Add a unit or integration test that verifies `handleInitialized` completes
successfully when the client either does not advertise support for the two optional
requests, or returns `-32601 Method not found` despite advertising support.

The existing test harness (`Tooling.fs`) already registers handlers for all three
server→client methods.  The cleanest approach for a capability-check regression test
is to add a `clientProfile` variant (or use the existing `initializeParamsUpdate`
callback) that clears `Workspace.Configuration` and all `DynamicRegistration` flags
from the `ClientCapabilities` sent in `initialize`, then assert:

- `initialize` and `initialized` complete without the server process exiting.
- No `client/registerCapability` or `workspace/configuration` request appears in the
  RPC log.
- A subsequent request (e.g. `textDocument/hover`) returns a valid response,
  confirming the server reached a working state with default config.

A complementary unit test using an in-process `CSharpLspClient` with a mock
`sendServerRequest_` that returns `Error` payloads can verify that after Step 1,
`sendServerRequest` returns `Result.Error { Code = -32601; ... }` instead of
throwing.

## Testing Checklist

- [ ] `sendServerRequest` returns `Result.Error` (not throws) when the transport
      returns an error JToken
- [ ] `client/registerCapability` is not sent when all per-feature `dynamicRegistration`
      flags are absent/false (empty registrations list)
- [ ] `workspace/configuration` is not sent when `ClientCapabilities.Workspace.Configuration`
      is absent/false
- [ ] `handleInitialized` completes and returns a non-empty `LspWorkspaceUpdate`
      when all three outbound calls receive error responses (robustness fallback)
- [ ] Server reaches a working state (can serve hover/completion) after an
      initialization sequence where the client does not advertise support for any of
      the three requests
- [ ] Existing tests still pass

## Out of Scope

The actual missing handlers in Claude Code's LSP client (`workspace/configuration`,
`client/registerCapability`, `window/workDoneProgress/create`) are a bug in Claude
Code, not in `csharp-ls`.  This plan only addresses the server-side robustness issue
so that `csharp-ls` degrades gracefully when those responses are missing rather than
entering a partially-initialised state.
