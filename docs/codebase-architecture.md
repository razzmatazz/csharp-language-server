# Codebase Architecture — csharp-language-server

> This document provides a comprehensive overview of the project structure, LSP message
> handling, test infrastructure, and handler registration patterns. Use it as a reference
> when planning new feature implementations.

---

## 1. Overall Project Structure

```
csharp-language-server/
├── csharp-language-server.sln
├── Directory.Build.props            # Shared build properties
├── Directory.Build.targets          # Shared build targets
├── Directory.Packages.props         # Central package version management (all NuGet versions here)
├── global.json
├── nuget.config
├── docs/
│
├── src/CSharpLanguageServer/        # ── Main server project ──
│   ├── CSharpLanguageServer.fsproj  # Target: net10.0, OutputType: Exe, PackAsTool: csharp-ls
│   ├── Program.fs                   # [<EntryPoint>] — CLI parsing, server bootstrap
│   ├── Logging.fs                   # Logging setup + LspTraceLoggerProvider ($/logTrace bridge)
│   ├── Util.fs                      # General utilities
│   ├── Types.fs                     # ServerSettings, ICSharpLspServer, document filters
│   ├── DocumentationUtil.fs         # XML-doc / symbol documentation helpers
│   ├── Diagnostics.fs               # `--diagnose` command implementation
│   │
│   ├── Lsp/                         # ── LSP protocol layer ──
│   │   ├── Client.fs                # CSharpLspClient (server→client messages)
│   │   ├── ProgressReporter.fs      # $/progress token management
│   │   ├── Server.fs                # **Central wiring**: capabilities, registrations, handler maps
│   │   ├── Workspace.fs             # Workspace-level change handlers
│   │   └── WorkspaceFolder.fs       # Multi-root workspace folder management
│   │
│   ├── Runtime/                     # ── JSON-RPC transport & scheduling ──
│   │   ├── JsonRpcServer.fs         # Custom JSON-RPC 2.0 over stdin/stdout (MailboxProcessor)
│   │   ├── Request.fs               # ServerRequestContext, ServerRequestMode
│   │   ├── RequestScheduling.fs     # Concurrent request queue (ReadOnly/ReadWrite/ReadOnlyBg)
│   │   ├── ServerEvent.fs           # ServerEvent discriminated union
│   │   └── ServerStateLoop.fs       # Main state machine (MailboxProcessor<ServerEvent>)
│   │
│   ├── Roslyn/                      # ── Roslyn/Microsoft.CodeAnalysis integration ──
│   │   ├── Conversions.fs           # LSP ↔ Roslyn type conversions
│   │   ├── Document.fs              # Roslyn Document helpers
│   │   ├── Solution.fs              # Solution loading/management
│   │   ├── Symbol.fs                # Symbol resolution
│   │   └── WorkspaceServices.fs     # Custom workspace services
│   │
│   └── Handlers/                    # ── LSP method handlers (one per feature, 35 files) ──
│       ├── LifeCycle.fs              # initialize, initialized, shutdown
│       └── (...)
│
└── tests/CSharpLanguageServer.Tests/  # ── Test project ──
    ├── CSharpLanguageServer.Tests.fsproj  # NUnit, net10.0, references main project
    ├── Tooling.fs                         # Test harness (~1060 lines)
    ├── AssemblyInfo.fs                    # [<Parallelizable(ParallelScope.All)>]
    ├── Fixtures/                          # Sample C# projects for integration tests
    │   ├── genericProject/
    │   ├── aspnetProject/
    │   ├── multiFolderWorkspace/
    │   └── ...
    └── *Tests.fs                          # 24 test files (one per feature area)
```

---

## 2. Key Dependencies

| Category | Package | Version | Purpose |
|----------|---------|---------|---------|
| **LSP** | **Ionide.LanguageServerProtocol** | 0.7.0 | LSP protocol types & base `LspClient` class |
| **JSON** | Newtonsoft.Json | 13.0.4 | JSON serialization (used by Ionide) |
| **Roslyn** | Microsoft.CodeAnalysis | 5.3.0 | C# compiler & workspace APIs |
| **Roslyn** | Microsoft.CodeAnalysis.CSharp.Features | 5.3.0 | Code actions, completion, etc. |
| **Roslyn** | Microsoft.CodeAnalysis.Workspaces.MSBuild | 5.3.0 | .sln/.csproj loading |
| **MSBuild** | Microsoft.Build.Locator | 1.10.12 | Finds MSBuild at runtime |
| **CLI** | Argu | 6.2.5 | CLI argument parsing |
| **Decompiler** | ICSharpCode.Decompiler | 9.1.0.7988 | Go-to-definition into metadata |
| **Logging** | Microsoft.Extensions.Logging.Console | 9.0.9 | Console logger |
| **Async** | FSharp.Control.AsyncSeq | 3.2.1 | Async sequences |

> **Note:** The project does **not** use `StreamJsonRpc` or `OmniSharp`. It implements its
> own JSON-RPC 2.0 transport from scratch.

---

## 3. LSP Message Handling Architecture

### 3.1 Server Startup Flow

```
Program.fs  →  Server.start (Lsp/Server.fs)
                │
                ├─ Creates ServerStateLoop (MailboxProcessor<ServerEvent>)
                ├─ Creates CSharpLspClient (Lsp/Client.fs)
                ├─ Calls configureRpcServer() to build handler maps
                └─ Starts JsonRpcServer (Runtime/JsonRpcServer.fs)
                     └─ Reads/writes JSON-RPC over stdin/stdout
```

### 3.2 JSON-RPC Transport (`Runtime/JsonRpcServer.fs`)

A hand-rolled JSON-RPC 2.0 transport implemented as an **F# `MailboxProcessor<JsonRpcServerEvent>`**:

- **Inbound:** Reads `Content-Length`-framed messages from stdin, deserializes JSON, dispatches by looking up `method` in the `callHandlers` or `notificationHandlers` maps.
- **Outbound:** Writes `Content-Length`-framed JSON to stdout. Two-priority write queue: `NormalPriority` and `LowPriority`.
- **Cancellation:** Handles `$/cancelRequest` natively via `CancellationTokenSource` keyed by request ID.
- **Server→Client calls:** Manages `PendingOutboundCalls` keyed by request ID for round-trip server→client requests.

### 3.3 Handler Registration (`Lsp/Server.fs` — the central wiring file)

The `configureRpcServer` function (lines ~102–175) builds two maps:

#### Call Handlers (`Map<string, JsonRpcCallHandler>`)

| Method | Handler | Mode |
|--------|---------|------|
| `initialize` | `LifeCycle.handleInitialize` | ReadWrite |
| `textDocument/definition` | `Definition.handle` | ReadOnly |
| `textDocument/hover` | `Hover.handle` | ReadOnly |
| `textDocument/completion` | `Completion.handle` | ReadOnly |
| `textDocument/references` | `References.handle` | ReadOnly |
| `textDocument/codeAction` | `CodeAction.handle` | ReadOnly |
| `codeAction/resolve` | `CodeAction.resolve` | ReadOnly |
| `textDocument/rename` | `Rename.handle` | ReadWrite |
| `textDocument/prepareRename` | `Rename.prepare` | ReadOnly |
| `textDocument/diagnostic` | `Diagnostic.handle` | ReadOnlyBackground |
| `workspace/diagnostic` | `Diagnostic.handleWorkspaceDiagnostic` | ReadOnlyBackground |
| `textDocument/documentSymbol` | `DocumentSymbol.handle` | ReadOnly |
| `textDocument/documentHighlight` | `DocumentHighlight.handle` | ReadOnly |
| `workspace/symbol` | `WorkspaceSymbol.handle` | ReadOnly |
| `textDocument/implementation` | `Implementation.handle` | ReadOnly |
| `textDocument/formatting` | `DocumentFormatting.handle` | ReadOnly |
| `textDocument/rangeFormatting` | `DocumentRangeFormatting.handle` | ReadOnly |
| `textDocument/onTypeFormatting` | `DocumentOnTypeFormatting.handle` | ReadOnly |
| `textDocument/typeDefinition` | `TypeDefinition.handle` | ReadOnly |
| `textDocument/signatureHelp` | `SignatureHelp.handle` | ReadOnly |
| `textDocument/semanticTokens/full` | `SemanticTokens.handleFull` | ReadOnly |
| `textDocument/semanticTokens/full/delta` | `SemanticTokens.handleFullDelta` | ReadOnly |
| `textDocument/semanticTokens/range` | `SemanticTokens.handleRange` | ReadOnly |
| `textDocument/prepareCallHierarchy` | `CallHierarchy.prepare` | ReadOnly |
| `callHierarchy/incomingCalls` | `CallHierarchy.incomingCalls` | ReadOnly |
| `callHierarchy/outgoingCalls` | `CallHierarchy.outgoingCalls` | ReadOnly |
| `textDocument/prepareTypeHierarchy` | `TypeHierarchy.prepare` | ReadOnly |
| `typeHierarchy/supertypes` | `TypeHierarchy.supertypes` | ReadOnly |
| `typeHierarchy/subtypes` | `TypeHierarchy.subtypes` | ReadOnly |
| `textDocument/codeLens` | `CodeLens.handle` | ReadOnly |
| `codeLens/resolve` | `CodeLens.resolve` | ReadOnly |
| `textDocument/inlayHint` | `InlayHint.handle` | ReadOnly |
| `csharp/metadata` | `CSharpMetadata.handle` | ReadOnly |
| `shutdown` | `LifeCycle.handleShutdown` | ReadWrite |

#### Notification Handlers (`Map<string, JsonRpcNotificationHandler>`)

| Method | Handler | Mode |
|--------|---------|------|
| `initialized` | `LifeCycle.handleInitialized` | ReadWrite |
| `textDocument/didOpen` | `TextDocumentSync.didOpen` | ReadWrite |
| `textDocument/didChange` | `TextDocumentSync.didChange` | ReadWrite |
| `textDocument/didClose` | `TextDocumentSync.didClose` | ReadWrite |
| `textDocument/willSave` | `TextDocumentSync.willSave` | ReadOnly |
| `textDocument/willSaveWaitUntil` | `TextDocumentSync.willSaveWaitUntil` | ReadOnly |
| `textDocument/didSave` | `TextDocumentSync.didSave` | ReadWrite |
| `workspace/didChangeWatchedFiles` | `Workspace.didChangeWatchedFiles` | ReadWrite |
| `workspace/didChangeWorkspaceFolders` | `Workspace.didChangeWorkspaceFolders` | ReadWrite |
| `workspace/didChangeConfiguration` | `Workspace.didChangeConfiguration` | ReadOnly |
| `$/setTrace` | `Trace.handleSetTrace` | ReadWrite |

### 3.4 Handler Wrapping

The `wrapHandler` function in `Lsp/Server.fs` bridges raw JSON-RPC ↔ typed handlers:

1. Posts `EnterRequestContext` to the state actor → receives a `ServerRequestContext`
2. Deserializes `JToken` params to the handler's typed parameter type
3. Calls the handler
4. On completion, posts `LeaveRequestContext` with buffered events

### 3.5 Request Scheduling (`Runtime/RequestScheduling.fs`)

Sophisticated concurrent request queue:

- **`ReadOnly`** requests run concurrently with other ReadOnly requests
- **`ReadWrite`** requests run serially, blocking until all prior requests retire
- **`ReadOnlyBackground`** requests (e.g. diagnostics) never block other requests
- **Draining mode** — used before workspace reloads; only requests up to a certain ordinal activate, then signals `Drained`
- **Event buffering** — handlers emit `ServerEvent`s into a `ServerRequestContext` buffer; events replay into the state loop only when the request retires in ordinal order (preserving serial mutation semantics)

### 3.6 Server State Loop (`Runtime/ServerStateLoop.fs`)

A `MailboxProcessor<ServerEvent>` maintaining `ServerState` (settings, workspace, client
capabilities, trace level, request queue, diagnostic backlog). Processes events like:

- `ClientInitialize`
- `TraceLevelChange`
- `WorkspaceReloadRequested`
- `DocumentOpened` / `DocumentChanged` / `DocumentClosed`
- `PushDiagnosticsProcessPendingDocuments`
- etc.

---

## 4. Handler Module Pattern

Every handler in `Handlers/` follows a consistent pattern with these exports:

```fsharp
module CSharpLanguageServer.Handlers.SomeFeature

// Static capability declaration (plugged into ServerCapabilities)
let provider (clientCapabilities: ClientCapabilities option) : SomeOption option =
    Some { ... }

// Dynamic registration (sent via client/registerCapability after initialized)
let registration (clientCapabilities: ClientCapabilities option) : Registration option =
    Some { Id = "...; Method = "..."; RegisterOptions = Some ... }

// The actual request handler
let handle (context: ServerRequestContext) (p: SomeParams) : AsyncLspResult<SomeResult option> =
    async {
        // Use context.Workspace, context.ClientCapabilities, etc.
        // Return LspResult.success ...
    }

// Optional: resolve, prepare, etc. for multi-step protocols
let resolve (context: ServerRequestContext) (p: ResolveParams) : AsyncLspResult<ResolvedResult> = ...
```

**Key types:**
- `ServerRequestContext` — provides access to workspace, client capabilities, client proxy, settings, trace level, and an event buffer for emitting side effects
- `AsyncLspResult<'T>` — `Async<LspResult<'T>>` where `LspResult` carries either `Ok` or a JSON-RPC error

---

## 5. Client Communication (`Lsp/Client.fs`)

`CSharpLspClient` extends Ionide's `LspClient()` base class:

- Delegates outbound **notifications** to `sendJsonRpcNotification` on the RPC server mailbox
- Delegates outbound **requests** (server→client) to `sendJsonRpcCall` on the RPC server mailbox
- Used for: `textDocument/publishDiagnostics`, `window/logMessage`, `window/showMessage`, `$/progress`, `$/logTrace`, `client/registerCapability`, etc.

### 5.1 LSP Trace Logging Bridge (`Logging.fs`)

The server bridges `Microsoft.Extensions.Logging` output to LSP `$/logTrace` notifications
via a custom `LspTraceLoggerProvider` / `LspTraceLogger` registered alongside the console
logger. This means all existing `logger.LogInformation(...)`, `logger.LogDebug(...)` etc.
calls throughout the codebase are automatically forwarded to the client when tracing is
enabled — no per-call-site changes needed.

**Lifecycle wiring** (in `ServerStateLoop.processServerEvent`):
- `ServerStarted` → `Logging.setLspTraceClient(Some lspClient)`
- `TraceLevelChange` → `Logging.setLspTraceLevel(newLevel)`
- `ClientShutdown` → `Logging.setLspTraceClient(None)`

**LogLevel → TraceValues mapping:**
- `TraceValues.Off` → nothing sent
- `TraceValues.Messages` → `LogLevel.Information` and above
- `TraceValues.Verbose` → `LogLevel.Trace` and above; `verbose` field populated with
  category name, log level, and exception info

**Safety:** A `[<ThreadStatic>]` reentrant guard prevents infinite loops when the JSON-RPC
transport layer itself logs while sending a `$/logTrace` notification. Messages before the
client connects (or after shutdown) are silently dropped — the console logger to stderr
still captures them.

---

## 6. Capabilities Declaration (`Lsp/Server.fs`)

### Static Capabilities (`getServerCapabilities`)

Builds a `ServerCapabilities` record by calling each handler module's `provider` function:

```fsharp
let getServerCapabilities (clientCapabilities: ClientCapabilities option) =
    { ServerCapabilities.Default with
        HoverProvider = Hover.provider clientCapabilities
        DefinitionProvider = Definition.provider clientCapabilities
        CompletionProvider = Completion.provider clientCapabilities
        // ... all handler providers ...
    }
```

### Dynamic Registrations (`getDynamicRegistrations`)

Collects `Registration` values from every handler's `registration` function. These are sent
to the client during `handleInitialized` via `client/registerCapability`.

---

## 7. Test Infrastructure

### 7.1 Test Framework

- **NUnit** with `[<Parallelizable(ParallelScope.All)>]` — all tests run in parallel
- **net10.0** target, references main server project directly
- `Tooling.fs` (~1060 lines) is the test harness, compiled before all test files

### 7.2 Integration Test Architecture: Out-of-Process via stdio

Tests do **not** use in-process hosting. Instead:

1. **Spawns a real server process** — `makeServerProcessInfo` locates the built
   `CSharpLanguageServer` executable relative to the test assembly, creates a
   `ProcessStartInfo` with `RedirectStandardInput/Output/Error = true`

2. **Communicates via stdin/stdout using raw JSON-RPC** — A `MailboxProcessor<ClientEvent>`
   (the "client event loop") writes JSON-RPC to `serverProcess.StandardInput` and reads
   from `serverProcess.StandardOutput.BaseStream`, parsing `Content-Length` headers

3. **Concurrency control** — `activeClientsSemaphore` (`SemaphoreSlim` initialized to
   `Environment.ProcessorCount`) throttles simultaneous server processes

### 7.3 Key Test Classes

#### `ClientController` (primary test API)

```fsharp
use client = new ClientController(fixtureName, settings)
client.LoadSolution()   // copies fixture → temp dir, starts server, initialize + initialized,
                        // waits for "Finished loading workspace" progress event

let result: HoverResult = client.Request("textDocument/hover", hoverParams)
let state = client.GetState()  // ServerCapabilities, PushDiagnostics, etc.

client.ServerDidRespondTo("textDocument/hover")     // RPC log assertion
client.ServerMessageLogContains(fun m -> ...)       // message log assertion
client.WaitForProgressEnd(fun p -> ...)             // poll for $/progress end (20s timeout)
```

#### `FileController` (single-document management)

```fsharp
use file = client.Open "Project/Class.cs"    // sends textDocument/didOpen
file.Edit(range, newText)                     // sends textDocument/didChange
file.Save()                                   // sends textDocument/didSave
// Dispose sends textDocument/didClose
```

### 7.4 Fixture Preparation

`prepareTempTestDirFrom` copies fixture project files (`.cs`, `.csproj`, `.sln`, `.slnx`,
`.cshtml`, `.editorconfig`, `global.json`, `.txt`) to a temp directory, filtering out
`bin`/`obj`. On macOS, prepends `/private` to the temp path.

### 7.5 Typical Test Pattern

```fsharp
[<Test>]
let testSomething () =
    use client = activateFixture "genericProject"
    use classFile = client.Open "Project/Class.cs"

    let result: SomeType =
        client.Request("textDocument/someMethod", someParams)

    Assert.AreEqual(expected, result)
    // Dispose: didClose → shutdown → kill process → delete temp dir
```

### 7.6 Other Test Categories

| Category | File | How it works |
|----------|------|-------------|
| JSON-RPC transport unit tests | `JsonRpcServerTests.fs` | Calls `startJsonRpcServer` with in-memory streams (no process) |
| Progress reporter unit tests | `ProgressReporterTests.fs` | Uses a `TrackingLspClientStub` mock |
| CLI diagnose command tests | `DiagnoseCommandTests.fs` | Spawns process with `--diagnose` flag |

---

## 8. Adding a New LSP Feature — Checklist

1. **Create handler file** `Handlers/NewFeature.fs` following the handler module pattern
   (export `provider`, `registration`, `handle`)
2. **Add to `.fsproj`** — insert `<Compile Include="Handlers/NewFeature.fs" />` in the
   Handlers section (order matters in F#)
3. **Register in `Lsp/Server.fs`**:
   - Add to `getServerCapabilities` (static capability)
   - Add to `getDynamicRegistrations` (if using dynamic registration)
   - Add to `configureRpcServer` in the appropriate handler map (`callHandlers` or
     `notificationHandlers`) with the correct `ServerRequestMode`
4. **Write tests** — create `tests/.../NewFeatureTests.fs`, add a fixture under
   `Fixtures/` if needed, use `ClientController` + `FileController` pattern
5. **Add test file to test `.fsproj`**
