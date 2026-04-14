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
│   ├── Types.fs                     # CSharpConfiguration, ICSharpLspServer, document filters
│   ├── DocumentationUtil.fs         # XML-doc / symbol documentation helpers
│   ├── DiagnoseCommand.fs           # `--diagnose` command implementation
│   │
│   ├── Lsp/                         # ── LSP protocol layer ──
│   │   ├── Client.fs                # CSharpLspClient (server→client messages)
│   │   ├── ProgressReporter.fs      # $/progress token management
│   │   ├── Server.fs                # **Central wiring**: capabilities, registrations, handler maps
│   │   ├── Workspace.fs             # Workspace-level change handlers
│   │   └── WorkspaceFolder.fs       # Multi-root workspace folder management
│   │
│   ├── Runtime/                     # ── JSON-RPC transport & scheduling ──
│   │   ├── JsonRpc.fs               # Custom JSON-RPC 2.0 over stdin/stdout (MailboxProcessor)
│   │   ├── RequestScheduling.fs     # RequestContext, RequestMode, RequestQueue + concurrent request scheduling (ReadOnly/ReadWrite/ReadOnlyBg)
│   │   ├── PushDiagnostics.fs       # Push-diagnostics state machine (PushDiagnosticsState + handlers)
│   │   └── ServerStateLoop.fs       # Main state machine + ServerEvent DU (MailboxProcessor<ServerEvent>)
│   │
│   ├── Roslyn/                      # ── Roslyn/Microsoft.CodeAnalysis integration ──
│   │   ├── Conversions.fs           # LSP ↔ Roslyn type conversions
│   │   ├── Document.fs              # Roslyn Document helpers
│   │   ├── Solution.fs              # Solution loading/management
│   │   ├── Analyzers.fs             # Shared helpers: compiler + analyzer diagnostics via CompilationWithAnalyzers
│   │   ├── Symbol.fs                # Symbol resolution
│   │   └── WorkspaceServices.fs     # Custom workspace services
│   │
│   └── Handlers/                    # ── LSP method handlers (one per feature, 35 files) ──
│       ├── LifeCycle.fs             # initialize, initialized, shutdown
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
    └── *Tests.fs                          # test files (one per feature area)
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
Program.fs  →  Server.startCore (Lsp/Server.fs)
                │
                ├─ Creates ServerStateLoop (MailboxProcessor<ServerEvent>)
                ├─ Calls startJsonRpcTransport with configureRpcTransport callback
                │    └─ configureRpcTransport creates CSharpLspClient + builds handler maps
                └─ JsonRpcTransport reads/writes JSON-RPC over stdin/stdout
```

### 3.2 JSON-RPC Transport (`Runtime/JsonRpc.fs`)

See [jsonrpc.md](jsonrpc.md) for full API documentation.

A hand-rolled JSON-RPC 2.0 transport implemented as an **F# `MailboxProcessor<JsonRpcTransportEvent>`**:

- **Inbound:** Reads `Content-Length`-framed messages from stdin, deserializes JSON, dispatches by looking up `method` in the `callHandlers` or `notificationHandlers` maps.
- **Outbound:** Writes `Content-Length`-framed JSON to stdout.
- **Cancellation:** Handles `$/cancelRequest` natively via `CancellationTokenSource` keyed by request ID.
- **Server→Client calls:** Manages `PendingOutboundCalls` keyed by request ID for round-trip server→client requests.

### 3.3 Handler Registration (`Lsp/Server.fs` — the central wiring file)

The `configureRpcTransport` function takes the `stateActor` and `rpcTransport` mailbox
processors and returns a tuple of two maps: `JsonRpcCallHandlerMap` (request/response) and
`JsonRpcNotificationHandlerMap` (fire-and-forget). Each handler is mapped to its LSP method
name and assigned a `RequestMode` (`ReadOnly`, `ReadWrite`, or `ReadOnlyBackground`) that
controls concurrent scheduling.

### 3.4 Handler Wrapping

The `wrapHandler` function (nested inside `configureRpcTransport`) bridges raw JSON-RPC ↔ typed handlers:

1. Posts `EnterRequestContext` to the state actor → receives a `RequestContext`
2. Deserializes `JToken` params to the handler's typed parameter type
3. Calls the handler, which returns `(LspResult<'T>, LspWorkspaceUpdate)`
4. In the `finally` block, posts `LeaveRequestContext` with the `LspWorkspaceUpdate`

### 3.5 Request Scheduling (`Runtime/RequestScheduling.fs`)

Also defines `RequestContext` (a class), `RequestMode`, `RequestInfo`, `RequestQueue`, and
related types.

Sophisticated concurrent request queue:

- **`ReadOnly`** requests run concurrently with other ReadOnly requests
- **`ReadWrite`** requests run serially, blocking until all prior requests retire
- **`ReadOnlyBackground`** requests (e.g. diagnostics) never block other requests
- **Draining mode** — used before workspace reloads; only requests up to a certain ordinal activate, then signals `Drained`
- **Workspace updates** — handlers return an `LspWorkspaceUpdate` alongside the result; this is stored in the `RequestInfo` and applied to the state loop when the request retires in ordinal order (preserving serial mutation semantics)

`processRequestQueue` is the single entry point for advancing the queue each tick. Retirement is checked first on every
call: a finished request is always replayed before any new request is activated.

### 3.6 Server State Loop (`Runtime/ServerStateLoop.fs`)

Also defines the `ServerEvent` discriminated union.

A `MailboxProcessor<ServerEvent>` maintaining `ServerState` (config, workspace, client
capabilities, trace level, request queue, push-diagnostics state, shutdown flag, and
solution-ready awaiters). Processes events including:

- `ServerStarted` / `ClientInitialize` / `ClientShutdown`
- `ClientCapabilityChange`
- `TraceLevelChange` / `SettingsChange`
- `WorkspaceReloadRequested`
- `WorkspaceFolderSolutionChange` / `WorkspaceFolderUpdates` / `WorkspaceConfigurationChanged`
- `EnterRequestContext` / `LeaveRequestContext` / `ProcessRequestQueue` / `RequestQueueDrained`
- `ApplyWorkspaceUpdate`
- `PushDiagnosticsBacklogUpdate` / `PushDiagnosticsProcessPendingDocuments` / `PushDiagnosticsDocumentDiagnosticsResolution`
- `GetWorkspaceFolder` / `GetWorkspaceFolderUriList` / `ProcessSolutionAwaiters`
- `PeriodicTimerTick`

**`ServerState` record fields:** `Config`, `LspClient`, `ClientCapabilities`, `TraceLevel`,
`Workspace`, `RequestQueue`, `PushDiagnostics`, `PeriodicTickTimer`, `ShutdownReceived`,
`SolutionReadyAwaiters`.

### 3.7 Diagnostics Pipeline

#### Push Diagnostics (`Runtime/PushDiagnostics.fs`)

Encapsulates the background push-diagnostics pipeline, which pro-actively publishes `textDocument/publishDiagnostics`
notifications to clients that do not support pull diagnostics.

**Push vs Pull selection:** Push diagnostics are enabled only when the client does **not**
advertise `TextDocument.Diagnostic` capability. When the client sets
`TextDocument.Diagnostic = Some { ... }`, push is disabled and the client is expected to
use `textDocument/diagnostic` (pull) instead. The server always responds to pull requests
regardless of whether the client advertised the capability.

**State:** `PushDiagnosticsState` holds a `DocumentBacklog` (URIs of open documents sorted
by most-recently-touched) and `CurrentDocTask` (at most one in-flight resolution).
`pushDiagnosticsBacklogUpdate` rebuilds the backlog from all open documents across workspace
folders. `processPendingPushDiagnostics` pops the next URI, resolves diagnostics via
`doc.GetSemanticModelAsync()` → `Analyzers.getDocumentDiagnosticsWithAnalyzers`, and posts the result.

**Internal flow of `processPendingPushDiagnostics`:**

1. Checks `CurrentDocTask` — if one is already in flight, returns immediately.
2. Pops the next URI from `DocumentBacklog`.
3. Looks up the workspace folder via `workspaceFolder docUri` and the document via
   `workspaceFolderDocument AnyDocument docUri`.
4. Two code paths:
   - **Normal `.cs` documents:** Obtains `doc: Microsoft.CodeAnalysis.Document` from the
     workspace. The containing `Project` is accessible via `doc.Project`. Runs in an
     `async { }` CE via `Async.StartChild`. Obtains `CancellationToken` via
     `let! ct = Async.CancellationToken`. Calls
     `Analyzers.getDocumentDiagnosticsWithAnalyzers doc.Project semanticModel ct`.
   - **Razor `.cshtml` documents:** Falls through when `docForUri = None`, uses
     `solutionGetRazorDocumentForPath` to get the compilation and syntax tree, then calls
     `compilation.GetSemanticModel cshtmlTree` directly. Uses compiler diagnostics only
     (no analyzer support for Razor files yet).
5. Results are mapped via `Diagnostic.fromRoslynDiagnostic` and posted.

**Test access:** Push diagnostics are observable via `client.GetState().PushDiagnostics`,
which returns `Map<string, int option * Diagnostic[]>` keyed by document URI (the
`int option` is the document version).

#### Pull Diagnostics (`Handlers/Diagnostic.fs`)

Handles `textDocument/diagnostic` (per-document) and `workspace/diagnostic` (all projects).
Both paths include **compiler and analyzer diagnostics** via `Roslyn/Analyzers.fs`.

**Document pull (`handle`):** Receives `DocumentDiagnosticParams`, resolves the workspace
folder and semantic model via `workspaceFolderDocumentSemanticModel`. Also looks up the
document via `workspaceFolderDocument` to obtain `doc.Project` (needed for analyzer
references). Filters through `diagnosticIsToBeListed` (suppresses `CS8019` on `.cshtml`).
Calls `Analyzers.getDocumentDiagnosticsWithAnalyzers project semanticModel ct`.

**Workspace pull (`getWorkspaceDiagnosticReports`):** Takes a `knownResultIds` map (for
unchanged-report optimization) and the list of workspace folders. For each project:
- Checks if the client already holds results for this `project.Version` → emits `Unchanged`
- Otherwise calls `project.GetCompilationAsync(ct)` →
  `Analyzers.getCompilationDiagnosticsWithAnalyzers project compilation ct`,
  groups diagnostics by document URI, and emits `Full` reports
- Results flow through a bounded `Channel<WorkspaceDiagnosticsReportsChannelItem>(256)` with
  one `Async.Start` per project writing to the channel and the main consumer yielding from
  it as `AsyncSeq`

The `handleWorkspaceDiagnostic` function supports two modes based on `PartialResultToken`:
- `None` → collects all reports into a single `WorkspaceDiagnosticReport`
- `Some token` → streams each report via `$/progress` notifications, returns empty report

#### Analyzer Helpers (`Roslyn/Analyzers.fs`)

Two functions used by all three diagnostic code paths:

**`getDocumentDiagnosticsWithAnalyzers project semanticModel ct`**
- Falls back to `semanticModel.GetDiagnostics(ct)` when `project.AnalyzerReferences` is empty.
- Otherwise wraps in `CompilationWithAnalyzers` using `project.AnalyzerOptions` (which
  `MSBuildWorkspace` populates with the `AnalyzerConfigOptionsProvider` derived from
  `.editorconfig` files), calls `GetAllDiagnosticsAsync`, and filters to the document's
  file path. Uses `GetAllDiagnosticsAsync` rather than `GetAnalyzerSemanticDiagnosticsAsync`
  because the span-based filter in the latter misses some IDE diagnostics (e.g. IDE0040).

**`getCompilationDiagnosticsWithAnalyzers project compilation ct`**
- Falls back to `compilation.GetDiagnostics(ct)` when no analyzers are present.
- Otherwise calls `compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)` →
  `GetAllDiagnosticsAsync(ct)`.

**Key design point:** `project.AnalyzerOptions` is the `AnalyzerOptions` instance that
`MSBuildWorkspace` builds from the project's `<Analyzer>` items and `.editorconfig` files.
It already carries the `AnalyzerConfigOptionsProvider` that makes `.editorconfig`
severity rules (e.g. `dotnet_diagnostic.IDE0051.severity = warning`) visible to
analyzers — no manual `.editorconfig` discovery is needed.

## 4. Handler Module Pattern

Every handler in `Handlers/` follows a consistent pattern with these exports:

```fsharp
[<RequireQualifiedAccess>]
module CSharpLanguageServer.Handlers.SomeFeature

// Static capability declaration (plugged into ServerCapabilities)
let provider (clientCapabilities: ClientCapabilities) : U2<bool, SomeOptions> option =
    Some (U2.C1 true)

// Dynamic registration (sent via client/registerCapability after initialized)
let registration (config: CSharpConfiguration) (clientCapabilities: ClientCapabilities)
    : Registration option =
    Some { Id = "...; Method = "..."; RegisterOptions = Some ... }

// The actual request handler — returns (result, workspace update) tuple
let handle (context: RequestContext) (p: SomeParams)
    : Async<LspResult<SomeResult option> * LspWorkspaceUpdate> =
    async {
        // Use context.ClientCapabilities, context.GetWorkspaceFolder, etc.
        return LspResult.success (Some result), LspWorkspaceUpdate.Nothing
    }

// Optional: resolve, prepare, etc. for multi-step protocols
let resolve (context: RequestContext) (p: ResolveParams)
    : Async<LspResult<ResolvedResult> * LspWorkspaceUpdate> = ...
```

**Key types:**
- `RequestContext` — a class providing access to `LspClient`, `Config` (`CSharpConfiguration`), `ClientCapabilities`, `ShutdownReceived`, `GetWorkspaceFolder`, `GetWorkspaceFolderReadySolution`, and `GetWorkspaceFolderList`
- `RequestMode` — `ReadOnly` | `ReadWrite` | `ReadOnlyBackground`; controls concurrent scheduling
- `LspWorkspaceUpdate` — returned alongside the `LspResult` from every handler; carries workspace mutations to apply when the request retires (e.g. document changes, reload requests, settings changes)
- `LspResult<'T>` — from Ionide; carries either `Ok` or a JSON-RPC error
- Handler return type: `Async<LspResult<'T> * LspWorkspaceUpdate>` — a tuple of the LSP result and any workspace side-effects

---

## 5. Client Communication (`Lsp/Client.fs`)

`CSharpLspClient` extends Ionide's `LspClient()` base class:

- Delegates outbound **notifications** to `sendServerNotification` (wraps the transport's `SendNotification`)
- Delegates outbound **requests** (server→client) to `sendServerRequest` (wraps the transport's `SendCall`)
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

Builds a `ServerCapabilities` record by calling each handler module's `provider` function.
Takes `CSharpConfiguration` and `InitializeParams`:

```fsharp
let getServerCapabilities (config: CSharpConfiguration) (p: InitializeParams) =
    let cc = p.Capabilities
    { ServerCapabilities.Default with
        HoverProvider = Hover.provider cc
        DefinitionProvider = Definition.provider cc
        CompletionProvider = Completion.provider cc
        // ... all handler providers ...
    }
```

### Dynamic Registrations (`getDynamicRegistrations`)

Takes `CSharpConfiguration` and `ClientCapabilities`. Collects `Registration` values from
every handler's `registration` function (each taking `config` and `clientCapabilities`).
These are sent to the client during `handleInitialized` via `client/registerCapability`.

---

## 7. Test Infrastructure

### 7.1 Test Framework

- **NUnit** with `[<Parallelizable(ParallelScope.All)>]` — all tests run in parallel
- **net10.0** target, references main server project directly
- `Tooling.fs` is the test harness, compiled before all test files

### 7.2 Integration Test Architecture: Out-of-Process via stdio

Tests do **not** use in-process hosting. Instead:

1. **Spawns a real server process** — `makeServerProcessInfo` locates the built
   `CSharpLanguageServer` executable relative to the test assembly, creates a
   `ProcessStartInfo` with `RedirectStandardInput/Output/Error = true`

2. **Communicates via stdin/stdout using `JsonRpc.fs`** — `LoadSolution` calls
   `startJsonRpcTransport` on the server's stdio streams, registering handlers for all
   server→client calls and notifications. A thin `MailboxProcessor<LspClientEvent>` (the
   "state actor") holds observable state (`RpcLog`, `PushDiagnostics`, etc.); the transport
   feeds it via `rpcLogCallback` and `UpdateState` posts from notification handlers.

3. **Concurrency control** — `activeClientsSemaphore` (`SemaphoreSlim` initialized to
   `Environment.ProcessorCount`) throttles simultaneous server processes

### 7.3 Key Test Classes

#### `LspTestClient` (primary test API)

```fsharp
use client = new LspTestClient(clientProfile)
client.LoadSolution(fixtureName, patchSolutionDir, initializeParamsUpdate)
                        // copies fixture → temp dir, starts server, initialize + initialized,
                        // waits for "Finished loading workspace" progress event

let result: HoverResult = client.Request("textDocument/hover", hoverParams)
let state = client.GetState()  // ServerCapabilities, PushDiagnostics, etc.

client.ServerDidRespondTo("textDocument/hover")     // RPC log assertion
client.ServerMessageLogContains(fun m -> ...)       // message log assertion
client.WaitForProgressEnd(fun p -> ...)             // poll for $/progress end (20s timeout)
```

#### `LspDocumentHandle` (single-document management)

```fsharp
use file = client.Open "Project/Class.cs"    // sends textDocument/didOpen
file.Change(newText)                          // sends textDocument/didChange
file.Save()                                   // sends textDocument/didSave
// Dispose sends textDocument/didClose
```

### 7.4 Test Harness API (`Tooling.fs`)

#### Client Activation Functions

```fsharp
// Simple: default profile, no fixture patching, no InitializeParams customization
let activateFixture (fixtureName: string) : LspTestClient

// Full control: custom profile, fixture dir patch callback, InitializeParams transform
let activateFixtureExt
    (fixtureName: string)
    (clientProfile: LspClientProfile)
    (patchFixtureDir: string -> unit)       // called after temp copy, before server start
    (initializeParamsUpdate: InitializeParams -> InitializeParams)
    : LspTestClient

let emptyFixturePatch: string -> unit       // no-op, for use when no patching needed
```

#### `LspClientProfile` type

Controls per-test server/client configuration:

```fsharp
type LspClientProfile =
    { LoggingEnabled: bool                  // echo RPC traffic to stderr
      ClientCapabilities: ClientCapabilities
      SolutionLoadDelay: int option         // injected via csharp.debug.solutionLoadDelay config
      ExtraEnv: Map<string, string>         // additional env vars for the server process
      ExtraArgs: string list }              // additional CLI args (appended to --features razor-support)
```

`defaultClientProfile` uses `defaultClientCapabilities` which sets:
- `TextDocument.Diagnostic = None` (pull diagnostics **not** advertised)
- `Workspace.Diagnostics = None` (workspace pull diagnostics **not** advertised)
- `TextDocument.CodeAction.CodeActionLiteralSupport = Some { ... }` (empty `ValueSet`)
- `TextDocument.DocumentSymbol.HierarchicalDocumentSymbolSupport = Some true`
- `Window.WorkDoneProgress = Some true`
- `Experimental = Some {| csharp = {| metadataUris = true |} |}`

To test pull diagnostics, create a custom profile that sets both `TextDocument.Diagnostic`
and `Workspace.Diagnostics`:

```fsharp
let pullDiagProfile =
    { defaultClientProfile with
        ClientCapabilities =
            { defaultClientCapabilities with
                TextDocument =
                    Some { defaultClientCapabilities.TextDocument.Value with
                            Diagnostic = Some { DynamicRegistration = Some true
                                                 RelatedDocumentSupport = None } }
                Workspace =
                    Some { defaultClientCapabilities.Workspace.Value with
                            Diagnostics = Some { RefreshSupport = Some true } } } }
```

#### Other Tooling Utilities

- `patchFixtureWithTfm (newTfm: string)` — returns a `patchFixtureDir` callback that
  rewrites `<TargetFramework>` in all `.csproj` files under the fixture
- `activateFixtureWithLoggingEnabled` — shorthand for `activateFixtureExt` with
  `LoggingEnabled = true`
- `getWorkspaceDiagnosticsForUri` — helper to extract diagnostics for a specific URI from
  a `WorkspaceDiagnosticReport`
- `waitUntilOrTimeout` — polls a predicate with 50ms intervals, fails after timeout

### 7.5 Fixtures

#### Temp-dir Preparation

`prepareTempTestDirFrom` copies fixture project files to a temp directory, filtering out
`bin`/`obj`. On macOS, prepends `/private` to the temp path.

**File filter** — only these extensions are copied: `.cs`, `.csproj`, `.sln`, `.slnx`,
`.cshtml`, `.editorconfig`, `global.json`, `.txt`. All other files and `bin`/`obj`
directories are excluded.

#### Directory Conventions

- Directory name matches the `.sln` file name (e.g. `genericProject/genericProject.sln`)
- C# source files live in a `Project/` subdirectory alongside `Project.csproj`
- Multi-project fixtures may omit the `.sln` (e.g. `projectWithSourceGenerator/`)

#### Pre-Build Pattern

Since `bin`/`obj` are excluded from the temp copy, fixtures that need NuGet-restored assets
(e.g. `obj/project.assets.json` for `MSBuildWorkspace` to resolve `AnalyzerReferences` or
source generator DLLs) must be pre-built after the copy. This is done via the
`patchFixtureDir` callback in `activateFixtureExt`:

```fsharp
let private prebuildProject (solutionDir: string) =
    let projectDir = Path.Combine(solutionDir, "Project")
    let psi = ProcessStartInfo("dotnet", "build")
    psi.WorkingDirectory <- projectDir
    // ...
    let proc = Process.Start(psi)
    proc.WaitForExit()

use client =
    activateFixtureExt "someFixture" defaultClientProfile prebuildProject id
```

Fixtures using this pattern:
- `projectWithSourceGenerator` — builds the generator DLL so Roslyn can load it as an analyzer
- `projectWithEditorConfigAnalyzers` — restores the project so `MSBuildWorkspace` sees analyzer references

#### Available Fixtures

| Fixture | Purpose |
|---------|---------|
| `genericProject` | General-purpose C# project used by most tests |
| `aspnetProject` | ASP.NET project with controllers, views, and Razor files |
| `multiFolderWorkspace` | Two separate project folders for multi-root workspace tests |
| `multiTargetProject` | Project targeting multiple TFMs |
| `projectWithEditorConfig` | Project with an `.editorconfig` for formatting-only rules |
| `projectWithEditorConfigAnalyzers` | Project with `.editorconfig` diagnostic severity rules (IDE0040, IDE0051, IDE0032) for analyzer tests |
| `projectWithSlnx` | Project using a `.slnx` solution file |
| `projectWithSourceGenerator` | Project with an incremental source generator for generated-document tests |
| `testDiagnosticsWork` | Project with deliberate errors for diagnostic tests |
| `testReferenceWorksDotnet8` | Project pinned to .NET 8 for reference/Go-to-def tests |

### 7.6 Test File Conventions

- **Module-level `[<Test>]` let bindings** — all test files use top-level module functions,
  not `[<TestFixture>]` class style:
  ```fsharp
  module CSharpLanguageServer.Tests.SomeFeatureTests
  // ...
  [<Test>]
  let testSomething () = ...
  ```
- **`.fsproj` compile order matters** — F# requires files in dependency order. New test
  files are added as `<Compile Include="SomeFeatureTests.fs" />` in the `<ItemGroup>`.
  Test files come after `Tooling.fs` (which defines the harness). Place new entries near
  related existing test files.
- **Naming:** file name matches module suffix (e.g. `AnalyzerTests.fs` →
  `module CSharpLanguageServer.Tests.AnalyzerTests`)

### 7.7 Typical Test Pattern

```fsharp
[<Test>]
let testSomething () =
    use client = activateFixture "genericProject"
    use doc = client.Open "Project/Class.cs"

    let result: SomeType =
        client.Request("textDocument/someMethod", someParams)

    Assert.AreEqual(expected, result)
    // Dispose: didClose → shutdown → kill process → delete temp dir
```

### 7.8 Other Test Categories

| Category | File | How it works |
|----------|------|-------------|
| JSON-RPC transport unit tests | `JsonRpcTests.fs` | Calls `startJsonRpcTransport` with in-memory streams (no process) — see [jsonrpc.md](jsonrpc.md) |
| Progress reporter unit tests | `ProgressReporterTests.fs` | Uses a `TrackingLspClientStub` mock |
| CLI diagnose command tests | `DiagnoseCommandTests.fs` | Spawns process with `--diagnose` flag |
| Request scheduling unit tests | `RequestSchedulingTests.fs` | Tests `RequestQueue` directly with no server process |
| CSharp metadata (go-to-decompiled) integration tests | `CSharpMetadataTests.fs` | Integration tests via `LspTestClient` |
| Call hierarchy integration tests | `CallHierarchyTests.fs` | Integration tests via `LspTestClient` |
| Source generator tests | `SourceGeneratorTests.fs` | Uses `prebuildGenerator` callback + `projectWithSourceGenerator` fixture |
| Analyzer / EditorConfig diagnostic tests | `AnalyzerTests.fs` | Uses `prebuildProject` callback + `projectWithEditorConfigAnalyzers` fixture; custom `LspClientProfile` with pull diagnostics enabled |
| Internal / cross-cutting tests | `InternalTests.fs` | Tests for internal utilities and cross-cutting concerns |

---

## 8. Adding a New LSP Feature — Checklist

1. **Create handler file** `Handlers/NewFeature.fs` following the handler module pattern
   (export `provider`, `registration`, `handle`)
2. **Add to `.fsproj`** — insert `<Compile Include="Handlers/NewFeature.fs" />` in the
   Handlers section (order matters in F#)
3. **Register in `Lsp/Server.fs`**:
   - Add to `getServerCapabilities` (static capability)
   - Add to `getDynamicRegistrations` (if using dynamic registration)
   - Add to `configureRpcTransport` in the appropriate handler map (`callHandlers` or
     `notificationHandlers`) with the correct `RequestMode`
4. **Write tests** — create `tests/.../NewFeatureTests.fs`, add a fixture under
   `Fixtures/` if needed, use `LspTestClient` + `LspDocumentHandle` pattern
5. **Add test file to test `.fsproj`**
