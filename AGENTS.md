# AGENTS.md

This project is an F# LSP server (`csharp-ls`). Agents and humans share the same docs —
prefer adding new content under `docs/` and linking here rather than duplicating it.

**Before exploring the codebase, read the Key Docs below.** They cover architecture,
conventions, and test infrastructure in full — reading them first is faster than inferring
the same from source files.

## Setup

See [CONTRIBUTING.md](CONTRIBUTING.md) for prerequisites, build/test commands, code style, and local install.

## Project Structure & Architecture

See [docs/codebase-architecture.md](docs/codebase-architecture.md) for a full walkthrough of:
- Source layout (`src/CSharpLanguageServer/`, `tests/`)
- LSP message handling, request scheduling
- Handler module pattern (`provider`, `registration`, `handle`)
- Test infrastructure (`LspTestClient`, `LspDocumentHandle`, fixtures)
- Checklist for adding a new LSP feature

## Debugging server-side data flow from tests

1. Swap `activateFixture` for `activateFixtureWithLoggingEnabled` in the failing test —
   this forwards server stderr to `Console.Error`.
2. Add `eprintfn "[DEBUG ...]"` calls at points of interest in the server source.
3. Run `dotnet test --filter FullyQualifiedName~<TestName> --logger "console;verbosity=detailed"`
   and read the interleaved `[+NNNms logger]` timeline.
4. Remove the `eprintfn` calls and revert to `activateFixture` when done.

**Read `$/csharp/debugInfo` first** — it's printed to Standard Output on every test
dispose and often pinpoints the problem without any code changes.  Key fields:
`workspace.phase` (`"Uninitialized"` = `initialized` never completed),
`requestQueue.stats` (suspiciously short `avgDurationMs` on a notification = it crashed
silently before doing any work), `workspace.folders` (empty = no solution loaded).

**Silent notification failures:** notification handlers (`initialized`, `textDocument/didOpen`,
…) throw into `Async.StartWithContinuations` in `JsonRpc.fs`, which discards the
exception — no wire error is sent.  If the crash happens inside `deserialize` (e.g. a new
STJ path that can't round-trip `unit` or an array-typed erased-union case), wrap it in
`try`/`catch` + `eprintfn` + `reraise()` in `wrapHandler` (`Lsp/Server.fs`) to surface it.

## Iteration Style

DON'T OVER-THINK SYNTAX OR MINOR DETAILS UPFRONT — make structural changes then let
`dotnet test` drive the fixes.

Always run tests as `dotnet test` (or `dotnet test --filter ...`) — never `dotnet test --no-build`.
The `--no-build` flag skips recompiling the test project, which means type errors in test
code are silently ignored and the stale binary from a previous build is used instead.
There is no meaningful performance win from skipping the build step.

## Code Style & Commit Conventions

See [CONTRIBUTING.md](CONTRIBUTING.md#code-style).

Commit messages should be a short single-line summary — no bullet lists of individual changes.

## Key Docs

| Topic | File |
|-------|------|
| Features & customization | [docs/features.md](docs/features.md) |
| Troubleshooting / `--diagnose` | [docs/troubleshooting.md](docs/troubleshooting.md) |
| JSON-RPC transport internals | [docs/jsonrpc.md](docs/jsonrpc.md) |
| Contributing / build / test / code style & commits | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Architecture deep-dive | [docs/codebase-architecture.md](docs/codebase-architecture.md) |
