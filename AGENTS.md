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
- LSP message handling, request scheduling (`ReadOnly` / `ReadWrite` / `ReadOnlyBackground`)
- Handler module pattern (`provider`, `registration`, `handle`)
- Test infrastructure (`LspTestClient`, `LspDocumentHandle`, fixtures)
- Checklist for adding a new LSP feature

## Iteration Style

Don't over-think syntax or minor details upfront — make structural changes then let
`dotnet build` / `dotnet test` drive the fixes.

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
