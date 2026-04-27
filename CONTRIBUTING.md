# Contributing

## Prerequisites

- [.NET 10 SDK](https://dotnet.microsoft.com/en-us/download) (version 10.0.100 or later,
  as specified in `global.json`)

## Build

```
dotnet build
```

## Run tests

```
dotnet test
```

`dotnet test` will build the project automatically before running the tests.
To run only the unit tests (request scheduling, JSON-RPC transport, etc.) without starting
a server process, filter by category:

```
dotnet test --filter "FullyQualifiedName~RequestScheduling|FullyQualifiedName~JsonRpc|FullyQualifiedName~ProgressReporter"
```

## Code Style

- **Language is F#**, not C#. File order in `.fsproj` matters — new files must be inserted at the correct position.
- Every handler in `Handlers/` must export `provider`, `registration`, and `handle`, and return `Async<LspResult<'T> * LspWorkspaceUpdate>`.
- Tag every handler with the correct `RequestMode` (`ReadOnly`, `ReadWrite`, or `ReadOnlyBackground`) when registering in `Lsp/Server.fs`.
- Test functions use top-level `[<Test>]` let bindings inside a module — not `[<TestFixture>]` classes.
- Formatter: **Fantomas** (declared in `.config/dotnet-tools.json`). Set up with `dotnet tool restore`, then run on any F# files you modify before committing:
  ```
  dotnet fantomas <file-or-dir>
  ```

## Install locally

```
dotnet pack src/CSharpLanguageServer
dotnet tool install --global --add-source src/CSharpLanguageServer/nupkg csharp-ls
```
