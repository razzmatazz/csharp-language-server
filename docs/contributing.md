# Contributing

See [codebase-architecture.md](codebase-architecture.md) for an overview of the
project structure, LSP message handling, request scheduling, and test infrastructure.

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

## Install locally

```
dotnet pack src/CSharpLanguageServer
dotnet tool install --global --add-source src/CSharpLanguageServer/nupkg csharp-ls
```
