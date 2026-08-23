# Contributing

## Prerequisites

- [.NET 10 SDK](https://dotnet.microsoft.com/en-us/download) (version 10.0.100 or later,
  as specified in `global.json`)

## Build

```
dotnet build
```

## Nix development environment

Enter the reproducible development shell with:

```
nix develop
```

The regular `dotnet build` workflow does not require a Nix build. Build the Nix package when
you need to verify or produce the packaged application:

```
nix build .#csharp-ls
```

After changing NuGet dependencies, update the Nix dependency lock with:

```
nix run .#update-deps
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

### Gradual Expecto migration

The test project is being migrated from NUnit to [Expecto](https://github.com/haf/expecto)
one file at a time, starting with `InternalTests.fs` as the pilot. Both frameworks coexist
in the *same* test project: `NUnit3TestAdapter` and `YoloDev.Expecto.TestSdk` are both VSTest
adapters, so a single `dotnet test` (or `make test`) invocation discovers and runs tests from
both side by side, with no separate project or custom entry point required. When porting a
file, factor the assertion logic into a shared private function called from both the NUnit
`[<Test>]`/`[<TestCase>]` functions and a new Expecto `[<Tests>]` `testList`, so the two
variants can't silently drift apart while both exist.

The `Makefile` runs the two frameworks separately instead, so Expecto-migrated suites get
Expecto's own (more concurrent) test runner rather than going through VSTest:

```
make test          # test-nunit, then test-expecto
make test-nunit     # NUnit suite via `dotnet test` (VSTest), excluding migrated suites
make test-expecto   # Expecto-migrated suites via `dotnet run` (Expecto's own runner)
```

`test-nunit` excludes each Expecto-migrated test list (by its `testList` name) via
`--filter`, listed in `EXPECTO_TEST_LISTS` at the top of the `Makefile` — update it whenever
another file is ported to Expecto, or `test-nunit` and `test-expecto` will run it twice.
`test-expecto` needs no such list: Expecto discovers every `[<Tests>]` value in the assembly
by reflection when invoked via its own `main` (`tests/CSharpLanguageServer.Tests/Program.fs`),
and NUnit `[<Test>]` functions are invisible to that discovery.

## Test Guidelines

Inside `async {}` handler lambdas passed to the transport or scheduler, never use
blocking calls (`mre.Wait()`, `Async.RunSynchronously`, `task.Result`, `Thread.Sleep`).
They starve the thread pool and deadlock the `MailboxProcessor` event loop.
Use async equivalents (`Async.AwaitWaitHandle`, `let!`, `Async.AwaitTask`, `Async.Sleep`) instead.
This restriction does not apply to the test body itself, which runs on a dedicated thread.

## Code Style

- **Language is F#**, not C#. File order in `.fsproj` matters — new files must be inserted at the correct position.
- Every handler in `Handlers/` must export `provider`, `registration`, and `handle`, and return `Async<LspResult<'T> * LspWorkspaceUpdate>`.
- Tag every handler with the correct `RequestMode` (`ReadOnly`, `ReadWrite`, or `ReadOnlyBackground`) when registering in `Lsp/Server.fs`.
- Test functions use top-level `[<Test>]` let bindings inside a module — not `[<TestFixture>]` classes.
- Formatter: **Fantomas** (declared in `.config/dotnet-tools.json`). Set up with `dotnet tool restore`, then run on any F# files you modify before committing:
  ```
  dotnet fantomas <file-or-dir>
  ```

## Git Hooks

A pre-commit hook (`.githooks/pre-commit`) runs `dotnet fantomas --check` on staged F#
files and blocks the commit if any aren't formatted. Opt in once per clone with:

```
git config core.hooksPath .githooks
```

Requires `dotnet tool restore` to have been run at least once. Skip it for a single
commit with `git commit --no-verify`.

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/) style:

```
<type>(<scope>): <short summary>
```

Examples:

```
feat(hover): include XML doc summary for property symbols
fix(completion): avoid NullReferenceException on empty import list
refactor(diagnostics): extract analyzer helper into Roslyn/Analyzers.fs
test(codeaction): add integration test for organize-imports action
docs(contributing): add conventional commits guidance
chore(deps): bump Fantomas to 7.0.3
```

## Install locally

```
dotnet pack src/CSharpLanguageServer
dotnet tool install --global --add-source src/CSharpLanguageServer/nupkg csharp-ls
```
