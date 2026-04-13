# Description

csharp-ls brings advanced language features like code completion, diagnostics,
and refactoring to your editor for C# projects. It supports projects targeting
older .NET SDK versions including .NET Core 3, .NET Framework 4.8,
and potentially earlier ones.

csharp-ls is MIT-licensed (see [LICENSE](LICENSE)) and is provided with no
warranty of any kind.

# Documentation

See [CHANGELOG.md](CHANGELOG.md) for the list of recent improvements/fixes.

See [docs/features.md](docs/features.md) for a more detailed discussion regarding features
and customization provided with csharp-ls.

See [docs/troubleshooting.md](docs/troubleshooting.md) for server troubleshooting and
diagnostics.

See [docs/contributing.md](docs/contributing.md) for build instructions and codebase overview.

See [docs/codebase-architecture.md](docs/codebase-architecture.md) for an overview of the project structure, LSP message handling, request scheduling, and test infrastructure.

# Quick Start

csharp-ls requires the .NET 10 SDK or later to be installed on your machine.
Please head to [Download .NET](https://dotnet.microsoft.com/en-us/download) to
download it.

The server can be installed as [csharp-ls dotnet tool](https://www.nuget.org/packages/csharp-ls/):

```
dotnet tool install --global csharp-ls
```
Once installed, your editor's LSP client should automatically detect and start
`csharp-ls` when opening C# project files.

# Settings

## Configuration

Settings are read from the `csharp` workspace configuration section
(`workspace/configuration`) and can also be provided via `workspace/didChangeConfiguration`.

- `csharp.logLevel` - log level for server output forwarded via `$/logTrace`;
  one of `trace`, `debug`, `info`, `warning`, `error`; defaults to `info`

- `csharp.applyFormattingOptions` - use formatting options as supplied by the
  client (may override `.editorconfig` values); defaults to `false`

- `csharp.useMetadataUris` - serve decompiled metadata and source-generated
  documents under `csharp:/` URIs; defaults to `false`

- `csharp.razorSupport` - enable Razor (`.cshtml`) document support;
  defaults to `false`

- `csharp.solutionPathOverride` - override the solution path to load; useful for
  specifying an alternative solution when multiple exist in the workspace;
  can also be set via the `--solution` CLI flag; defaults to `null`

- `csharp.locale` - force the output language for diagnostics and messages,
  e.g. `en-US` or `de`; useful for consistent logs across different OS locales;
  overrides `DOTNET_CLI_UI_LANGUAGE`; defaults to the system locale

- `csharp.debug.solutionLoadDelay` - delay in milliseconds before loading the
  solution after the workspace is ready

- `csharp.debug.debugMode` - enable debug mode, which logs periodic request
  queue statistics; defaults to `false`

## Command Line Arguments

```
USAGE: csharp-ls [--help] [--version] [--loglevel <level>] [--solution <solution>] [--locale <locale>] [--debug] [--diagnose] [--features <features>] [--rpclog <path>]

OPTIONS:

    --version, -v         display versioning information
    --loglevel, -l <level>
                          set log level, <trace|debug|info|warning|error>; default is `info`
    --solution, -s <solution>
                          specify .sln file to load (relative to CWD)
    --locale, -L <locale> force output locale, e.g. `en-US` or `de`; overrides DOTNET_CLI_UI_LANGUAGE
    --debug               enable debug mode
    --diagnose            run diagnostics
    --features, -f <features>
                          enable optional features, comma-separated: [metadata-uris, razor-support]
    --rpclog <path>       write RPC wire log to file (READ/WRITE/ERROR/...)
    --help                display this list of options.
```

See `csharp-ls --help`.

## Environment Variables

- `DOTNET_CLI_UI_LANGUAGE` - sets the output locale for diagnostics and messages
  (e.g. `en-US`, `de`); follows the standard dotnet CLI convention; overridden
  by `--locale` / `csharp.locale` if set

### LSP Tracing

csharp-ls supports the LSP `$/setTrace` and `$/logTrace` notifications. When a
client sets tracing to `messages` or `verbose`, the server forwards its internal
log output as `$/logTrace` notifications. This is independent of `--loglevel`,
which only controls stderr console output. In VS Code, tracing can be enabled
via the `[langId].trace.server` setting.

## Extended Client Capabilities

### Decompiled and Source-Generated Document URIs
When enabled, `textDocument/definition` on symbols in compiled assemblies or
source-generated files returns a virtual `csharp:/` URI instead of a temporary
file path. The URI can then be passed to the custom `csharp/metadata` request to
retrieve the decompiled or generated source text.

- Decompiled symbols: `csharp:/<project.csproj>/decompiled/<Symbol>.cs`
- Source-generated files: `csharp:/<project.csproj>/generated/<HintName>`

This can be enabled via the `csharp.useMetadataUris` configuration setting, the
`--features metadata-uris` CLI flag, or by setting the
`experimental.csharp.metadataUris` client capability to `true`.

### Razor (.cshtml) support
Can be enabled via the `csharp.razorSupport` configuration setting or by passing
the `--features razor-support` CLI flag.

# Clients

`csharp-ls` implements the standard LSP protocol to interact with your editor.
However, there are some features that require a non-standard implementation.
This is where editor-specific plugins can be helpful.

Notable clients:
* Neovim: [csharpls-extended-lsp.nvim](https://github.com/Decodetalkers/csharpls-extended-lsp.nvim)
* Emacs: [emacs/lsp-mode](https://github.com/emacs-lsp/lsp-mode)
* Visual Studio Code: [vscode-csharp-ls @ github](https://github.com/vytautassurvila/vscode-csharp-ls)

# Acknowledgements

* csharp-ls is not affiliated with Microsoft Corp.,
* csharp-ls uses [Roslyn](https://github.com/dotnet/roslyn) to parse and update code,
* csharp-ls uses the LSP interface from the [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol) project,
* csharp-ls uses [ILSpy/ICSharpCode.Decompiler](https://github.com/icsharpcode/ILSpy) to decompile types in assemblies to C# source.

# Alternatives

- [OmniSharp](https://github.com/OmniSharp/omnisharp-roslyn)
- [C# Dev Kit for VSCode](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csdevkit)


