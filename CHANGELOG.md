# (unreleased)
- Don't publish diagnostics for C# documents generated from metadata.

# 0.1.3
- Support `--help` and `--version` command line params.
- Support for `-s/--solution` param to load specific solution file.

# 0.1.2
- No -alpha version number suffix so you don't need --version to install with `dotnet tool install`;
- Don't return from `initialize` until we actually load the solution;
- Report csharp-ls version number to the client.

# 0.1.1-alpha
Initial version:
- Most of basic LSP features work: rename/go-to-def/find references, etc;
- Support for textDocument/documentHighlight;
- Go-to-metadata (needs integration from your LSP client;)
- Published as `dotnet tool csharp-ls`
