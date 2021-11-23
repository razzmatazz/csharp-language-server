# (unreleased)
- Drop local LanguageServerProtocol clone from FSAC and use [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol) instead;
- Fix file creation via code actions (e.g. when extracting an interface or inner class to another file);
- List params and exceptions in the same order as they appear on docxml on the symbol (instead of random order) -- [csharp-language-server#6](https://github.com/razzmatazz/csharp-language-server/issues/6);
- Disable incorrectly implemented `textDocument/implementation` provider [csharp-language-server#4](https://github.com/razzmatazz/csharp-language-server/issues/4);

# 0.1.6
- Update roslyn+msbuild libs to support for `net6.0` projects;
- Use .NET 6.0 for runtime;
- Fix `workspace/symbol` to return correct locations;
- Implement `textDocument/implementation` (alias to `textDocument/definition` actually);
- Support for `codeAction/resolve` so we have faster code action resolution/less overhead when working on large solutions.

# 0.1.5
- Enable code actions that are nested, like "generate field/property/ro-field/ro-property";
- Update how tooltip on hover is formatted; should be much nicer;
- Fix how adding new file works -- don't write unnecesary `<Compile>` when a new file is added and trigger diagnostics update immediately.

# 0.1.4
- Don't reference NuGet.* packages as those bind to particular(?) sdk -- things work better now, as it was not working for me with updated SDK dependencies;
- Update roslyn dependencies;
- Adding new file works (i.e. no need to restart workspace anymore);
  - There is an issue that Roslyn adds (unnecessary) `<Compile>` to the project;
- Don't highlight namespace symbols as those can cover all the symbols on the source file and that is not useful;
- Add -l/--loglevel <debug|info|warning|error> param;
- Expose C# docxml as markdown for more proper on-hover experience (still needs more love, FsAutoComplete does it beautifully);
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
