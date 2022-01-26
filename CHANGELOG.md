# (unreleased)
- Run diagnostics resolution outside the main state actor so we don't lock up other processing;
- Add timeout for codelens requests to avoid excessive CPU usage as that is prone to run for a long time; 
- Really fix write-request serialization;
- Revert the emacs-29 fix, didn't do much.

# 0.2.1
- Carefully observe incoming requests from StreamJsonRpc to actually serialize write-access to solution state; 
  - Should fix sync issues (another attempt..)
- Attempt to fix an issue with emacs-29 by setting `bufferSize` in `Console.OpenStandardInput` call.

# 0.2.0
- Support for document formatting, courtesy of @kstatz12;
- Fix `textDocument/didOpen` handler:
  - we will now refresh roslyn solution with file contents as reported by the editor, as our state could have been outdated since;
- Fix sync issues by serializing writes to server state;
- Implement incremental file change sync to improve performance for large files;
- Handle $/cancellationRequest's to handle cancellation requests properly.

# 0.1.9
- Refactor server state tracking to (hopefully) fix some issues with document sync/versioning;
- Fix, hopefully in the final form, how symbols are formatted by using roslyn To*DisplayString() methods;
- Don't show symbol attributes on textDocument/documentSymbol response;

# 0.1.8
- Implementation for code lens that shows number of references;
  - Needs client implementation of `workspace/executeCommand`: `csharp.showReferences`;
- Nicer on-hover markdown that should match the context better;
- Expose properties & events on textDocument/documentSymbol;
- Pull `add using import` code action to the top of action list, mark it prefered and with `Kind`, [csharp-language-server#9](https://github.com/razzmatazz/csharp-language-server/issues/9). 

# 0.1.7
- Bump roslyn libs to a newer version;
- Add support for `textDocument/implementation`;
- Expose more bits from xmldoc properly on tip on hover;
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
