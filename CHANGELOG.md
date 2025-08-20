# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [0.19.0] - 2025-08-20 / Kapčiamiestis
* Select common target framework when multiple projects are found
  - By @AdeAttwood in https://github.com/razzmatazz/csharp-language-server/pull/253
* Fix how completion item details are resolved
  - https://github.com/razzmatazz/csharp-language-server/pull/251
* Apply simple heuristics to select a .sln/.slnx file when multiple are found.
  - https://github.com/razzmatazz/csharp-language-server/pull/250
* Print error mesage to stderr in case of invalid args or a server crash.
  - Fixed by @zachristmas in https://github.com/zachristmas/csharp-language-server
* Fix completion item kind for an extension method
  - Reported by @sharpchen in https://github.com/razzmatazz/csharp-language-server/issues/237
  - Fixed in https://github.com/razzmatazz/csharp-language-server/pull/238
* Documentation formatting fixes
  - Reported by @prashanthbabu07 in https://github.com/razzmatazz/csharp-language-server/issues/247
  - Fixed in https://github.com/razzmatazz/csharp-language-server/pull/248

## [0.18.0] - 2025-06-23 / Pabradė
* Upgrade Roslyn to 4.14.0
* Use diagnostic url from Roslyn
  - By @CoolCoderSuper https://github.com/razzmatazz/csharp-language-server/pull/227
* Support loading slnx files
  - By @CoolCoderSuper https://github.com/razzmatazz/csharp-language-server/pull/226
* Fix how doc strings are rendered
  - https://github.com/razzmatazz/csharp-language-server/pull/228
* Implement (experimental) support for loading multi-tfm solutions
  - https://github.com/razzmatazz/csharp-language-server/pull/205
* Implement workspace diagnostics
  - https://github.com/razzmatazz/csharp-language-server/pull/235

## [0.17.0] - 2025-04-30 / Krokšlys
* Upgrade Roslyn to 4.13.0
  - https://github.com/razzmatazz/csharp-language-server/pull/220
* csharp-ls will now use and require .NET 9 runtime/SDK
  - PR by @lapponiandevil in https://github.com/razzmatazz/csharp-language-server/pull/222
* Upgrade Ionide.LanguageServerProtocol to 0.7.0:
  - https://github.com/razzmatazz/csharp-language-server/pull/221
* Reduce startup notifications by using "window/logMessage"
  - By @nikolightsaber in https://github.com/razzmatazz/csharp-language-server/pull/217
* Make sure `.editorconfig` settings are respected, add `csharp.applyFormattingOptions` override (defaults to false):
  - By @DmitryHudrich in https://github.com/razzmatazz/csharp-language-server/pull/204

**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/0.16.0...0.17.0

## [0.16.0] - 2024-12-24 / Nemunaitis
* Upgrade Roslyn to 4.12.0
* Fix an issue where server breaks when inspecting class/properties involved in source-generated code:
  - By @granitrocky in https://github.com/razzmatazz/csharp-language-server/pull/189
* Make sure textDocument/findReferences respects Context.IncludeDeclaration
  - https://github.com/razzmatazz/csharp-language-server/pull/199
* Fix an issue where server would file on projects using code generators (like Asp.Net razor) on some of the endpoints:
  - https://github.com/razzmatazz/csharp-language-server/pull/190

**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/0.15.0...0.16.0

## [0.15.0] - 2024-08-15 / Šventoji
* Upgrade Roslyn to 4.10.0:
  - https://github.com/razzmatazz/csharp-language-server/pull/182
* Implement pull diagnostics for better performance;
  - https://github.com/razzmatazz/csharp-language-server/pull/174

**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/0.14.0...0.15.0

## [0.14.0] - 2024-06-23 / Palanga
* Speed up completion by not showing name suggestions or items from unimported namespaces
  - https://github.com/razzmatazz/csharp-language-server/pull/168
* Bump Ionide.LanguageServerProtocol to 0.6.0, fix some of the types used for dynamic registration
* Reduce noise when loading >10 project files, include currently loading project in progress bar.
  - By @BrianCArnold in https://github.com/razzmatazz/csharp-language-server/pull/162
* Register TextDocumentSync lazily
  - By Adam Tao @tcx4c70
* Support completionItem/resolve method to speed up completion;
  - By Adam Tao @tcx4c70
* Do not rename symbol in strings or comments, restore file rename
  - As reported by @Myrslaver in https://github.com/razzmatazz/csharp-language-server/issues/157
* Stop collecting symbols in method body
  - By Adam Tao @tcx4c70
* Fix TypeHierarchy registration
  - By Adam Tao @tcx4c70
* Fix wrong references number
  - By Adam Tao @tcx4c70
* Add progress reporting when loading a solution/project
  - By Adam Tao @tcx4c70
* More refactoring and fixes from rework branch:
  - By Adam Tao @tcx4c70, from https://github.com/tcx4c70/csharp-language-server/commits/rework/

**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/0.13.0...0.14.0

## [0.13.0] - 2024-05-08 / Baltoji Vokė
* Bump Ionide.LanguageServerProtocol dependency to v0.5.0
* Fix performance issue in completion handler introduced in 0.12.0
  - Reported by @DanielViberg in https://github.com/razzmatazz/csharp-language-server/issues/151, bisected by @DmitryHudrich
* Fix "Extract interface.." code action
  - Reported by @JorgeHerreraU in https://github.com/razzmatazz/csharp-language-server/issues/152
* Bump MSBuild, Argu and ICSharpCode.Decompiler dependency versions

**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/0.12.0...0.13.0

## [0.12.0] - 2024-04-22 / Valkininkai
* Update Roslyn 4.7.0->4.9.2 to support C# 12
  - PR by @arussellk: https://github.com/razzmatazz/csharp-language-server/pull/141
* Register server capabilities with the client lazily to improve startup performance
  - PR by @tcx4c70: https://github.com/razzmatazz/csharp-language-server/pull/102
* Rework logging to be based on Serilog
  - PR by @tcx4c70: https://github.com/razzmatazz/csharp-language-server/pull/134
* More refactoring by @tcx4c70
  - PR by @tcx4c70: https://github.com/razzmatazz/csharp-language-server/pull/102
* Change package management to CPM
  - PR by @tcx4c70: https://github.com/razzmatazz/csharp-language-server/pull/132

### More about Valkininkai, Lithuania
- [Valkininkai on WP](https://en.wikipedia.org/wiki/Valkininkai)
- [Google Images on Valkininkai](https://www.google.com/search?tbm=isch&q=valkininkai+lithuania)

## [0.11.0] - 2024-01-21 / Jieznas
* Update Ionide.LanguageServerProtocol dependency to v0.4.22
* refactoring by @tcx4c70
  - PR by @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/102
* csharp-ls will now use and require .NET 8 runtime/SDK
  - PR by @BeauCranston in https://github.com/razzmatazz/csharp-language-server/pull/122
* Add support for textDocument/typeDefinition
  - PR by @rudiejd in https://github.com/razzmatazz/csharp-language-server/pull/123

## [0.10.0] - 2023-11-04 / Seirijai
* Upgrade dependencies: Roslyn, ICSharpCode.Decompiler, Microsoft.Build;
* Fix for a crash in serverEventLoop
  - By @kstatz12 in https://github.com/razzmatazz/csharp-language-server/issues/113
* Possible fixes to https://github.com/razzmatazz/csharp-language-server/issues/62 
and https://github.com/razzmatazz/csharp-language-server/issues/57
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/112
* Fix issues with code actions and other functionality that was
broken after upgrade to recent roslyn version.
  - Fixed by Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/111

## [0.9.0] - 2023-08-12 / Rūdninkai
* Make --solution cmd line param take effect even when the editor provides us
with `csharp.` settings;
  - By @vytautassurvila in https://github.com/razzmatazz/csharp-language-server/pull/105
* Upgrade dependencies: Roslyn, ICSharpCode.Decompiler, Microsoft.Build;
* Support formatting options
  - By @weirongxu in https://github.com/razzmatazz/csharp-language-server/pull/94

### More about Rūdninkai, Lithuania
- [Rūdninkai on WP](https://lt.wikipedia.org/wiki/R%C5%ABdninkai)
- [Google Images on Rūdninkai](https://www.google.com/search?tbm=isch&q=rudninkai+lithuania)

## [0.8.0] - 2023-05-06 / Varėna
* Add more symbols to documentSymbols & codeLens
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/87
* Support type and call hierarchy 
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/74
* Semantic token types fix
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/84 
* Fix crash if there is no newline at the end of the last line
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/83 

### More about Varėna, Lithuania
- [Varėna on WP](https://en.wikipedia.org/wiki/Var%C4%97na)
- [Google Images on Varėna](https://www.google.com/search?tbm=isch&q=varena+lithuania)

## [0.7.1] - 2023-04-08 / Igliauka
* Avoid a crash when client does not signal textDocument/publishDiagnostics cap
  - Reported and fixed by @sharpSteff in https://github.com/razzmatazz/csharp-language-server/pull/77

### More about Igliauka, Lithuania
- [Google Images on Igliauka](https://www.google.com/search?tbm=isch&q=igliauka)

## [0.7.0] - 2023-03-25 / Simnas
* Update Roslyn libs to 4.6.0-1.final, Microsoft.Build* to 17.5.0
* Finish initialization early and load solution/project in background
  - Previous behaviour was causing problems in eglot (emacs) where client has a timeout set for server initialization
* Add support for server configuration via LSP:
  - `csharp.solution` - solution file to load
* Semantic token improvements
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/70
* Inlay hint support
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/71 
  and https://github.com/razzmatazz/csharp-language-server/pull/73
* Remove timeout on handler for `codelens/resolve` -- that wasn't a good idea to begin with

### More about Simnas, Lithuania
- [Simnas on WP](https://en.wikipedia.org/wiki/Simnas)
- [Google Images on Simnas](https://www.google.com/search?tbm=isch&q=simnas)

## [0.6.1] - 2023-02-26 / Marcinkonys
* Fixes for Windows where file:// URIs were not parsed/built properly from native filenames before;
  - Reported by @blasco and @gorsheninmv in https://github.com/razzmatazz/csharp-language-server/issues/57
* Upgrade Roslyn to 4.5.0-2.final (from 4.4.0);
* Semantic token support
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/68
* Fix an issue where some of initial LSP requests deadlock.

### More about Marcinkonys, Lithuania
- [Google Images on Marcinkonys](https://www.google.com/search?tbm=isch&q=marcinkonys)
- [Čepkeliai Marsh](https://en.wikipedia.org/wiki/%C4%8Cepkeliai_Marsh)

## [0.6.0] - 2022-12-30 / Alytus
* Improve scheduling of background diagnostics processing;
* Support for the .NET SDK 7.0;
  - Note that there was an issue with .NET SDK 7 previews breaking SDK, you may need to do some cleanup, see:
    - https://github.com/dotnet/sdk/issues/28947
* csharp-ls will now require .NET 7 SDK
  - Thanks to @Decodetalkers in https://github.com/razzmatazz/csharp-language-server/pull/56
* Fixes for the `textDocument/prepareRename` request;
* Upgrade Microsoft.Build + Roslyn libs.

### More about Alytus, Lithuania
- [Google Images on Alytus](https://www.google.com/search?tbm=isch&q=alytus+lithuania)
- [Alytus on WP](https://en.wikipedia.org/wiki/Alytus)

## [0.5.7] - 2022-10-09 / Marijampolė
* Add support for `textDocument/prepareRename` request;
* Upgrade Microsoft.Build + Roslyn libs;
* Update `textDocument/documentSymbol` handler:
  - emit hierarchical symbol tree, where client supports it;
  - emit `DocumentSymbol[]` instead of `SymbolInformation[]`, the former of which has more info.

### More about Marijampolė, Lithuania
- [Google Images on Marijampolė](https://www.google.com/search?tbm=isch&q=marijampole+lithuania)
- [Marijampolė on WP](https://en.wikipedia.org/wiki/Marijampol%C4%97)

## [0.5.6] - 2022-09-05 / Birštonas
* Fix an issue where we would fail to generate code action list if one of code action providers would raise an exception; -- log the message to the log instead and continue with the next provider to be more resilent;
* Upgrade Microsoft.Build libs;
* Inject required roslyn WorkspaceServices so more refactorings work now:
  - pull member;
  - move static members to another class;
  - generate overrides;
  - others;
* Fix an issue where a new file was added to wrong project.

### More about Birštonas, Lithuania
- [Google Images on Birštonas](https://www.google.com/search?tbm=isch&q=birstonas+lithuania)
- [Birštonas on WP](https://en.wikipedia.org/wiki/Bir%C5%A1tonas)

## [0.5.5] - 2022-08-23 / Prienai
* Fix intermittent server crashes after upgrading to latest Ionide.LanguageServerProtocol:
  - https://github.com/razzmatazz/csharp-language-server/issues/44
  
### More about Prienai, Lithuania
- [Google Images on Prienai](https://www.google.com/search?tbm=isch&q=prienai+lithuania)
- [Prienai on WP](https://en.wikipedia.org/wiki/Prienai)

## [0.5.4] - 2022-08-20 / Šilavotas
* Upgrade roslyn libs to 4.4.0-1.final;
* Properly format + "localize" symbol names in `textDocument/documentSymbol` response;
  - Reported by @joefbsjr in https://github.com/razzmatazz/csharp-language-server/issues/42
* Properly format ITypeSymbol (structs/enums/classes) when displaying type info on `workspace/symbol` and other LSP requests;
  - Reported by @joefbsjr in https://github.com/razzmatazz/csharp-language-server/issues/41 
* Load solution-in-sync when initializing. This will help "server initializing" notification work better for clients that depend on `initialize` request not to complete until the server/solution is properly loaded initialized.
  - Reported by @joefbsjr in https://github.com/razzmatazz/csharp-language-server/issues/40

### More about Šilavotas, Lithuania
- [Kitoks kaimas Prienų rajone](https://www.lrytas.lt/kultura/istorija/2021/08/07/news/kitoks-kaimas-prienu-rajone-kurio-neisdraske-net-sovietu-valdzia-bendruomenes-nariais-tampa-ir-kaunieciai-20347142)

## [0.5.3] - 2022-07-29 / Daugai
  https://www.google.com/search?tbm=isch&q=daugai+lithuania
* Fix (hopefully) how timeout on "textDocument/codeLens" works -- an empty result is returned now, and not as a request cancellation;
* Fix how request cancellation is signalled to down the pipes to StreamJsonRpc/Ionide.LSP -- we will unwrap AggregateException to expose TaskCanceledException:
  - see https://github.com/razzmatazz/csharp-language-server/issues/32 reported by @vytautassurvila;
* Add a fix for some editor where special urls on file path, like '#' were not handled properly:
  - PR https://github.com/razzmatazz/csharp-language-server/pull/38 by @tcx4c70

### More about Daugai, Lithuania
- [Google Images on Daugai](https://www.google.com/search?tbm=isch&q=daugai+lithuania)

## [0.5.2] - 2022-07-02 / Luksnėnai
* Update Roslyn, MSBuild and ICSharpCode.Decompiler;
* Fix how "unnecesary using directive" is exposed, hopefully:
  - fixes https://github.com/razzmatazz/csharp-language-server/issues/35 by @Decodetalkers;
* Expose actual csharp compiler diagnostics ids (e.g. CS0117) for nicer diagnostics messages.

## [0.5.1] - 2022-05-19 / Straigiškė 
- Fix another long-standing bug with incremental document synchronisation;
  - very visible on nvim but affects all editors/clients;
  - reported by @Decodetalkers https://github.com/razzmatazz/csharp-language-server/issues/31;
- Improve textDocument/completion:
  - provide `CompletionItem.FilterText` and `CompletionItem.SortText`;

## [0.5.0-Lazdijai]
- Implement textDocument/signatureHelp;
  - https://github.com/razzmatazz/csharp-language-server/issues/30

## [0.4.3-Meteliai]
- change `CodeAnalysis.AddImport` kind to `quickfix`
  - this will show add missing imports in vscode's quick fix panel;
- don't run pending read/only requests until previous r/w one is processed;
  - this should fix https://github.com/razzmatazz/csharp-language-server/issues/28 reported by @vytautassurvila

## [0.4.2-Randamonys]
- Don't register capabilities when initialized, but after "initialized" has been received from the client;
  - this makes us more LSP-spec compatible;
  - noted by @vytautassurvila in https://github.com/razzmatazz/csharp-language-server/issues/26

## [0.4.1-Liškiava]
- Revert Roslyn update, broke some of the stuff with now gain;
- Fix an issue where some of the assemblies cannot be inspected with go-to-definition from metadata;
  - https://github.com/razzmatazz/csharp-language-server/issues/22
- Try harder to match method by arity when doing "go-to-definition" on metadata;
  - e.g. `Console.WriteLine()` should now open the correct definition as it is single matching
    method `WriteLine` on `Console` with arity of 0;

## [0.4.0-Druskininkai]
- Update Roslyn and ICSharpCode libs;
- Observe and update solution when unopen files are changed/added/removed;
  - this should reduce the need to restart lsp workspace often after adding/removing files or after git branch switches;
- Reload solution on .sln or .csproj change;
- Change how diagnostics are calculated (again) so we sent it for all the documents that we know are open, once a random file has been modified, in the descending order that they were touched.

## [0.3.0]
- Run diagnostics resolution outside the main state actor so we don't lock up other processing;
- Add timeout for codelens requests to avoid excessive CPU usage as that is prone to run for a long time; 
- Really fix write-request serialization;
- Revert the emacs-29 fix, didn't do much.

## [0.2.1]
- Carefully observe incoming requests from StreamJsonRpc to actually serialize write-access to solution state; 
  - Should fix sync issues (another attempt..)
- Attempt to fix an issue with emacs-29 by setting `bufferSize` in `Console.OpenStandardInput` call.

## [0.2.0]
- Support for document formatting, courtesy of @kstatz12;
- Fix `textDocument/didOpen` handler:
  - we will now refresh roslyn solution with file contents as reported by the editor, as our state could have been outdated since;
- Fix sync issues by serializing writes to server state;
- Implement incremental file change sync to improve performance for large files;
- Handle $/cancellationRequest's to handle cancellation requests properly.

## [0.1.9]
- Refactor server state tracking to (hopefully) fix some issues with document sync/versioning;
- Fix, hopefully in the final form, how symbols are formatted by using roslyn To*DisplayString() methods;
- Don't show symbol attributes on textDocument/documentSymbol response;

## [0.1.8]
- Implementation for code lens that shows number of references;
  - Needs client implementation of `workspace/executeCommand`: `csharp.showReferences`;
- Nicer on-hover markdown that should match the context better;
- Expose properties & events on textDocument/documentSymbol;
- Pull `add using import` code action to the top of action list, mark it prefered and with `Kind`, [csharp-language-server#9](https://github.com/razzmatazz/csharp-language-server/issues/9). 

## [0.1.7]
- Bump roslyn libs to a newer version;
- Add support for `textDocument/implementation`;
- Expose more bits from xmldoc properly on tip on hover;
- Drop local LanguageServerProtocol clone from FSAC and use [Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol) instead;
- Fix file creation via code actions (e.g. when extracting an interface or inner class to another file);
- List params and exceptions in the same order as they appear on docxml on the symbol (instead of random order) -- [csharp-language-server#6](https://github.com/razzmatazz/csharp-language-server/issues/6);
- Disable incorrectly implemented `textDocument/implementation` provider [csharp-language-server#4](https://github.com/razzmatazz/csharp-language-server/issues/4);

## [0.1.6]
- Update roslyn+msbuild libs to support for `net6.0` projects;
- Use .NET 6.0 for runtime;
- Fix `workspace/symbol` to return correct locations;
- Implement `textDocument/implementation` (alias to `textDocument/definition` actually);
- Support for `codeAction/resolve` so we have faster code action resolution/less overhead when working on large solutions.

## [0.1.5]
- Enable code actions that are nested, like "generate field/property/ro-field/ro-property";
- Update how tooltip on hover is formatted; should be much nicer;
- Fix how adding new file works -- don't write unnecesary `<Compile>` when a new file is added and trigger diagnostics update immediately.

## [0.1.4]
- Don't reference NuGet.* packages as those bind to particular(?) sdk -- things work better now, as it was not working for me with updated SDK dependencies;
- Update roslyn dependencies;
- Adding new file works (i.e. no need to restart workspace anymore);
  - There is an issue that Roslyn adds (unnecessary) `<Compile>` to the project;
- Don't highlight namespace symbols as those can cover all the symbols on the source file and that is not useful;
- Add -l/--loglevel <debug|info|warning|error> param;
- Expose C# docxml as markdown for more proper on-hover experience (still needs more love, FsAutoComplete does it beautifully);
- Don't publish diagnostics for C# documents generated from metadata.

## [0.1.3]
- Support `--help` and `--version` command line params.
- Support for `-s/--solution` param to load specific solution file.

## [0.1.2]
- No -alpha version number suffix so you don't need --version to install with `dotnet tool install`;
- Don't return from `initialize` until we actually load the solution;
- Report csharp-ls version number to the client.

## [0.1.1-alpha]
Initial version:
- Most of basic LSP features work: rename/go-to-def/find references, etc;
- Support for textDocument/documentHighlight;
- Go-to-metadata (needs integration from your LSP client;)
- Published as `dotnet tool csharp-ls`
