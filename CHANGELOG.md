# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]
* Finish initialization early and load solution/project in bacground. Previous behaviour was causing problems on eglot where client has a timeout for initialization
* Add support for server configuration via LSP:
  - `csharp.solution` - solution file to load
* Semantic token improvements
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/70
* Inlay hint support
  - By Adam Tao @tcx4c70 in https://github.com/razzmatazz/csharp-language-server/pull/71 
  and https://github.com/razzmatazz/csharp-language-server/pull/73
* Remove timeout on 'codelens/resolve' handler -- that wasn't a good idea to begin with

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
