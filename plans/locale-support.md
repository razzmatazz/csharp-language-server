# Plan: Add Locale / UI Language Support

## Motivation

The server currently emits diagnostics and log messages in the OS system locale.
Users want to force a consistent language (usually English) for:

- Consistent logs/diagnostics across different OS locales
- Easier searching/copy-paste of errors in issues, docs, and team chats
- Aligning with common practice of forcing English output in tooling

The current workaround is to manually delete the locale satellite-assembly
subdirectory from the tool installation folder.

---

## Background: How .NET Locale Control Works

Two thread-static properties control all resource-string lookups and formatting:

- `CultureInfo.DefaultThreadCurrentUICulture` — controls which satellite assembly
  is used for resource strings (Roslyn diagnostics text, MSBuild messages, etc.)
- `CultureInfo.DefaultThreadCurrentCulture` — controls number/date formatting

Both must be set **before** any Roslyn or MSBuild work begins. Setting them to
`CultureInfo.InvariantCulture` (or `en-US`) replicates the effect of deleting
the locale subdirectory.

The dotnet SDK and MSBuild already honour this environment variable for this:

| Variable | Notes |
|---|---|
| `DOTNET_CLI_UI_LANGUAGE` | Standard dotnet CLI convention |

---

## Proposed Priority Order (highest → lowest)

1. `--locale` CLI argument
2. `DOTNET_CLI_UI_LANGUAGE` environment variable
3. *(none — system default unchanged)*

---

## Implementation Steps

### 1. `src/CSharpLanguageServer/Types.fs`

- Add `locale: string option` field to the `CSharpConfiguration` record.
- Update `mergeCSharpConfiguration` to merge the new field (new value wins).

### 2. `src/CSharpLanguageServer/Program.fs`

- Add `| Locale of locale: string` case to the `CLIArguments` discriminated union.
- Register `--locale / -L` with Argu (description: "force output locale, e.g. en-US").
- In the entry point, resolve locale with the priority order above:
  - CLI arg → `DOTNET_CLI_UI_LANGUAGE`
- If a locale is resolved, call `CultureInfo.GetCultureInfo(locale)` and assign to
  both `CultureInfo.DefaultThreadCurrentCulture` and
  `CultureInfo.DefaultThreadCurrentUICulture` before any other work.
- An empty string value should map to `CultureInfo.InvariantCulture` (English fallback).
- Store the resolved locale in `CSharpConfiguration.locale`.

### 3. `src/CSharpLanguageServer/Runtime/ServerStateLoop.fs`

- In the `SettingsChange` event handler, check if `Config.locale` has changed.
- If it has, re-apply `CultureInfo.DefaultThreadCurrentCulture` /
  `DefaultThreadCurrentUICulture` to the new value (or revert to the original
  system culture if `None`).
- This allows a live workspace setting (`csharp.locale` in `settings.json`) to
  take effect without restarting the server.

### 4. `README.md`

- Add `csharp.locale` row to the Settings table:
  `(system locale)` default, description: "Force output language, e.g. `en-US`".
- Add `--locale / -L` row to the CLI Arguments section.
- Add a short Environment Variables section noting `DOTNET_CLI_UI_LANGUAGE`.

---

## Testing

### 1. `--diagnose` smoke test (manual / CI)

`--diagnose` runs the full MSBuild + solution-load path without needing an LSP
client. It is the cheapest way to verify that setting a locale does not break
startup:

```sh
# Force English output
DOTNET_CLI_UI_LANGUAGE=en-US csharp-ls --diagnose

# Via CLI flag
csharp-ls --locale en-US --diagnose

# Invariant (English fallback)
DOTNET_CLI_UI_LANGUAGE="" csharp-ls --diagnose
```

A successful exit code (`0`) confirms the culture was applied without crashing.
To verify the language is actually changed, pass `de` (German) and compare
diagnostic text — the Roslyn satellite assemblies for `de` ship inside the build
output unconditionally, so this works on any machine regardless of OS locale.

### 2. Integration test: `LocaleTests.fs`

Add `tests/CSharpLanguageServer.Tests/LocaleTests.fs` with NUnit tests that
exercise the feature end-to-end via the existing `LspTestClient` harness.

The test harness spawns the real server binary as a child process via
`makeServerProcessInfo`. It does **not** set any custom environment variables on
the `ProcessStartInfo` today — child processes inherit the test runner's
environment wholesale. To test locale isolation we need to set env vars on the
`ProcessStartInfo` before starting the server, or pass `--locale` via
`Arguments`.

The Roslyn NuGet packages ship satellite assemblies for 13 locales (`cs`, `de`,
`es`, `fr`, `it`, `ja`, `ko`, `pl`, `pt-BR`, `ru`, `tr`, `zh-Hans`, `zh-Hant`)
and these are copied into the build output directory unconditionally. **This
means tests can force `de` locale and expect German diagnostic text even on a
stock en-US CI runner** — no special OS configuration is needed.

Note: Roslyn uses neutral culture codes (`de`, not `de-DE`). The tests must pass
`"de"` to `CultureInfo.GetCultureInfo` (and to `DOTNET_CLI_UI_LANGUAGE`).

#### Required harness change — `Tooling.fs`

Extend `ClientProfile` (or the `makeServerProcessInfo` helper) to accept an
optional `extraEnv: Map<string, string>` and optional `extraArgs: string list`.
In `makeServerProcessInfo`, apply them:

```fsharp
for KeyValue(k, v) in extraEnv do
    processStartInfo.Environment[k] <- v
processStartInfo.Arguments <- "--features razor-support " + String.concat " " extraArgs
```

#### Test cases in `LocaleTests.fs`

| Test | Mechanism | What is asserted |
|---|---|---|
| `testLocaleEnvVar` | Set `DOTNET_CLI_UI_LANGUAGE=de` in server env | Diagnostic message contains German wording (e.g. `"Der Typ- oder Namespacename"`) |
| `testLocaleCliArg` | Pass `--locale de` in `Arguments` | Same assertion |
| `testLocaleCliArgTakesPriority` | Set `DOTNET_CLI_UI_LANGUAGE=de`, pass `--locale en-US` | `--locale` wins — diagnostic text is English |
| `testLocaleSetting` | Send `workspace/didChangeConfiguration` with `{ csharp: { locale: "de" } }` after server is up | Subsequent diagnostics contain German wording |

For the diagnostic text assertions, use the `testDiagnosticsWork` fixture (a
project with deliberate C# errors). Pull diagnostics via `textDocument/diagnostic`
or read from `PushDiagnostics` state — both already used in `DiagnosticTests.fs`.

Because the `de` satellite assemblies are present in the build output on every
platform, all four tests are reliable on en-US CI runners without any special
setup — no caveat needed.

---

## Files Changed Summary

| File | Change |
|---|---|
| `Types.fs` | Add `locale` field + merge logic |
| `Program.fs` | Add CLI arg, env-var cascade, apply culture at startup |
| `Runtime/ServerStateLoop.fs` | Re-apply culture on live settings change |
| `README.md` | Document new setting, CLI flag, and env vars |
| `tests/…/Tooling.fs` | Extend `ClientProfile` / `makeServerProcessInfo` with `extraEnv` + `extraArgs` |
| `tests/…/LocaleTests.fs` | New test file with 5 integration tests (see above) |
