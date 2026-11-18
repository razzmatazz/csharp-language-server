# Releasing a new version

The package version is not set in any project file: `Ionide.KeepAChangelog.Tasks` reads it
from the top `## [X.Y.Z]` heading of `CHANGELOG.md`, and `Directory.Build.props` takes the
codename after the `/` into `InformationalVersion` (e.g. `0.29.0 (Merkinė)`). If the top
heading is `## [Unreleased]`, the build falls back to version `1.0.0` — so the release
commit must not have an `[Unreleased]` heading above the new version.

1. In `CHANGELOG.md`, rename the `## [Unreleased]` heading to
   `## [X.Y.Z] - YYYY-MM-DD / Codename` (codename: a Lithuanian place name not used by an
   earlier release), and end the section with
   `**Full Changelog**: https://github.com/razzmatazz/csharp-language-server/compare/<previous>...X.Y.Z`.
2. Check the version the build will pick up:
   ```
   dotnet msbuild src/CSharpLanguageServer/CSharpLanguageServer.fsproj -t:SetVersionFromChangelog -getProperty:Version -getProperty:InformationalVersion
   ```
3. Commit, tag (lightweight, bare version — no `v` prefix) and push:
   ```
   git commit -am "csharp-ls: X.Y.Z / Codename"
   git tag X.Y.Z
   git push origin main X.Y.Z
   ```
4. Run the `Build` workflow (`.github/workflows/build.yaml`, manual `workflow_dispatch` only —
   pushing a tag does not trigger anything) on `main` to produce the `.nupkg`, uploaded as the
   `packages` artifact:
   ```
   gh workflow run build.yaml --ref main
   ```
5. Create the GitHub release for tag `X.Y.Z`, titled with the changelog heading
   (`[X.Y.Z] - YYYY-MM-DD / Codename`), with the changelog section as its notes.
6. Add a fresh `## [Unreleased]` heading to `CHANGELOG.md` only once new changes land.
