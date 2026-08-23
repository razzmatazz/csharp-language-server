/// Custom entry point used only when running Expecto tests directly via `dotnet run`
/// (or the built executable), bypassing VSTest entirely.
///
/// Expecto discovers every `[<Tests>]`-tagged value in this assembly by reflection, so
/// NUnit `[<Test>]` functions are invisible to it — running this entry point only ever
/// exercises the Expecto-migrated suites (see CONTRIBUTING.md's "Gradual Expecto
/// migration" section). This is separate from, and complementary to, running the same
/// Expecto tests through the YoloDev.Expecto.TestSdk VSTest adapter via `dotnet test`;
/// running them here lets Expecto use its own (more concurrent) test runner instead of
/// going through VSTest orchestration. See `make test-expecto` in the Makefile.
module CSharpLanguageServer.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [] argv
