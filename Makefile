TEST_PROJECT := tests/CSharpLanguageServer.Tests

# The test project is being migrated from NUnit to Expecto one file at a time (see
# CONTRIBUTING.md's "Gradual Expecto migration"). Both frameworks' tests live in the same
# assembly and are both discovered by the YoloDev.Expecto.TestSdk / NUnit3TestAdapter
# VSTest adapters, so `test-nunit` must exclude every Expecto-migrated test list by name
# (its `[<Tests>]` testList name) to avoid running it twice: once here via VSTest, once in
# `test-expecto` via Expecto's own runner.
#
# Kept as a single literal (rather than built up from a list) because VSTest's --filter
# operand joining (via `&`) behaved inconsistently across `make` versions/platforms when
# generated with $(subst)/$(foreach) — see git history. Append `&FullyQualifiedName!~<Name>`
# whenever another file is ported to Expecto.
EXPECTO_EXCLUDE_FILTER := FullyQualifiedName!~InternalTests

# Extra args appended to the `dotnet test` / `dotnet run` invocations below, e.g. from CI
# to add `--no-build`, blame-hang options, a --results-directory, or (after a literal `--`)
# NUnit/Expecto-specific runner options. Left empty for a plain local run.
DOTNET_TEST_ARGS ?=
DOTNET_RUN_ARGS ?=
EXPECTO_ARGS ?= --summary

.PHONY: build test test-nunit test-expecto

build:
	dotnet build

## Run the NUnit suite via `dotnet test` (VSTest adapter), excluding test lists already
## migrated to Expecto (see EXPECTO_TEST_LISTS above).
test-nunit:
	dotnet test $(TEST_PROJECT) --filter "$(EXPECTO_EXCLUDE_FILTER)" $(DOTNET_TEST_ARGS)

## Run the Expecto-migrated suites via Expecto's own test runner instead of VSTest, for
## increased test concurrency. Expecto discovers every `[<Tests>]`-tagged value in the
## assembly by reflection, so this only ever exercises Expecto tests (NUnit `[<Test>]`
## functions are invisible to it) — see tests/CSharpLanguageServer.Tests/Program.fs.
test-expecto:
	dotnet run --project $(TEST_PROJECT) $(DOTNET_RUN_ARGS) -- $(EXPECTO_ARGS)

## Run both suites, one after the other: NUnit via VSTest first, then Expecto via its own
## (more concurrent) runner.
test: test-nunit test-expecto
