# Reducing Superfluous Inlay Hints

**Status:** Design not started — this file is a placeholder to hold scope and candidate ideas
until the exact suppression rules are worked out.

**Affects:** `textDocument/inlayHint` — `Handlers/InlayHint.fs`, primarily the `toInlayHint`
function.

---

## Problem Statement

`InlayHint.fs` currently emits two kinds of hints with essentially no "is this hint actually
useful to the reader" heuristics:

- **Type hints** (`InlayHintKind.Type`) — for `var` declarations, deconstruction designations,
  `foreach` loop variables, implicit lambda parameters, and `new(...)` implicit object creation.
  The only filtering applied today is `validateType`, which suppresses a hint solely when the
  type fails to resolve (`IErrorTypeSymbol`) or is literally named `"var"`. It does **not**
  consider whether the type is already obvious from the surrounding code.
- **Parameter name hints** (`InlayHintKind.Parameter`) — for positional call-site arguments.
  The only filtering applied today is `validateParameter`, which suppresses a hint for indexers,
  for parameters with an empty name, or when the argument text already matches the parameter
  name case-insensitively.

Both kinds of hints can currently be shown in situations where the information they convey is
redundant, adding visual noise without value. The existing TODO comment in the file already
flags this generally:

> `// TODO: Support the configuration whether or not to show some kinds of inlay hints.`

## Goal

Suppress inlay hints in cases where they are superfluous — i.e. where the hidden information is
already apparent from context — while keeping hints in the cases where they genuinely aid
readability (untyped literals, ambiguous overloads, non-obvious inferred types, etc.).

## Explicitly Out of Scope (for now)

The **exact suppression rules** (which syntactic/semantic patterns count as "apparent from
context", for both type hints and parameter hints) are **not decided yet** and will be defined in
a follow-up design pass. This file exists to track the goal and hold candidate ideas gathered so
far; it should be expanded into a full design (with a rule table and implementation plan, mirroring
the style of `plans/analyzer-support.md`) before implementation starts.

## Candidate Ideas Gathered So Far (unconfirmed, subject to change)

These are starting points for the design discussion, not a final rule set.

### Type hints — suppress when type is apparent from the initializer

For `var` declarations where the right-hand side already makes the type obvious to the reader,
the type inlay hint may be redundant:

```csharp
var someValue = getIntAbove10();  // showing `: int` here may add no value
```

Roslyn's own `CSharpInlineTypeHintsService` has a similar suppression heuristic worth reviewing as
prior art, keyed off the shape of the initializer expression, e.g.:

- `InvocationExpressionSyntax` — method/delegate call (`getIntAbove10()`)
- `ObjectCreationExpressionSyntax` — `new Foo(...)`
- `CastExpressionSyntax` — `(int)x`
- `LiteralExpressionSyntax` — `42`, `"hi"`, `true`, `null`
- `DefaultExpressionSyntax` — `default(T)` / `default`
- `TypeOfExpressionSyntax` — `typeof(T)`
- `SizeOfExpressionSyntax` — `sizeof(T)`
- `BinaryExpressionSyntax` with `AsExpression` kind — `expr as T`
- `ParenthesizedExpressionSyntax` — recurse into inner expression

Whether all, some, or none of these should suppress the hint (and whether the same treatment
should extend to `DeclarationExpressionSyntax`, `SingleVariableDesignationSyntax`, and
`ForEachStatementSyntax` initializers) is an open design question — Roslyn's IDE hints and an LSP
client's needs are not necessarily identical (e.g. terminal/TUI clients without hover support may
want more hints kept, not fewer).

### Parameter hints — other possible redundancy cases to consider

Not yet investigated in detail, but worth putting on the table for the design pass:

- Single-parameter calls where the parameter name duplicates the method name or is otherwise
  low-information (e.g. `value`, `obj`).
- Calls where every argument already gets a hint (dense hinting) vs. only hinting "surprising"
  positions (e.g. boolean literals, which Roslyn's own heuristics treat specially).

## Configuration Angle

A related (but separate) axis is **making hint kinds configurable** (per the existing TODO
comment), so users/editors can opt in/out of type hints, parameter hints, or specific sub-cases
independently of any built-in suppression heuristics. Whether this ships alongside or instead of
built-in suppression heuristics is also to be decided during design.

## Next Steps

1. Design session to enumerate the exact suppression rules per hint kind (type vs. parameter),
   informed by the candidates above and by user/editor feedback on which current hints feel
   noisy in practice.
2. Turn the agreed rules into a rule table (condition → suppress/keep) similar in spirit to the
   `validateType` / `validateParameter` predicates already in `InlayHint.fs`.
3. Implement the agreed predicates in `toInlayHint`, with unit/integration test coverage for each
   rule (positive case: hint suppressed; negative case: hint still shown for the non-redundant
   variant).
4. Decide on and (if wanted) implement the configuration angle above.

## Files Likely Touched

| File | Likely Change |
|------|----------------|
| `src/CSharpLanguageServer/Handlers/InlayHint.fs` | Add suppression predicates to `toInlayHint`'s match arms, per the design once finalized |
| `tests/CSharpLanguageServer.Tests/InlayHintTests.fs` (if present) or a new test file | Add positive/negative coverage per rule |
