# Reducing Superfluous Inlay Hints

**Status:** Full design/rule-table pass for the remaining candidates still not started, but
informed by six real-world samples (an MVC controller, two large business-logic "ops" classes, a
companion pair of MVC controller files, a small API controller, and a diverse batch spanning a
controller/file-I/O utility/Win32 interop/message handler) — see the "Real-World Evidence" sections
below for concrete label-frequency data pulled from a live `csharp-ls-rpc.log` session, plus a
source-level root-cause diagnosis of the same-name suppression gap (from reading `InlayHint.fs`
directly).

Implementation has started against the "Net takeaway" priority ranking below, backed by a new
`InlayHintTests.fs` (+ dedicated `Fixtures/genericProject/Project/InlayHintTest.cs` fixture, safe
to keep extending for further rules without touching other tests):

- ✅ **Rule #1 (same-name suppression gap)** — both sub-fixes landed in `validateParameter`:
  comparing against the argument expression's significant (trivia-free) identifier text instead
  of `arg.GetText()`, and matching the last identifier segment of a qualified member-access
  expression (`this.Foo` vs. parameter `foo`).
- ✅ **Rule #2 (short / numbered-suffix parameter names)** — `hasUninformativeParameterName`
  suppresses hints for parameter names of length ≤ 2 or matching `^\w*?\d+$` (`arg0`, `val1`,
  `path2`, …), with negative-control test coverage for `string.Substring(startIndex, length)` and
  a `Stream.Write`-shaped `(buffer, offset, count)` method to confirm it doesn't over-suppress.
  Deliberately narrower than the original per-method-allow-list idea (it does *not* catch
  `Math.Round`'s `decimals`/`mode`, only `d`) — see the rule's doc comment in `InlayHint.fs` for
  the rationale.
- ✅ **Rule #3 (single lambda-argument calls)** — `validateParameter` now suppresses the hint for
  the sole (effective) argument of a call when that argument is a lambda expression, regardless of
  the parameter's own name — covers both LINQ (`Where(predicate)`, `Select(selector)`, …) and other
  fluent/ORM APIs (NHibernate-style `Fetch(relatedObjectSelector)`/`ThenFetch(...)`). Deliberately
  scoped to single-argument calls (multi-lambda-argument calls like `Combine(first, second)` keep
  both hints) and to lambda expressions specifically (a method-group argument, e.g.
  `Where(IsPositive)`, is out of scope and keeps its hint) — both scoping decisions are covered by
  dedicated negative-control tests. Refined so a trailing `CancellationToken` argument doesn't
  count towards "effective argument count" (`isCancellationTokenArgument`, matched by name only) —
  e.g. `WhereAsync(x => x.IsActive, cancellationToken)` is treated the same as
  `Where(x => x.IsActive)`; a two-lambda call with a trailing token (`CombineAsync(first, second,
  cancellationToken)`) still correctly keeps both hints.
- ✅ **Rule #4 (composite-format-string positional arguments)** — investigated and closed out with
  **no separate implementation**: confirmed via a test mirroring log4net's real
  `ILog.DebugFormat(string format, object arg0, object arg1, object arg2)` overload shape that
  `arg0`/`arg1`/`arg2` are already suppressed by rule #2's numbered-suffix pattern
  (`hasUninformativeParameterName`), while `format:` (not numbered-suffix) correctly stays. The
  broader originally-proposed heuristic (parse the format string's `{n}` placeholders) turned out
  to be unnecessary for the evidence gathered.
- ✅ **Rule #5 (generic-invocation-redundant `var` type hints)** — `validateType`'s result is now
  filtered by `isTypeSpelledOutInGenericInvocation`, which suppresses the `var` type hint when the
  initializer is a generic invocation (qualified, e.g. `Enum.Parse<T>()`, or unqualified, e.g. a
  local `CreateLocal<T>()`) whose explicit type-argument list contains a type symbol-equal to the
  inferred variable type. Compares by resolved symbol rather than by name, so it correctly does
  *not* fire when the invocation's return type differs from its type argument (e.g. a hypothetical
  `Describe<Widget>(...)` returning `string` keeps its `: string` hint). Covered by a test using
  the plan's own real BCL example, `Enum.Parse<DayOfWeek>(...)`.
- ✅ **Rule #6 (identifier-echoes-type-name generalization)** — new `identifierEchoesTypeName`
  helper (full case-insensitive match, or last-camelCase-word match via `lastWord`) wired into
  both sides: the `VariableDeclarationSyntax`, `DeclarationExpressionSyntax`,
  `SingleVariableDesignationSyntax`, and `ForEachStatementSyntax` type-hint match arms (suppress
  when the declared identifier echoes the inferred type, e.g. `resourceCondition`/
  `settingChangeCondition` vs. `ResourceCondition`), and a new `validateParameter` match arm (the
  "narrower, safer sibling" from the evidence: suppress a parameter-name hint when the parameter's
  own name is a decapitalized echo of its own declared type, e.g. a `MessageDispatcherAsync
  messageDispatcherAsync` parameter). Both sides covered by matching negative-control tests where
  the identifier does *not* echo the type.
- ✅ **New rule (generic `obj`-style parameter names)** — prompted by a real `csharp-ls-rpc.log`
  example, `NHibernate.ISession.SaveAsync(object obj, CancellationToken cancellationToken =
  default)` called with a single explicit argument. Neither rule #1 (argument name doesn't match
  `obj`) nor rule #2 (`obj` is 3 characters, over the ≤2 mechanical cutoff, and not a
  numbered-suffix shape) caught it. Added a small, explicit, easy-to-extend
  `genericParameterNames` set (currently just `{ "obj" }`) folded into
  `hasUninformativeParameterName`, rather than broadening the mechanical length/regex checks
  (which risked over-suppressing other short-but-meaningful 3-letter names). Covered by a positive
  test (`Save(object obj)`) and a negative control confirming a different, coincidentally
  same-length parameter name (`Log(string msg)`) still keeps its hint.
- ✅ **New rule (explicit-object-creation-redundant `var` type hints)** — prompted by a second real
  `csharp-ls-rpc.log` example, `var whProd = new DBWarehouseProduction { ... };`, which showed a
  `": DBWarehouseProduction?"` hint even though the type is spelled out immediately after `new` on
  the same line. This is the plain-object-creation sibling of rule #5 (which only handles generic
  invocations' type-argument lists): added `isTypeSpelledOutInObjectCreation`, which suppresses the
  `var` type hint when the initializer is an explicit (non-target-typed, i.e. not `new()`)
  `ObjectCreationExpressionSyntax` whose spelled-out type is symbol-equal to the inferred variable
  type. Deliberately restricted to explicit `new Type(...)`/`new Type { ... }` — target-typed
  `new()` is the genuinely useful case and is handled separately by the existing
  `ImplicitObjectCreationExpressionSyntax` match arm, which is untouched. Covered by tests for both
  the plain-constructor shape (`new Widget()`) and the object-initializer shape (`new
  WidgetWithProperty { Value = 1 }`, mirroring the real `{ Product = ..., Warehouse = ..., ... }`
  shape from the log).
- Rule #7 below is not yet implemented.

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

## Real-World Evidence: Sample MVC Controller

Captured from a local RPC trace of a real editor session against an unrelated private codebase
(details anonymized below; only the structural pattern matters) — the `textDocument/inlayHint`
responses for a stretch of ~150 lines covering five near-identical MVC actions, all following the
same `this.WithResourceAsync(resourceName, async (resourceOps, resource) => { ... })` idiom (a
shared `BaseController` helper that loads a resource by name and hands the caller an ops/context
pair). Label frequency across that single response (~230 hints for ~150 lines of code):

| Label | Count | Kind |
|---|---|---|
| `predicate:` | 58 | Parameter |
| `selector:` | 21 | Parameter |
| `asyncOp:` | 21 | Parameter |
| `: DBEntity` / `: DBEntity?` | 21 | Type |
| `: EntityOps?` | 20 | Type |
| `: ResourceOps` | 19 | Type |
| `: DBResource` | 19 | Type |
| `resourceName:` | 17 | Parameter |
| `: EntityTranslationViewModel` | 16 | Type |
| `value:` | 13 | Parameter |
| `savedTranslations:` / `routeValues:` / `availableLanguages:` / `actionName:` | 12 each | Parameter |
| `: EntityNode` | 12 | Type |
| (long tail of narrower types/params) | ≤10 each | — |

Cross-referencing these against the actual source lines surfaces a few concrete, recurring
patterns — much sharper than the abstract candidates above:

1. **LINQ combinator parameter names dominate the noise (`predicate:` + `selector:` = 79 of
   ~230 hints, >1/3 of the total).** Every `.Where(x => ...)`, `.Select(x => ...)`,
   `.Single(x => ...)`, `.SingleOrDefault(x => ...)` call gets a parameter-name hint whose label
   is generic BCL boilerplate (`predicate`, `selector`) carrying zero information beyond "this is
   a lambda passed to a LINQ method" — which is already obvious from the method name itself
   (`Where` → predicate, `Select` → selector). This is the single biggest win available and maps
   directly to the "low-information parameter name" candidate already in this file, just with
   real numbers behind it now.

2. **The repeated callback-shape idiom re-teaches the same fact every time.** The
   `WithResourceAsync(resourceName, async (resourceOps, resource) => {...})` pattern appears 5
   times in this one file alone (and is presumably repeated across most controllers in the same
   codebase, given the helper lives on a shared base controller). Each occurrence re-hints the
   lambda parameter types (`: ResourceOps`, `: DBResource` — 19 each) and the `asyncOp:` parameter
   name (21 times) identically. There's no per-call variation for a reader to gain from
   repetition — once the shape is known, every subsequent occurrence is pure noise. This is a case
   the current candidate list doesn't call out explicitly: **suppress-on-repetition /
   suppress-for-idiomatic, fixed-signature callbacks**, not just suppress-on-initializer-shape.

3. **Found a likely gap in the existing same-name suppression, not just a design question.**
   `resourceName:` fires 17 times on calls of the shape `WithResourceAsync(resourceName, ...)`.
   Checked the shared helper's actual signature and confirmed the shape is:

   ```fsharp
   public async Task<IActionResult> WithResourceAsync(
       string resourceName,
       Func<ResourceOps, DBResource, Task<IActionResult>> asyncOp,
       ...)
   ```

   The parameter is literally named `resourceName` and every call site passes a local/parameter
   also literally named `resourceName` — an exact, same-case identifier match, which
   `validateParameter`'s existing "argument text matches parameter name case-insensitively" rule
   should already suppress. It doesn't, here. Worth a standalone investigation (likely a separate
   bug fix, independent of the broader suppression-design work) into why this exact-match case
   isn't being caught — e.g. is the argument wrapped in something (implicit conversion node?)
   that defeats the current text-comparison logic in `toInlayHint`/`validateParameter`?

4. **ASP.NET idioms self-describe via call shape.** `actionName:` / `routeValues:` (12 each) hint
   every `RedirectToAction(nameof(SomeAction), new { a, b })` call. The combination of `nameof(...)`
   as first arg and an anonymous object as second arg is an extremely well-known, unambiguous MVC
   idiom — a case where the *shape of a specific, recognizable call pattern* (not just the
   initializer shape of a `var`) makes the hint redundant. Same reasoning likely applies to other
   framework idioms beyond `RedirectToAction` (worth enumerating during design).

5. **Type hints on `var` from method calls are the most defensible of the bunch, but still
   repetitive.** `: DBEntity?`, `: EntityOps?`, `: EntityNode` etc. (all from
   `var x = someCollection.Single(...)` / `.GetChild(...)` style calls) aren't obvious from the
   call alone — you'd need to know the return type of the accessor/the element type of the
   collection. These look like legitimate keeps under the "initializer shape" heuristic
   (`InvocationExpressionSyntax` doesn't make the type obvious here, unlike e.g. `new Foo()`).
   The volume is high mostly *because* the same call idiom (`entity = ...Single(...)`,
   `entityOps = ...GetChild(...)`) is repeated in every action, not because any individual hint
   is wrong.

6. **Misc low-information single-arg parameter names**: `value:` (13), plus a couple of similar
   domain-specific single-arg names each occurring a handful of times — single-parameter
   update/setter-style calls where the parameter name is generic and adds little beyond what the
   call site already conveys positionally.

## Real-World Evidence: Business-Logic ("Ops") Class Sample

A second sample, from a large (~6,500 line) business-logic class in the same private codebase —
this time a plain C# service/"ops" class (no MVC, mostly synchronous/async domain methods,
logging, and validation), sampled across ~1,800 lines via 14 separate `inlayHint` requests,
de-duplicated by position. 366 unique hints total. Top labels:

| Label | Count | Kind |
|---|---|---|
| `format:` | 47 | Parameter |
| `arg0:` | 45 | Parameter |
| `predicate:` | 29 | Parameter |
| `arg1:` | 24 | Parameter |
| `: DBResourceDocument` | 17 | Type |
| `: Timing?` | 15 | Type |
| `name:` | 15 | Parameter |
| `: ResourceStatus` | 11 | Type |
| `message:` | 11 | Parameter |
| `db:` | 11 | Parameter |
| `proxy:` | 11 | Parameter |
| `: DBResourceItem` | 9 | Type |
| `flag:` | 8 | Parameter |
| `arg2:` | 6 | Parameter |
| `selector:` | 6 | Parameter |
| `: ResourceCondition` | 5 | Type |
| (long tail) | ≤4 each | — |

This file surfaces two patterns not visible in the controller sample, plus reinforces two from
before:

1. **Format-string positional-argument hints are the single biggest category here — bigger than
   LINQ names.** `format:` + `arg0:` + `arg1:` + `arg2:` = 122 of 366 hints (~1/3 of the total),
   all from calls of the shape:

   ```csharp
   Logger.DebugFormat(
       "{0}: cannot do the thing; reason",
       nameof(SomeMethodAsync));
   ```

   or

   ```csharp
   throw new ResourceOpException(
       string.Format(
           "{0}: mode could not be set to `{1}`: {2}",
           nameof(this.SetModeAsync), mode, modeChangeCondition));
   ```

   These are arguably the single clearest redundant-parameter-hint case found across both
   samples: the format string itself already spells out the positional correspondence via
   `{0}`, `{1}`, `{2}` placeholders, so labeling the arguments `format:`/`arg0:`/`arg1:`/`arg2:`
   adds strictly zero information beyond what's already visible in the preceding string literal.
   **Candidate rule:** suppress parameter-name hints for arguments following a composite/format
   string parameter on well-known formatting methods (`string.Format`, `*.DebugFormat`,
   `*.InfoFormat`, `*.ErrorFormat`, `string.Join`-adjacent, or more generally any method whose
   preceding string-literal argument contains `{n}`-style placeholders).

2. **New rule candidate: the local variable's own name already spells out the type name.**
   Confirmed concrete cases:

   ```csharp
   var resourceStatus = this.Resource.Status;  // hint: ": ResourceStatus"
   ```

   ```csharp
   public async Task<ResourceCondition> CanSetModeAsync(ResourceMode mode)
   {
       var settingChangeCondition = await this.CanChangeSettingsAsync();  // hint: ": ResourceCondition"
       ...
   ```

   In the first case the variable identifier is a case-insensitive match of the *entire* type
   name (`resourceStatus` ↔ `ResourceStatus`). In the second, the variable name's suffix
   (`...Condition`) already matches the type name (`ResourceCondition`). This is the type-hint
   analogue of the same-name suppression rule `validateParameter` already applies to parameter
   hints — it just hasn't been generalized to `validateType` yet. Given how idiomatic this naming
   style is in this codebase (and probably others — `xyzStatus`/`xyzCondition`/`xyzResult`-style
   locals are a common C# convention), this looks like a high-value, low-risk rule to add
   alongside the LINQ and format-string ones.

3. **The repeated profiling/scope idiom reinforces the earlier "suppress fixed-signature,
   repeated callback/idiom" finding, at even higher volume.** `: Timing?` (15) + `name:` (15) come
   entirely from a one-line profiling-scope idiom (`using var scope = Profiler.Current.Step(nameof(SomeMethodAsync));`)
   that opens nearly every public method in the file. Same shape, same labels, dozens of times —
   arguably the strongest case yet for a "don't re-hint an idiom you've already hinted
   identically N times in this file/session" rule, if such a stateful heuristic is in scope.

4. **Single string-literal exception/log messages (`message:`, 11) reinforce the earlier
   "low-information single-arg parameter name" finding** — e.g. `throw new ResourceOpException("cannot enter status X")`.
   The only sensible parameter for a custom exception's sole string argument is its message; the
   hint adds nothing a reader doesn't already know from the fact that it's a string literal being
   thrown.

## Real-World Evidence: Third Sample (Large Business-Logic Class, ~3,000 Lines)

A third sample, from another large business-logic "ops" class in the same private codebase,
sampled across nearly the entire ~3,000-line file via 46 `inlayHint` requests, de-duplicated by
position: 975 unique hints. Top labels:

| Label | Count | Kind |
|---|---|---|
| `predicate:` | 76 | Parameter |
| `selector:` | 53 | Parameter |
| `message:` | 40 | Parameter |
| `value:` | 36 | Parameter |
| `db:` | 31 | Parameter |
| `obj:` | 22 | Parameter |
| `format:` | 22 | Parameter |
| `arg0:` | 20 | Parameter |
| `arg1:` | 16 | Parameter |
| `item:` | 14 | Parameter |
| `proxy:` | 13 | Parameter |
| `paramName:` | 11 | Parameter |
| `d:` | 10 | Parameter |
| `arg2:` | 9 | Parameter |
| `decimals:` | 8 | Parameter |
| `val1:` / `val2:` | 6 each | Parameter |
| (long tail of narrower types/params) | ≤17 each | — |

This sample reinforces the LINQ (`predicate:`/`selector:` = 129, the biggest category yet) and
format-string (`format:`/`arg0:`/`arg1:`/`arg2:` = 67) findings from the previous two samples at
even higher volume, and surfaces two genuinely new points:

1. **Even the .NET BCL itself ships famously opaque parameter names — and the hints faithfully
   reproduce them.** Confirmed concrete cases:

   ```csharp
   decimal computedVatAmount = Math.Round(
       computedVatAmountForDelivery + computedVatAmountForItems,
       2,
       MidpointRounding.AwayFromZero);
   ```
   → hints `d:` and `decimals:` on the first two arguments (`Math.Round`'s real parameter names
   are literally `d` and `decimals`).

   ```csharp
   return Math.Min(100, Math.Max(0, parsedValue));
   ```
   → hints `val1:`/`val2:` on *both* the outer `Math.Min` and inner `Math.Max` calls
   (`Math.Min`/`Math.Max`'s real parameter names are literally `val1`/`val2`).

   These are arguably a stronger suppression candidate than the LINQ case: they're tied to a
   small, enumerable list of extremely common, well-known BCL methods (`Math.Round`, `Math.Min`,
   `Math.Max`, and likely similar ones worth auditing) whose own parameter names are so generic
   they need a name *for* the parameter name — a hint here can't possibly help a reader, only
   clutter. Unlike the fuzzier heuristics above, this could ship as a small, static allow-list
   with essentially zero risk of suppressing a genuinely useful hint.

2. **Counter-example found — not every well-known BCL method should lose its hints.** In the same
   file:

   ```csharp
   var bank = bankAccount.Substring(4, bankAccount.Length - 4) + bankAccount.Substring(0, 4);
   ```
   → hints `startIndex:`/`length:` on `string.Substring(int startIndex, int length)`. Unlike
   `Math.Round`/`Math.Min`/`Math.Max`, these parameter names are genuinely informative — two
   same-typed (`int`) positional arguments where transposing them is a real, easy, silent bug.
   This is exactly the kind of hint that should be *kept*. It's a useful reminder that "well-known
   BCL method" alone isn't a sufficient signal for suppression — it has to be combined with
   "the parameter names themselves are uninformative," which has to be judged per-method (or at
   minimum per a curated allow-list), not derived from a generic rule like method popularity or
   argument-type sameness alone.

3. **New UX nuance, orthogonal to redundancy: anonymous-type hints can be very long.** Found a
   hint whose label was the full shape of a multi-field anonymous type (7 occurrences), e.g.
   `: <anonymous type: int Year, int Month, ..., int InvoiceCount>` — presumably from a grouped/
   aggregated LINQ projection assigned to `var`. The *information* here isn't redundant (anonymous
   types have no name to look up any other way), so this isn't a suppression candidate under the
   framework this doc has used so far. But the sheer rendered length is its own distinct
   complaint — worth tracking separately as a possible follow-up (e.g. truncating/eliding
   anonymous-type hints beyond N members) rather than folding it into the suppress/keep binary.

## Real-World Evidence: Fourth Sample (MVC Controller, Two Companion Files)

A fourth sample, from two companion partial-class files of one MVC controller in the same private
codebase (an "account details" controller and its "order details" sub-view counterpart — the
same controller class split across two files by feature). 307 unique hints from the first file,
781 from the second (de-duplicated by position). Top labels combined:

| Label | Count | Kind |
|---|---|---|
| `selector:` | 50 | Parameter |
| `predicate:` | 43 | Parameter |
| `value:` | 35 | Parameter |
| `timestamp:` | 20 | Parameter |
| `name:` | 20 | Parameter |
| `keySelector:` | 17 | Parameter |
| `user:` | 17 | Parameter |
| `: Timing?` | 14 | Type |
| `relatedObjectSelector:` | 10 | Parameter |
| `expression:` / `errorMessage:` | 10 each | Parameter |
| `d:` / `decimals:` | 10 each | Parameter |
| `mode:` | 9 | Parameter |
| `actionName:` / `routeValues:` | 8 each | Parameter |
| `viewName:` / `model:` | 6 / 7 | Parameter |
| (long tail incl. single-letter names `s:`, `k:`, `l:`) | ≤10 each | Parameter |

Alongside reinforcing every finding from the first three samples (LINQ names, the repeated
profiling-scope idiom, format-string args, `RedirectToAction`'s idiom), this pair of files
surfaces three more new points:

1. **A fourth confirmed instance of the "opaque BCL parameter name" pattern — plus a nuance.**
   `Math.Round(price, decimalPlaces, MidpointRounding.AwayFromZero)`-style calls appear here too,
   this time surfacing all three parameter names (`d:`/`decimals:`/`mode:`) rather than just the
   first two seen in the third sample. `mode:` is a mildly more informative name than `d`/`decimals`,
   but is still arguably redundant here specifically *because* the argument is a fully-qualified
   enum member access (`MidpointRounding.AwayFromZero`) — the enum's own type name already conveys
   the meaning. **Tentative extra candidate:** suppress parameter-name hints when the argument is
   a qualified enum-member-access expression *and* it's the sole parameter of that enum type in the
   call — same caution as the `Substring` counter-example applies: this breaks down for calls with
   two same-typed enum arguments where the parameter names are the only way to tell them apart.

2. **The "single lambda argument, method name already says what it's for" LINQ finding
   generalizes beyond `System.Linq`.** `relatedObjectSelector:` (10 occurrences, often repeated
   identically down a chain of consecutive lines) comes from NHibernate's fluent eager-loading API:

   ```csharp
   .Fetch(x => x.RelatedEntity).ThenFetch(y => y.SubEntity)
   .Fetch(x => x.RelatedEntity).ThenFetch(y => y.OtherSubEntity)
   ```

   Exactly the same shape as LINQ's `Where`/`Select`: a single lambda argument whose role is
   already fully conveyed by the enclosing method's own name (`Fetch`/`ThenFetch`). This suggests
   rule #2 in the running list below should be phrased more generally — "single lambda/expression
   parameter, unambiguous overload, method name conveys role" — rather than hard-coded to
   `System.Linq`, so it also naturally covers other fluent/ORM-style APIs.

3. **Single/near-single-character parameter names recur widely, on both BCL and user-defined
   methods** — `d:`, `s:`, `k:`, `l:` all appear as real parameter-name hints in this pair of
   files (the last three from ordinary user-defined helper methods, not BCL). This broadens the
   "opaque BCL parameter name" allow-list idea (finding #1 from the third sample) into a
   potentially simpler, more general, method-agnostic heuristic: **a parameter name of length ≤ 1–2
   characters is essentially never informative as a hint, regardless of which method (BCL or
   user-defined) it belongs to** — likely cheaper to implement and higher-coverage than maintaining
   a hand-curated per-method allow-list, though it should be validated against real cases before
   committing (e.g. does a 2-letter abbreviation ever meaningfully disambiguate two same-typed
   args better than nothing?).

## Real-World Evidence: Fifth Sample (Small API Controller, ~210 Lines)

A fifth, much smaller sample — an API controller file (~210 lines) in a different subsystem of the
same private codebase (an auth-scoped API, not the MVC website). Only 90 unique hints total, but
dense (roughly one hint every 2 lines), and it surfaces two clean, novel findings on top of
reinforcing the repeated-idiom pattern at unusually high density.

| Label | Count | Kind |
|---|---|---|
| `value:` | 7 | Parameter |
| `: DBCategoryMapping` (illustrative name) | 6 | Type |
| `scopeId:` (illustrative name) | 5 | Parameter |
| `scopeCaps:` (illustrative name) | 5 | Parameter |
| `asyncOp:` | 5 | Parameter |
| `predicate:` | 4 | Parameter |
| `id:` | 4 | Parameter |
| (long tail incl. 2 tuple/anonymous-type hints, ≤3 each) | ≤3 each | — |

1. **A third confirmed instance of the repeated fixed-signature callback idiom — at very high
   density, and with a second confirmed instance of the same-name suppression gap.** Every action
   method in this file opens with:

   ```csharp
   public async Task<IActionResult> SomeAction(
       [FromRoute] string scopeId,
       [FromRoute] string otherId
   ) => await this.AuthContext.WithScopeAsync(
       scopeId,
       AuthCaps.ReadScope,
       async (db, scope) => { ... });
   ```

   repeated 5 times in ~210 lines (roughly once every 40 lines) — a *different* shared helper
   (`AuthContext.WithScopeAsync`, an auth-scoping helper) than the `BaseController` one from the
   first sample, confirming this "load-scope-then-invoke-callback" idiom is a pervasive,
   independently-reimplemented architectural convention across the codebase, not a one-off. Each
   occurrence re-hints `scopeId:`, `scopeCaps:`, and `asyncOp:` identically. Critically,
   `scopeId:` fires on an argument whose local variable is *also* named `scopeId` — the exact same
   same-name-suppression gap found in the first sample's `resourceName:` case, now reproduced a
   second time against a completely unrelated helper method. This raises confidence that it's a
   genuine, reproducible bug in `validateParameter`'s text-matching logic rather than a one-off
   fluke, and that fixing it would have an outsized real-world impact given how idiomatic this
   call shape is throughout the codebase.

2. **New rule candidate: the type is already spelled out in an explicit generic type argument.**
   ```csharp
   var integration = Enum.Parse<IntegrationId>(integrationCode);
   ```
   → hint `: IntegrationId` on `integration`. But the type is already written out, verbatim, one
   `Enum.Parse<` away from the hint itself — the reader doesn't need a hint to know the type; it's
   sitting right there in the explicit type-argument list of the very same expression. This is a
   concrete, low-risk, easily-specified refinement of the initializer-shape idea from the original
   candidate list: **suppress a `var` type hint when the initializer is a generic invocation whose
   explicit type-argument list contains the exact inferred type** — covers `Enum.Parse<T>()` and
   similarly-shaped generic factory/parse/deserialize methods (`JsonSerializer.Deserialize<T>()`,
   `Activator.CreateInstance<T>()`, etc.) without needing a per-method allow-list.

3. **A second, different confirmation of the anonymous/tuple-type verbosity nuance.** A hint like
   `: (string Code, string Value, string Name, string UIValue)` appears here from a named
   `ValueTuple` literal returned out of a `.Select(...)` projection — the same "non-redundant but
   visually heavy" complaint raised for anonymous types in the third sample, just for tuples
   instead. Reinforces that this is worth tracking as its own (separate, length-focused) follow-up
   regardless of which compound-type shape triggers it.

## Real-World Evidence: Sixth Sample (Four Files Spanning Very Different Domains)

A sixth, deliberately diverse batch: an API controller, a small low-level file-system utility
class, a Win32 P/Invoke device-info wrapper, and a message/event-processing handler class — picked
specifically to test whether the findings so far are specific to "MVC controller / business-logic
ops class" code, or hold more generally. They do — every prior finding reappears — plus this batch
surfaces several new, sharply-defined points, including (via reading `InlayHint.fs` itself) a
concrete, high-confidence diagnosis of the same-name suppression bug's root cause.

**Controller file** (~1,100 lines, 27 requests): reinforces `predicate:`/`office:`/`db:`/`user:`
etc. at similar volumes to earlier controller samples, plus two small new points:
`opOnOrderAsync:`/`messageDispatcherAsync:` are further confirmed instances of the "repeated
delegate-callback-parameter" idiom (finding from sample 1), and `messageDispatcherAsync` is
additionally an example of **a parameter name that's just a decapitalized copy of its own
delegate type name** (`MessageDispatcherAsync` the type → `messageDispatcherAsync` the parameter)
— a narrower, safer sibling of the "var name already spells out the type" rule (#6 below), applied
to parameters instead of locals.

**File-system utility class** (~240 lines, 63 hints): a plain, non-business-logic I/O helper —
useful because it's dominated entirely by BCL calls, giving two more data points on the
"well-known BCL method, good or bad parameter names?" question:
- `new FileStream(path, FileMode.OpenOrCreate, FileAccess.Write)` (repeated identically 4 times)
  → hints `path:`/`mode:`/`access:`. Here the *two enum-typed* arguments are of **two different
  enum types** (`FileMode`, `FileAccess`), so — unlike the single-enum-argument `Math.Round`
  case — there's no way to confuse them even without hints; the qualified enum member access
  already fully disambiguates both. Cleaner support for the tentative "qualified enum member
  access is self-describing" idea than the earlier example.
- `file.Write(value, 0, value.Length)` (`Stream.Write(byte[] buffer, int offset, int count)`)
  → hints `buffer:`/`offset:`/`count:`. A second confirmed "genuinely useful, don't suppress"
  counter-example alongside `string.Substring` — `offset`/`count` are same-typed ints where mixing
  them up is a real bug class.
- `Path.Combine(path1, path2)` → hints `path1:`/`path2:`. A third real BCL example (after
  `Math.Min`/`Math.Max`'s `val1`/`val2` and the format-string `arg0`/`arg1`/`arg2`) of the same
  underlying anti-pattern: **numbered-suffix parameter names**. All three of these could collapse
  into a *single* mechanical rule instead of three separate special cases (see rule #2 below).

**Win32 P/Invoke device-info wrapper** (~270 lines, 90 hints): the most extreme evidence yet for
short/opaque parameter names, and a wholly new category:
- `new Guid(0x8c7ed206, 0x3f8a, 0x4827, 0xb3, 0xab, 0xae, 0x9e, 0x1f, 0xae, 0xfc, 0x6c)` → hints
  `a:`/`b:`/`c:`/`d:`/`e:`/... — the *actual .NET BCL* `Guid` struct constructor's real parameter
  names are single letters. About as strong a confirmation as possible of rule #1 below.
- A native `[DllImport]`-declared method (a real Win32 SetupAPI call) is invoked identically 4
  times, each time hinting its native-header-derived, PascalCase parameter names (`DeviceInfoSet:`,
  `DeviceInfoData:`, `PropertyBuffer:`, `RequiredSize:`, etc.) — a **new category**: parameter
  hints on calls to `extern`/P-Invoke-declared methods largely restate names that are meaningless
  without the platform's own API documentation anyway (which is where a developer calling a
  well-known native API actually looks up argument order, not the C# parameter hint), and are
  frequently non-idiomatic verbatim copies of the native header. **Candidate rule:** suppress
  parameter-name hints for arguments to methods marked `extern` / decorated with
  `DllImportAttribute` (or, more conservatively, restrict this to well-known OS API DLLs).

**Message/event-processing handler class** (~530 lines, 139 hints): reinforces `id:`/`message:`/
`value:` at high volume, plus a very clean **counter-example**: `oldValue:`/`newValue:` hints on
what's evidently a change-tracking/audit-style handler — transposing old vs. new is a serious,
easy, silent bug class, so these hints should absolutely be *kept*, reinforcing that any
implemented ruleset needs solid negative-case test coverage, not just positive suppression cases.

### Root-Cause Analysis: The Same-Name Suppression Gap (Confirmed by Reading `InlayHint.fs`)

This sample's investigation went one step further: rather than just re-observing the
`resourceName:`/`scopeId:`-style gap a third time, we read the actual `validateParameter`
implementation in this repo's `src/CSharpLanguageServer/Handlers/InlayHint.fs`:

```fsharp
let validateParameter (arg: SyntaxNode) (par: IParameterSymbol) =
    match arg.Parent with
    | :? BracketedArgumentListSyntax -> None
    | _ when String.IsNullOrEmpty(par.Name) -> None
    | _ when String.Equals(arg.GetText().ToString(), par.Name, StringComparison.CurrentCultureIgnoreCase) ->
        None
    | _ -> Some par
```

This directly explains two distinct issues found across the real-world samples:

1. **Likely root cause of the `resourceName:`/`scopeId:` bug:** `arg.GetText()` returns the
   `SyntaxNode`'s text *including its trivia* (leading/trailing whitespace, newlines) — much like
   `ToFullString()` — rather than just its significant text. This codebase's dominant call-site
   style puts each argument of a multi-line call on its own indented line
   (`WithScopeAsync(\n    scopeId,\n    ...)`), so `arg.GetText()` for that `scopeId` argument
   likely includes its leading indentation whitespace and is therefore *not* byte-equal to the
   bare parameter name `"scopeId"`, even though the meaningful identifier text is identical —
   causing the case-insensitive equality check to silently fail and the hint to wrongly render.
   **Suggested fix:** compare against the argument's significant/underlying identifier text
   instead of raw `GetText()` — e.g. match on `argument.Expression` when it's an
   `IdentifierNameSyntax` and compare `.Identifier.ValueText`, rather than the whole node's text.
   This is a hypothesis based on reading the code, not yet runtime-verified — worth confirming
   with a quick unit test contrasting a single-line call vs. a multi-line indented call before
   relying on it.
2. **A second, structurally distinct gap, not just a trivia bug:** even with the trivia issue
   fixed, this exact-string-match approach can *never* suppress a qualified member-access argument
   like `this.RazorEngineService` against a parameter named `razorEngineService` — the *whole*
   argument text (`"this.RazorEngineService"`) will never equal the bare parameter name
   (`"razorEngineService"`), no matter the casing. This was confirmed directly against source: a
   6-argument constructor call passing `this.RazorEngineService`, `this.LocalizationProvider`,
   `this.Timestamp`, `this.PdfRenderer`, and `this.BlobStore` for parameters
   `razorEngineService`, `localizationProvider`, `timestamp`, `pdfRenderer`, `blobStore`
   (all case-insensitive exact matches once the `this.` qualifier is considered) all still get
   hints — this is a real, common pattern (`this.Foo` arguments matching `foo` parameters in
   constructor/DI-style calls) that the current same-name check simply isn't designed to catch.
   **Suggested enhancement:** extend `validateParameter` to also compare the parameter name
   against the last identifier segment of a qualified member-access expression
   (`MemberAccessExpressionSyntax.Name.Identifier.ValueText`), not just bare identifiers.

Both fixes are independent, small, and additive to the existing `validateParameter` logic — no new
heuristic categories needed, just making the existing "don't restate what's already in the
argument's own name" rule actually work in the two most common shapes it currently misses.

**Net takeaway across all six samples:** the highest-value, lowest-risk rules to prioritize
first, ranked roughly by combined volume, confidence, and implementation risk:

1. **✅ Implemented.** Fix the same-name parameter suppression gap — now root-caused (see above)
   into two concrete, independent, small fixes: (a) compare significant identifier text instead of
   raw `GetText()` (likely trivia/whitespace bug), and (b) extend the match to the last segment of
   qualified member-access arguments (`this.Foo` vs. parameter `foo`). Confirmed across three
   samples against at least three unrelated call sites/helpers; given how idiomatic both call
   shapes are throughout the codebase, this pair of fixes alone would likely eliminate a large
   fraction of real-world noise, before any new heuristic work.
2. **✅ Implemented.** Suppress parameter-name hints for very short (≤ 1–2 character) parameter
   names and for numbered-suffix parameter names (`arg0`, `val1`, `path2`, i.e. matching
   `^\w*?\d+$`) — collapses what were three separate special cases (BCL opaque names,
   format-string args, `Math.Min`/`Max`, `Path.Combine`) into one or two simple, mechanical,
   method-agnostic rules. Confirmed against the `string.Substring`/`Stream.Write(buffer, offset,
   count)`-style counter-example — it doesn't fire on short-but-genuinely-disambiguating names.
   Note: intentionally doesn't catch every opaque BCL name this way (e.g. `Math.Round`'s
   `decimals`/`mode` are kept, only `d` is suppressed) — a per-method allow-list would catch more
   but was deferred as higher-risk/higher-maintenance.
3. **✅ Implemented.** Suppress parameter-name hints for single lambda/expression-typed arguments
   passed to methods whose own name already conveys the lambda's role and which have no ambiguous
   overload — generalized from "well-known `System.Linq` methods" to also cover other fluent/ORM
   APIs (e.g. NHibernate's `Fetch`/`ThenFetch`). Implemented as a syntactic check (sole argument of
   the call is a `LambdaExpressionSyntax`) rather than a method allow-list, so it covers any
   fluent/ORM API with this shape for free. Confirmed via tests that it doesn't fire on
   multi-lambda-argument calls (e.g. a hypothetical `Combine(first, second)`) or on method-group
   arguments (e.g. `Where(IsPositive)`), both of which keep their hints. Also refined to not count
   a trailing `CancellationToken` argument towards "sole argument", since it's a conventional,
   non-essential pass-through parameter on async APIs (`WhereAsync(x => x.IsActive,
   cancellationToken)` behaves the same as `Where(x => x.IsActive)`).
4. **✅ Verified subsumed by rule #2, no separate implementation needed.** Suppress
   parameter-name hints for arguments following a composite-format string literal (`{0}`/`{1}`/...
   placeholders) on formatting/logging-style method calls. Confirmed via a test mirroring
   log4net's real `ILog.DebugFormat(string format, object arg0, object arg1, object arg2)`
   overload shape that `arg0`/`arg1`/`arg2` are already caught by rule #2's numbered-suffix
   pattern — the broader originally-proposed heuristic (parsing `{n}` placeholders out of the
   format string) wasn't needed.
5. **✅ Implemented.** Suppress `var` type hints when the initializer is a generic invocation whose
   explicit type-argument list already contains the exact inferred type (e.g. `Enum.Parse<T>()`,
   `JsonSerializer.Deserialize<T>()`) — concrete, low-risk, no allow-list needed. Implemented as
   `isTypeSpelledOutInGenericInvocation`, comparing by resolved symbol (not name), so it covers
   both qualified (`Enum.Parse<T>()`) and unqualified (a local generic factory method) call shapes
   without a per-method allow-list, and correctly keeps the hint when the invocation's return type
   differs from its type argument (confirmed via a `Describe<Widget>(...) : string` test).
6. **✅ Implemented.** Generalize the existing same-name suppression (`validateParameter`) to type
   hints (`validateType`): suppress when the declared variable's identifier is a case-insensitive
   match — full or suffix — of the inferred type's name (and, per this sample, consider the same
   for parameter names that are a decapitalized echo of their own type, e.g. `messageDispatcherAsync`).
   Implemented as `identifierEchoesTypeName` (full case-insensitive match, or last-camelCase-word
   match, so `settingChangeCondition` vs. `ResourceCondition` — both ending in "Condition" — is
   caught even though neither is a full match or plain substring of the other), wired into all
   four identifier-bearing type-hint contexts (`var` declarations, `out var`/deconstruction,
   pattern-match designations, `foreach` variables) and, for the parameter-name sibling, into
   `validateParameter`.
7. Consider suppressing parameter-name hints for arguments to `extern`/P-Invoke-declared methods,
   given their names are typically non-idiomatic, verbatim native-header copies that add little
   without external platform documentation regardless.

The idiom-repetition (suppress-on-known-recurring-callback-shape), idiom-shape (e.g.
`RedirectToAction`/`View`), qualified-enum-argument idea, and anonymous/tuple-type-verbosity ideas
remain worth pursuing but are lower-volume, more subjective, and/or a different kind of problem
(length vs. redundancy) than the seven above. Equally important: this sample reinforced that
**negative test cases matter as much as positive ones** — `Stream.Write(buffer, offset, count)`
and `oldValue`/`newValue`-style handlers are concrete, real examples where hints must be kept.

## Configuration Angle

A related (but separate) axis is **making hint kinds configurable** (per the existing TODO
comment), so users/editors can opt in/out of type hints, parameter hints, or specific sub-cases
independently of any built-in suppression heuristics. Whether this ships alongside or instead of
built-in suppression heuristics is also to be decided during design.

## Next Steps

1. ✅ **Done.** Land the two same-name suppression gap fixes identified by reading `InlayHint.fs`
   (see "Root-Cause Analysis" above): (a) compare significant identifier text rather than raw
   `arg.GetText()` in `validateParameter`, and (b) extend the same-name match to also cover the
   last identifier segment of qualified member-access arguments (`this.Foo` vs. parameter `foo`).
   Landed with dedicated positive/negative test coverage in `InlayHintTests.fs`.
2. ✅ **Done.** Implement rule #2 (short / numbered-suffix parameter names) as
   `hasUninformativeParameterName` in `validateParameter`, with test coverage including the
   `string.Substring`/`Stream.Write(buffer, offset, count)`-style negative controls.
3. ✅ **Done.** Implement rule #3 (single lambda-argument calls) as a new `validateParameter`
   match arm keyed on the argument list's effective (non-`CancellationToken`) argument count being
   1 and `argExpr :? LambdaExpressionSyntax`, with test coverage for the multi-lambda-argument,
   method-group-argument, and trailing-`CancellationToken` negative/edge-case controls.
4. ✅ **Done.** Verify rule #4 (composite-format-string positional arguments) against a
   `log4net`-shaped `DebugFormat(format, arg0, arg1, arg2)` test — confirmed it's fully subsumed by
   rule #2's numbered-suffix pattern; closed out with no separate implementation.
5. ✅ **Done.** Implement rule #5 (generic-invocation-redundant `var` hints) as
   `isTypeSpelledOutInGenericInvocation`, filtering `validateType`'s result in the
   `VariableDeclarationSyntax` match arm, with test coverage for qualified/unqualified matching
   invocations, a non-matching-return-type negative control, and the plan's own
   `Enum.Parse<DayOfWeek>(...)` example.
6. ✅ **Done.** Implement rule #6 (identifier-echoes-type-name generalization) as
   `identifierEchoesTypeName`, wired into the four identifier-bearing type-hint match arms and
   into `validateParameter` for the parameter-name sibling, with test coverage for full-name
   matches, last-word matches, foreach variables, and non-matching negative controls on both the
   type-hint and parameter-hint sides.
6a. ✅ **Done.** Implement the `obj`-specific generic-parameter-name rule as `genericParameterNames`
   (folded into `hasUninformativeParameterName`), with test coverage for the positive case
   (`Save(object obj)`) and a same-length-but-different-name negative control (`Log(string msg)`).
6b. ✅ **Done.** Implement rule #5's plain-object-creation sibling as
   `isTypeSpelledOutInObjectCreation`, filtering `validateType`'s result in the
   `VariableDeclarationSyntax` match arm alongside `isTypeSpelledOutInGenericInvocation`, with test
   coverage for both the plain-constructor and object-initializer shapes.
7. Design session to enumerate the exact suppression rule for the one remaining candidate (rule
   #7 in the "Net takeaway" ranking — `extern`/P-Invoke parameter names — plus any of the
   lower-priority/subjective ideas from "Candidate Ideas Gathered So Far" worth reconsidering),
   informed by the real-world evidence above and by user/editor feedback on which current hints
   still feel noisy in practice.
8. Implement the agreed predicate(s) in `toInlayHint`, with unit/integration test coverage
   (positive case: hint suppressed; negative case: hint still shown for the non-redundant
   variant) — extend the existing `InlayHintTests.fs` / `InlayHintTest.cs` fixture rather than
   creating new ones, so all rules stay exercised against one file.
9. Decide on and (if wanted) implement the configuration angle above.

## Files Likely Touched

| File | Likely Change |
|------|----------------|
| `src/CSharpLanguageServer/Handlers/InlayHint.fs` | Add remaining suppression predicate to `toInlayHint`'s match arms, per the design once finalized (rules #1–#6 already landed/closed) |
| `tests/CSharpLanguageServer.Tests/InlayHintTests.fs` | Add positive/negative coverage for the remaining rule (rules #1–#6 already covered) |
| `tests/CSharpLanguageServer.Tests/Fixtures/genericProject/Project/InlayHintTest.cs` | Extend with new code shapes per remaining rule |
