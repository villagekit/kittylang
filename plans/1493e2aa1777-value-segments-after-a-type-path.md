---
title: Value segments after a type path
status: done
parent: 1cee599ce218
derived_from: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
  - target: 4423cd0aa2bb
    strength: soft
    note: the keyword-argument snapshot uses = once it lands
  - f280931fdfe2
priority: medium
---

An expression may name an item through its type: `N.default()`,
`Quaternion.default()`, `Self.regular()` and `GridBeam.Z` parse as a
type path followed by a value segment ([[1cee599ce218]]). Decision
[[f2708b12e004]] says `.` is unified across values and types because a
segment's kind is known from its spelling; today the expression grammar
takes only type segments after a type path, so `prop x: N = N.default()`
errors in `3d-math.kitty` and every associated function call in the
examples fails with it.

## Work

- `parser/src/grammar/expression.rs` and `type.rs`: in expression
  position, a type path may be followed by `.` and a value identifier,
  ending the type path and continuing as an expression (a call, a
  field, an operator). The segment takes `from` as a name too, as the
  function name and the field get do since [[9d84d1f97437]], so
  `Length.from(5)` reaches the `From` method. `kitty-syntax` gets the
  node the expression needs, if the existing path node cannot carry it;
  `kitty-cst` the view.
- Snapshots: `N.default()`, `Self.regular()`, `GridBeam.Z(x = 0)` once
  [[4423cd0aa2bb]] lands (a soft edge; the snapshot uses positional arguments
  otherwise), `Type.Assoc.value` mixing the two, and a `.` with nothing
  after it recovering.

Interfaces: none produced.
Verify first: `cargo run -p kitty-cli -- parse examples/3d-math.kitty`
shows "expected type-id, but found value-id" at `N.default()`.
Specs: `grammar.md`, paths in expressions.
Not this slice: what the segment resolves to, which is M3's.

## Seams under test

`kitty_parser::parse` snapshots; the examples test on `3d-math.kitty`
and `3d-object.kitty`.

## Done when

- The examples test's expectation for `3d-math.kitty` lists no error at `N.default()`
- `timeout 600 just check` is green

## Outcome

Shipped. In expression position a type path now ends before a `.` that
no type identifier follows, and the Pratt loop's existing field get
takes the value segment (`N.default()`, `Self.regular()`,
`Length.from(5)`, `Type.Assoc.value`). `type_path` in `type.rs` became a
shared segment loop with a `TypePathEnd` mode: `Type` keeps a `.` an
association in annotations and patterns; `Expression` is what
`expression_primary` calls. No new node: `ExpressionGet` carries the
type path as its lhs, so no node kind or view changed.
The examples test drops eight `expected type-id, but found value-id`
errors (three in `3d-math.kitty`, four in `3d-object.kitty`, one in
`chair.kitty`).

Deviations: none. The `GridBeam.Z(x = 0)` snapshot is named for what it
pins, `two_type_segments_stay_a_type_path`; both segments are types, so
it covers behaviour that already held.

In flight: `N.` reports `missing value-id or ‘from’`, the field get's
names, though a type identifier is also legal there. Kept as is, since
the value reading is the likelier intent and a complete message would
need the field get to know its lhs; stated in `grammar.md`, not as a
Gap, since no requirement asks for the fuller message.
The spec's Terms gain "value segment" so the plan's noun and the spec's
"field get" bind.

Review findings deferred, not fixed: the CST `Expression` compound
does not include type-path kinds, so an `ExpressionGet` or
`ExpressionApply` over a type path has no `Expression` lhs view;
pre-existing for apply, noted in `cst/src/lib.rs` for M3. `a.B` (a type
name after a value) is still an error; decision f2708b12's "`.` unified
across values and types" is applied for the type-then-value direction
only, and no example needs the other. Rejected: always breaking
`type_path` before a non-type-id `.` in every position, which would
lose `expected type-id, but found value-id` in a mistyped annotation
and dangle the `.` onto the caller.

No visual gate: the change touches no route.

## Log
