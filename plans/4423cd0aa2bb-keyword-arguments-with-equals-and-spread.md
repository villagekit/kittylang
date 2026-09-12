---
title: Keyword arguments with equals, and spread
status: todo
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
  - target: 466ffcebbfc1
    strength: soft
    note: both touch function.rs; sequencing avoids a conflict
priority: medium
derived_from: 1cee599ce218
---

Keyword arguments are spelled `name = value` in every call form
([[95cd2585f916]], [[881178303bc8]]): `( )`, `{ }` and an indented
block, and `...expr` spreads a value into a construction
([[1cee599ce218]]).

## Work

- `parser/src/grammar/function.rs`: the labelled argument rule takes
  `=` instead of `:`; `expression_apply` also fires on `{` (a keyword
  argument list in braces) and on an indent after a callee (an indented
  block of `name = value` lines, one per line). A positional line in an
  indented block is a parse error citing [[e6a33eab19d4]] until that
  plan decides otherwise; mixed blocks are an error.
- `parser/src/grammar/pattern.rs`: a brace-delimited constructor
  pattern, `Self { x, y = a }`, which does not exist today (patterns
  dispatch on `(` only, and `3d-math.kitty`'s `let Self { x, y, z } =
  self` cascades into a dozen errors): `pattern_single` accepts a type
  path followed by `{`, the field list allowing a bare name (shorthand)
  or `name = pattern`; and the existing labelled field in the `( )` form
  takes `=` instead of `:` too, so [[95cd2585f916]]'s patterns read as
  shown.
- `...expr` as an argument in any of the three forms, a spread node in
  `kitty-syntax` with a `kitty-cst` view.
- Snapshots for each form, for a spread, for a positional line in an
  indented block, for `{` on a struct name.

Interfaces: produces the argument list that [[328c0a9906fb]] reuses for
attributes.
Verify first: `function_labelled_arg` in `parser/src/grammar/function.rs`
and `pattern_type_arg_labelled` in `parser/src/grammar/pattern.rs` both
expect `Colon`; `grep -rn Ellipses parser/src` prints nothing.
Specs: `grammar.md`, calls and arguments.
Not this slice: positional lines in indented blocks, the design plan's.

## Seams under test

`kitty_parser::parse` snapshots per form; the examples test.

## Done when

- `Self { x = 1 }`, `f(x = 1)` and a callee followed by an indented `x = 1` line each parse to the same argument-list shape in the snapshot
- A snapshot shows `Self { x = 1 }` parsing to a labelled argument carrying `Equal`, and `Self { x: 1 }` recovering with one error
- Snapshots show the patterns `Self { x, y, z }` and `Self { x = a }`, the second binding `a` to field `x`
- `timeout 600 just check` is green

## Outcome

## Log
