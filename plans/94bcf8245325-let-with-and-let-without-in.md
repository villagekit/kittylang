---
title: let with, and let without in
status: done
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
  - 277624e4f5fe
  - target: 9d84d1f97437
    strength: soft
    note: both edit lexer/src/token.rs; sequencing avoids a conflict
priority: medium
derived_from: 1cee599ce218
---

`let` ends at the newline, `in` is allowed on one line, and `let with
<expr>` followed by an indented list of names destructures over several
lines ([[1cee599ce218]]).

## Work

- `parser/src/grammar/expression.rs`: the `let` rule requires `in`
  today; it takes `in` when the next non-trivia token is `in` and
  otherwise ends after the initialiser, the rest of the block being the
  body. Newlines are trivia to the parser today, so an initialiser
  continued on the next line with `(` would read as a call; the
  examples do not do this, and where a newline ends an expression is
  [[d0658cb19697]]'s question. `let with`
  parses the expression then an indented block of value identifiers,
  one per line, into a `let with` node in `kitty-syntax`; no `in`
  follows the block, the body is what comes next. The lexer gains a
  `With` keyword, in `lexer/src/token.rs` beside [[9d84d1f97437]]'s edits.
- `kitty-cst` views; snapshots: `let` with `in`, without, `let with` with
  three names, `let with` with an empty block.
- The HIR desugaring of `let with` to field bindings is M3's; the tree
  keeps the form.

Interfaces: none produced.
Verify first: `sed -n 160,180p parser/src/grammar/expression.rs` shows
`in` required; `grep -n With lexer/src/token.rs` prints nothing.
Specs: `lexing.md`, keywords; `grammar.md`, `let`.
Not this slice: newlines inside brackets ([[d0658cb19697]]).

## Seams under test

`kitty_parser::parse` snapshots; the examples test on `chair.kitty`,
`sample.kitty` and `3d-math.kitty`.

## Done when

- Snapshots show `let` with `in`, `let` without, `let with` with three names and `let with` with an empty block
- The examples test's expectations for `sample.kitty`, `3d-math.kitty` and `chair.kitty` list no error at a `let` line or in the `let with` block
- `timeout 600 just check` is green

## Outcome

Shipped. `in` is optional after a `let` value: the value ends where the
next token is not `in`, and the body is what follows. `let with <expr>`
then an indented block of value identifiers then the body parses into a
new `ExpressionLetWith` node; the block after the value is the names,
never the value's argument list. The lexer gains the `with` keyword.
`kitty-cst` gains `ExpressionLetWith` (`value`, `names`, `body`) and
`ExpressionLet::body`. The four snapshots the plan asks for are in
`parser/src/grammar/expression.rs` (`let_expression_type`,
`let_expression_without_in`, `let_with_three_names`,
`let_with_empty_block`), with three recovery cases beside them. The
examples test: `sample.kitty` now lists no error; `chair.kitty` lists
none at its `let` lines or in the `let with` block, and the errors in
the let region fell from 41 to one, the bare `in` line after the block
([[75d0d7eea26c]] removes that line).

Deviations and flags for the operator:

- The plan's claim that "the examples do not" continue a value on the
  next line with `(` is false: `3d-math.kitty` lines 14-15 (`= self`
  then `(x * x + ...).sqrt()`) read as a call, so the file still lists
  one error, at its end (the missing body). Newlines are trivia as the
  plan says, and where a newline ends a value is [[d0658cb19697]]'s
  question; [[75d0d7eea26c]]'s exit demo wants this file empty, so one
  of the two must resolve that line. Not fixed here: no other slice
  edits `examples/`.
- Recovery: an `in` after the `let with` block, the single-line form's
  habit, is the body's error, consumed as one, and the body is read
  after it (`let_with_stray_in`); without this the enclosing block
  cascaded errors onto the `let` lines below, the plan's exit
  criterion. The Spec reviewer flagged it as scope creep; kept, since
  the grammar is unchanged and the mistake is the likely one.

Review findings deferred, each marked in the code: the CST accessors
are positional, so on a recovery tree with the value missing `value`
is the body (`Note(cc)` in `cst/src/lib.rs`, M3's concern); a `let`
body missing at the end of input lists the operator kinds the value's
Pratt loop recorded, since nothing clears them between the value and
the body (`TODO(cc)` in `expression.rs`; the old `expect(In)` masked
this); a nested block among the names is skipped token by token and
its dedent ends the names, as the argument block's loop does
(`Note(cc)`). Rejected: an indented block after a `let` value being
its argument list is the designed call form, not a defect.

No visual gate: the change touches no route.

## Log

- 2026-09-18: shipped on branch `slop`.
