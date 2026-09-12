---
title: let with, and let without in
status: todo
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

## Log
