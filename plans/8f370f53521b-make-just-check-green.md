---
title: "Make just check green"
status: done
priority: high
---

The quality gate `just check` was added with the working methodology on
2026-09-09 and does not pass yet: clippy refuses the workspace and the
parser prints to stdout. This slice makes the gate green so every later
slice can end on it.

## Work

- `kitty-meta`: `meta/src/source.rs` compares a length to zero
  (`clippy::len_zero`); use `is_empty`.
- `kitty-parser`: `parser/src/lib.rs` prints the event list in
  `parse_grammar` (`println!("events: ...")`), which CLAUDE.md's tracing
  rule forbids in committed code; remove it.
- Anything else `timeout 600 just check` finds once those two are gone.

Verify first: `cargo clippy --workspace --all-targets -- -D warnings`
stops at `kitty-meta`, so the parser and later crates have not been
linted yet; expect more.

## Seams under test

None new. The existing lexer and parser snapshot tests are the guard.

## Done when

- `timeout 600 just check` is green

## Outcome

Shipped: `timeout 600 just check` is green, 154 tests passing. The two
named fixes landed as written. Clippy then found more once `kitty-meta`
compiled, all fixed in the same pass:

- `kitty-lexer`: the elided lifetime on `lex`, the test-only `Tokens`
  helper gated behind `cfg(test)`, a redundant `Vec` import.
- `kitty-cst`: the unused `tokens` helper deleted; `nodes` shows the shape
  when a view needs it.
- `kitty-parser`: `map_or(false, ..)` to `is_some_and` in three places,
  `mem::replace` to `Option::replace` in `Marker::complete`,
  `PATTERN_FIRST` deleted because it duplicated `pattern_single`'s
  dispatch, `TokenSet::ALL` and `without` deleted as unused, and
  `TokenSet::NONE` gated to `cfg(test)` since only the grammar tests use
  it. `Marker::abandon` is kept under an explicit `allow(dead_code)`: the
  glossary defines a marker as completed or abandoned, and recovery rules
  will need it.

Deviations: none from the plan. Review round one flagged two things:
`Marker::abandon` had been deleted, which made the glossary's Marker
entry false, so it was restored; and a hand-written `Default` on
`TokenSet` existed only to keep `NONE` alive, replaced by the `cfg(test)`
gate above. The unused `Default` derive on `TokenSet` went with it. No
decisions made.

Found, not fixed, left as `TODO(cc)` for the M2 grammar work:
`pattern_type` asserts on `IdentifierType` while `pattern_single`
dispatches to it on `TYPE_PATH_FIRST`, which includes `SelfUpper`, so a
`Self` pattern panics against the parser's no-panic rule.

## Log

- 2026-09-09: implemented and shipped.
