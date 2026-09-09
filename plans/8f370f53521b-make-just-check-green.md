---
title: "Make just check green"
status: todo
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

## Log
