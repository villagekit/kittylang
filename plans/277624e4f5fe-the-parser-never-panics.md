---
title: The parser never panics
status: todo
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
priority: medium
derived_from: 1cee599ce218
---

The parser never panics, on any input ([[1cee599ce218]]): every assertion and
unwrap reachable from `parse` becomes a parse error with recovery or is
made unreachable by construction, starting with the two that fire on
the examples today.

## Work

- `parser/src/grammar/function.rs`: the parameter-list rule asserts on
  `(`; a missing list becomes a parse error naming the expected token
  and a `Missing` node, and parsing continues. `fn parts: Parts` and a
  lambda inside an argument then parse with errors instead of aborting.
- `parser/src/grammar/pattern.rs`: `pattern_type` asserts on a type
  identifier but is dispatched on a set that includes `Self`; a `Self`
  pattern parses as a type path. The `TODO(cc)` there goes.
- The rest of the inventory, each made an error or unreachable:
  `Node::cast(..).unwrap()` in `parser/src/lib.rs`; the two
  `last_token_range().unwrap()` calls in `parser/src/parser.rs`,
  reachable on empty input; the two asserts in `parser/src/sink.rs`;
  `Parser::parse`'s `expect` on an empty event list and the `None`
  `Marker::abandon` leaves mid-stream (research synthesis of 2026-09-09,
  section 8); the `DropBomb` on a leaked `Marker`; and the `assert!(p.at(
  ..))` preconditions at the head of the grammar rules, which stay only
  where the dispatching caller guarantees them and become `debug_assert!`
  with a comment saying which caller, or an error otherwise.
- The three `should_panic` cases in the examples test become
  expectations.

Interfaces: none produced; consumes the examples test from [[942ff7a43d93]].
Verify first: `cargo test -p kitty-parser examples` shows three
`should_panic` cases; `grep -rn "assert!\|unwrap()\|expect(" parser/src`
is the inventory to walk.
Specs: `grammar.md`, recovery.
Not this slice: the fuzz target ([[378fe0c39238]]); any grammar the examples
lack.

## Seams under test

`kitty_parser::parse` recovery snapshots for a function without a
parameter list, a lambda in an argument, a `Self` pattern and empty
input; the examples test.

## Done when

- `cargo test -p kitty-parser examples` has six expectations and no `should_panic`
- A `kitty-parser` snapshot shows empty input parsing to an empty module with no errors
- `timeout 600 just check` is green

## Outcome

## Log
