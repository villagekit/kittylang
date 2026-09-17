---
title: The examples test
status: done
parent: 5c5f4b9256b3
priority: high
derived_from: 5c5f4b9256b3
---

One test that parses every `examples/*.kitty` program and records, per
file, the parse errors it produces, as an `expect-test` expectation. The
M2 exit bar as a test ([[5c5f4b9256b3]]): today the expectation is long,
and every grammar slice of [[1cee599ce218]] shrinks it. It stays as the
fixture guard for every later milestone.

## Work

- In `kitty-parser`, a test module that reads each example with
  `include_str!` (the six files named explicitly, so a new example is
  added on purpose), parses it, and lists each error through `ParseError`'s
  `Display` (a byte range and a message), with an `expect!` per file. Updated with `UPDATE_EXPECT=1`
  only when a grammar change is intended and reviewed.
- The three examples that panic today (`assembly`, `chair`, `sample`; the
  parameter-list assertion in `parser/src/grammar/function.rs`) cannot
  be listed until [[277624e4f5fe]] lands, so this slice records them with
  `#[should_panic]` and a `TODO(cc)` naming that plan, and [[277624e4f5fe]]
  turns them into expectations.

Interfaces: produces the test the M2 exit demo reads.
Verify first: `grep -rn "examples/" parser lexer` prints nothing, so no
test parses an example today.
Specs: none.
Not this slice: fixing anything the expectation shows.

## Seams under test

`kitty_parser::parse` over `examples/*.kitty`, the fixture seam.

## Done when

- `cargo test -p kitty-parser examples` runs six cases, three with expectations listing the current errors and three marked `should_panic` citing [[277624e4f5fe]]
- `timeout 600 just check` is green

## Outcome

Shipped: `parser/src/examples.rs`, a `#[cfg(test)]` module of `kitty-parser`
with one case per example. `3d-math` (37 errors), `3d-object` (99) and
`units` (23) carry an `expect!` listing each error through `ParseError`'s
`Display`; `assembly`, `chair` and `sample` are `#[should_panic]` with the
assertion message pinned (`function_param_list`'s
`assert!(p.at(TokenKind::ParenOpen))`, `parser/src/grammar/function.rs`),
so a different panic fails them, and a `TODO(cc)` names [[277624e4f5fe]].
`cargo test -p kitty-parser examples` runs the six.

No deviation from the plan. The expectations were recorded with
`UPDATE_EXPECT=1` once the red run had shown the six cases failing.

The tests live in-crate rather than under `parser/tests/`, so their path
carries `examples::` and the plan's filter selects them by name. The
expectation bodies are written at the layout `expect-test` 1.5.1 itself
writes (content four columns in from the `expect!` line, the closing
bracket at that line's column), so the first grammar slice that shrinks
one reindents nothing.

Review findings, no change made:

- The pinned panic message is `assert!`'s stringified expression, and the
  same text sits on `function_arg_list` at line 70 of the same file, so
  the pin cannot tell the two apart; all three examples panic at line 36
  (checked with `--nocapture`). Accepted: [[277624e4f5fe]] deletes these
  cases.
- `specs/grammar.md`'s Contract says the parser must not panic (review
  only) and its Gap line says three examples abort; the Gap could now cite
  `assembly_panics_in_the_parser`. Left as the plan says (Specs: none),
  the operator's call.
- [[1cee599ce218]]'s exit demo wants each remaining error listed with the
  owning plan's prefix beside it. The renderer here is `ParseError`'s
  `Display` alone, with no slot for that; the slice that first leaves an
  error to a design plan designs it.
- [[1cee599ce218]]'s exit demo names `units`, `3d-math` and `sample` as
  the three to parse clean; today the three that parse at all are
  `3d-math`, `3d-object` and `units`, and `sample` panics. Not false (an
  end-of-M2 target), but for the operator's eye.

## Log

- 2026-09-17: shipped after two review rounds (Standards clean in round
  two; round two's one Spec finding was a false sentence in this Outcome,
  verified against `parser/src/grammar/function.rs` and removed without
  a further round).
