---
title: The examples test
status: todo
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

## Log
