---
title: The fuzz target
status: todo
parent: 1cee599ce218
derived_from: 1cee599ce218
blocked_by: 277624e4f5fe
priority: medium
---

A fuzz target over `lex` then `parse`, seeded with `examples/`, so the
never-panic rule of [[277624e4f5fe]] stays held as the grammar grows
([[1cee599ce218]]). Not part of `just check`; run on demand.

## Work

- A `fuzz/` directory with a `cargo-fuzz` target (MIT or Apache-2.0)
  calling `kitty_parser::parse` on arbitrary bytes that are valid UTF-8,
  outside the workspace members as `cargo-fuzz` expects, its corpus
  seeded from `examples/`.
- A `just fuzz` recipe that installs `cargo-fuzz` if absent, runs with
  `cargo +nightly fuzz run parse` for a bounded time (`-max_total_time`),
  and says so in the justfile comment; the nightly toolchain is a
  documented requirement of the recipe, not of `just check`. The crate
  README says how to run it.
- Any panic the first run finds is fixed in this slice, with a recovery
  snapshot.
- `CLAUDE.md`: a `just fuzz` row in the Commands table and `fuzz/` in
  the structure list; `DESIGN.md`'s structure list likewise.

Interfaces: none produced.
Verify first: `cargo fuzz --version` fails on a fresh machine; the
recipe must install it.
Specs: none.
Not this slice: fuzzing the lexer's indenter separately; a corpus beyond
the examples.

## Seams under test

The fuzz target; `kitty_parser::parse` recovery snapshots for anything
it finds.

## Done when

- `timeout 120 just fuzz` runs and finds no panic in that time
- `timeout 600 just check` is green

## Outcome

## Log
