---
title: The fuzz target
status: done
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

Shipped. `fuzz/` is a `cargo-fuzz` crate excluded from the workspace
(root `Cargo.toml`, `exclude = ["fuzz"]`), its one target `parse` feeding
every input that is valid UTF-8 to `kitty_parser::parse` and dropping
the result. `just fuzz [seconds]` installs `cargo-fuzz` if absent, copies
`examples/*.kitty` into `fuzz/corpus/parse/` and runs
`cargo +nightly fuzz run parse -- -max_total_time=<seconds> -timeout=10`,
sixty seconds by default; the per-input timeout is so a loop that
consumes nothing is reported as a finding in seconds rather than as an
out-of-memory abort minutes later. `fuzz/README.md` says how to run it,
replay and shrink a finding, and turn it into a snapshot test.
`CLAUDE.md` and `DESIGN.md` list the directory and the recipe.

The first run found two defects, both fixed here, test-first:

- The `where` bound loop (`generic_where_clause`, `parser/src/grammar/
  type.rs`) ended only at a dedent, the hang [[277624e4f5fe]] flagged;
  in a release build it grew the event list until libFuzzer's memory
  limit reported it as out-of-memory. It now ends where a bound would
  consume nothing, the guard the `match` arm loop already had, and
  expects the dedent. Snapshot: `where_clause_with_no_bounds_ends_at_
  the_input`, `parser/src/grammar/declaration.rs`. The Gap line in
  `specs/grammar.md` is gone; the always-return rule cites both loops.
- The indenter (`lexer/src/indenter.rs`) split a whitespace run at
  `start + level`, a width used as a byte count, so a tab (width four)
  opening a block inside a two-space one put the `Indent` range's start
  past its end, which `TextRange::new` (text-size 1.1.1, `src/range.rs:48`)
  refuses with a panic; the lexing spec had recorded it as a Gap. The split is now the byte length
  of the longest prefix whose level is at most the previous level, one
  `char_width` per character, so it lands between characters and inside
  the run. Tests: `lex_indent_tab_inside_a_space_block` and
  `lex_indent_spaces_after_a_tab`, `lexer/src/lib.rs`.
  The spec's indenter rule cites it, its Terms say a form feed ends the
  count, and the Gap keeps only the `Error`-after-newline half.

After the fixes, a sixty-second run and a five-minute run (1.05 million
executions, corpus 4236 inputs) found nothing.

Deviations: none from the Work list. "Specs: none" did not hold: the
two fixes close a Gap line in each spec, and the never-panic and
always-return lines name the fuzz target as the on-demand aid. The
`where` fix is the liveness bug
[[277624e4f5fe]] left for "a plan of its own or the fuzz slice"; it is
here because the run's Done-when cannot hold with it in place. The
plan's parenthetical "(MIT or Apache-2.0)" is not literally true of
`libfuzzer-sys` 0.4.13, whose licence is `(MIT OR Apache-2.0) AND NCSA`,
with the bundled libFuzzer `Apache-2.0 WITH LLVM-exception`; both are
permissive, so the licensing convention holds. `fuzz/Cargo.lock` is
committed, as the root lockfile is. The 120-second bound holds once
`cargo-fuzz` is installed and the target built; a fresh machine's first
run pays for both, and the recipe comment, `CLAUDE.md` and the README
say so.

Review: two rounds on fresh sub-agents. Round one applied: the spec
lines that named the fuzz target as what holds the never-panic rule are
`review only` again, with the target as the on-demand aid (a fuzz run is
neither a named test nor in the gate); one `char_width` shared by the
level measure and the split, so the form-feed case is defined rather
than unreachable; `license` on the fuzz crate; one workspace exclusion,
not two; the README says the gate never formats, lints or builds
`fuzz/`. Round two applied: `cargo install cargo-fuzz --locked`; a
second lexer case where the split lands after a tab; `split_at_level` a
free function beside its helpers. Deferred: the Structure list's "One
cargo workspace" heading over non-crate entries (`examples/` and
`sketches/` set the precedent); "fuzz target" is not a glossary term,
since the glossary holds language and compiler vocabulary, not tooling;
the `where` clause's recovery set lacks `=>`, so a body after a
malformed clause is read as bounds, a `TODO(cc)` in
`parser/src/grammar/function.rs` (recovery quality, not this plan).

Visual gate: none in this repo.

## Log
