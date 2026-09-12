---
title: "The kitty command: lex and parse"
status: todo
parent: 5c5f4b9256b3
priority: high
derived_from: 5c5f4b9256b3
---

A `kitty` binary that prints what the compiler sees: `kitty lex <file>`
prints the token stream, `kitty parse <file>` prints the syntax tree and
then the parse errors rendered through `ariadne`. The first instrument of
[[5c5f4b9256b3]]: from here on a syntax question is answered by running a
file through the command, not by reading the grammar.

## Work

- A new crate `cli/` (`kitty-cli`, binary `kitty`), a workspace member,
  `clap` with derive for the two subcommands (MIT or Apache-2.0; confirm
  in the slice). It reads the file, calls `kitty_lexer::lex` or
  `kitty_parser::parse`, writes to a locked `stdout` handle through one `Write` so the
  output can be captured, exits non-zero when there are errors.
  CLAUDE.md's Tracing section says no `println!` in committed code and
  makes no exception; this slice edits it to say a binary's output goes
  through a `Write` handle and the rule is about library code, flagged
  here rather than read in. Its infrastructure
  errors (the file unreadable, not UTF-8) are one `thiserror` enum per
  CLAUDE.md's error convention. It is the pipeline's first I/O
  boundary: thin, exempt from TDD.
- `CLAUDE.md`'s and `DESIGN.md`'s structure lists gain `cli/`; the crate
  gets a README saying what the two subcommands print.
- `kitty-lexer`: the one-token-per-line printer that exists only under
  `cfg(test)` (`lexer/src/lib.rs`, `Tokens`) becomes a public `Display`
  so the command and the playground print tokens the way the snapshots
  do.
- `kitty-parser`: `Parse<N>` already prints tree then errors through
  `Debug`; the command prints the tree through that and the errors
  through the renderer below, not the `Display` of `ParseError`.
- `kitty-meta`: a `render` function taking a source name, the source
  text and a list of (range, message) pairs, returning the `ariadne`
  report as a string. `kitty-meta` depends on `ariadne` already and
  renders nothing yet; DESIGN.md's structure places diagnostic rendering
  here. `ParseError` carries a `text-size` range, not a `Span`, so the
  renderer takes ranges; a `Span`-taking form waits for a caller.

Interfaces: produces `kitty_meta::render` and the public token `Display`
for [[45a1b9e958a2]].
Verify first: `grep -n "cfg(test)" lexer/src/lib.rs` shows the printer is
test-only; `grep -rn ariadne meta/src` shows only the `ariadne::Span` impl on
`Span`, nothing that renders.
Specs: none exist yet; `docs/context.md` gains "the kitty command".
Not this slice: the playground ([[45a1b9e958a2]]); rendering for analysis
diagnostics, which do not exist.

## Seams under test

`kitty_meta::render`, one `expect-test` snapshot of a report with two
labels. The command itself is I/O and untested; the lexer and parser
printers are covered by the snapshots that already use them.

## Done when

- `cargo run -p kitty-cli -- lex examples/units.kitty` prints one token per line
- `cargo run -p kitty-cli -- parse examples/units.kitty` prints the tree, then each parse error as an `ariadne` report with the source line and a label, and exits 1
- `cargo run -p kitty-cli -- parse` on a file with no errors exits 0
- `timeout 600 just check` is green

## Outcome

## Log
