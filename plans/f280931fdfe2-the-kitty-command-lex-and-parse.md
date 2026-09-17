---
title: "The kitty command: lex and parse"
status: done
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

Shipped: the `kitty-cli` crate (`cli/`, binary `kitty`), `clap` derive
with `lex` and `parse`, output through one locked `BufWriter` over
stdout; `kitty_meta::render`, an `ariadne` report as a string,
snapshot-tested with two labels and with an empty range at the end of
the source; `kitty_lexer::Tokens` as a public `Display` with `iter`;
`ParseError::range` and `ParseError::message`, `Display` written over
them with its output unchanged; `SourceId`'s `Display` shows an absolute
path as given (it printed `//tmp/x`); the crate README; `cli/` in
CLAUDE.md's and DESIGN.md's structure lists; "kitty command" in the
glossary; a Contract line in `specs/lexing.md` for the one-token-per-line
form; CLAUDE.md's Tracing section says the ban is on library code and a
binary's stdout goes through one locked `Write` handle.

Licence: the crate inherits the workspace's `MIT OR Apache-2.0`; `clap`
4.5.61, `clap_lex` 1.0.1 and `thiserror` 2.0.20 are `MIT OR Apache-2.0`,
`ariadne` 0.5.1 is MIT.

Deviations from the plan, and why:

- `render` takes a `SourceId` and a headline message beside the (range,
  message) labels. `ariadne`'s header is `Error: {message}` and prints
  it empty otherwise (`ariadne-0.5.1/src/write.rs`); a `&str` name that
  `render` turned into a `SourceId` itself mangled absolute paths. The
  command passes `syntax error` as the headline and the parse error's
  message as the one label.
- `ParseError` gained `range()` and `message()`: the command renders
  errors through `render`, not `Display`, so it needs the message
  without the position, and [[45a1b9e958a2]] wants (range, message)
  pairs too.
- The tree is printed through `SyntaxTreeBuf`'s `Debug`, not `Parse`'s,
  which appends the errors through `Display` and would print them twice.
- `kitty lex` exits 1 when the stream holds an `Error` token, the
  lexing counterpart of "exits non-zero when there are errors".
  Infrastructure failures (unreadable, not UTF-8, output closed) exit 2;
  a closed pipe (`kitty parse f | head`) is silent.
- "Specs: none exist yet" was stale: `specs/lexing.md` gained the line
  above.

Decision in flight, for the operator: `Cargo.lock` is committed
(`.gitignore` no longer lists it; CLAUDE.md's Conventions say so).
`clap` 4.6 and `clap_lex` 1.1 need Rust 1.85 and the workspace's
`rust-version` is 1.83. A `<4.6` bound on `clap` did not hold the claim
(`clap_lex` comes through `clap_builder`), and cargo's
`resolver.incompatible-rust-versions = "fallback"` config is ignored by
cargo 1.83 itself (`warning: ignoring resolver config table without
-Zmsrv-policy`). The lock holds the versions the gate tested on every
cargo, and `cargo install --locked` needs one. The lock also pins
`ariadne` 0.5.1: the report header changed between 0.5.0 and 0.5.1
(`[ name:1:5 ]`, padded), so a floating patch version would move the
snapshot. If the operator would rather raise `rust-version`, that is a
decision (the rust skill's toolchain rule).

Also from review: `render` asserts in debug builds that every label
lies on character boundaries inside the source, since `ariadne` slices
the line by byte column and drops a label past the end silently; the
glossary gains "Diagnostic" and its rendered "report", the one term the
crates, DESIGN.md and the README now share.

Review findings not acted on:

- `run` in `cli/src/main.rs` is untested: the plan's seam under test is
  `render`; the command is the I/O boundary CLAUDE.md exempts. A test
  through a `Vec<u8>` would be cheap if the exit-code contract grows.
- `cli/Cargo.toml` sets `readme = "README.md"` where the other members
  inherit: inheriting would point the crate at the root README, not its
  own, and the plan asks for a crate README.
- A closed pipe exits 2, silent. A reviewer put 0 as the convention for
  a filter under `set -e`; left as the README documents until a script
  needs it.
- `ParseError::message` builds a `String` through `format!` once per
  found token; a `fmt::Write` sink would save the allocation. Premature.
- `specs/grammar.md` does not name `ParseError::range` and `message`:
  accessors, not behaviour; its `Display` contract still holds.
- `parser/src/error.rs` imports std after external, pre-existing.

## Log

- 2026-09-17: shipped after three review rounds (round one: the MSRV
  mechanism and the `render` name; round two: wording, the glossary term,
  the boundary assert; round three clean).
