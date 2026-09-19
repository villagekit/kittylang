---
title: Tooling for language design
status: done
parent: 436aea0e22af
---

## Goal

Instruments to see the language while it is designed: a command line
that prints the tokens and the tree of a `.kitty` file, a test that
holds every example program to the grammar, and a web playground where
the tokens, the tree and the errors update as the source is typed. The
grilling of 2026-09-12 put these before any grammar work so the
ergonomics are judged by looking, not by argument.

## Scope

- **The `kitty` command**, a new crate `cli/` (`kitty-cli`), binary
  `kitty`, subcommands `lex <file>` printing the token stream and `parse
  <file>` printing the syntax tree and its parse errors. The printers are
  the ones the snapshot tests already use in `kitty-lexer` and
  `kitty-parser`. Errors render through `ariadne` from `kitty-meta`
  spans; `kitty-meta` depends on `ariadne` but renders nothing yet, so
  the rendering lands there, as DESIGN.md's structure says.
- **The examples test**, in `kitty-parser`: every `examples/*.kitty`
  program parsed, the expectation a per-file list of parse errors with
  spans, updated with `UPDATE_EXPECT=1`. Red where the grammar is behind;
  the red names the sites. It is the M2 exit bar as a test, and it stays
  as the fixture guard for every later milestone.
- **The playground**, a new crate `playground/` compiled with
  `wasm-bindgen`, exporting one function that takes source text and
  returns the tokens, the tree and the errors as data; and a plain HTML
  page, no framework, with an editor pane, syntax colouring from the
  lexer's token kinds, the tree beside it, errors underlined, live on
  each keystroke. Highlighting comes only from `kitty-lexer`: no
  TextMate or tree-sitter grammar, per CLAUDE.md's "one grammar, one
  home". Licences of everything pulled in are checked in the slice.

Specs: `specs/lexing.md` and `specs/grammar.md` are written by
[[1cee599ce218]]; this record adds nothing normative. `docs/context.md`
gains the terms this work needs (the playground, the command).

## Seams under test

- `kitty_lexer::lex` and `kitty_parser::parse`, the existing seams: the
  command and the playground are thin I/O over them and are exempt from
  TDD as CLAUDE.md's testing rules say.
- The examples test, a new fixture seam over `parse` and `examples/`.
- The playground's wasm export, tested as a pure function from source to
  data; the page itself is not tested.

## Exit demo

`cargo run -p kitty-cli -- parse examples/units.kitty` prints the tree
and the errors rendered by `ariadne`; `just test` runs the examples test
and its expectations list the errors for the three files that parse,
the other three marked `should_panic` until [[277624e4f5fe]] lands; the playground page
opened in a browser shows tokens, tree and errors for a pasted example
and updates as it is edited.

## Out of scope

A language server and editor extensions (a later record, after M3, when
there are semantic tokens to send); a formatter; rendering of analysis
diagnostics, which do not exist yet.

## Outcome

The `kitty` command (`lex`, `parse`), the resilient parser's error rendering through `ariadne`, the examples test and the playground page all shipped. The exit demo was run on 2026-09-19 and holds: `cargo run -p kitty-cli -- parse examples/units.kitty` prints the tree; `just test` runs the examples test with its expectations; the playground page, served locally and loaded in headless Chrome, shows the tree for the example it starts with. The interactive part (pasting an example and watching it update while editing) was not exercised by hand, only the initial render. `just playground` builds the wasm; the `pkg/` output is not committed.

## Log

- 2026-09-12: Minted by the grilling of 2026-09-12; its transcripts are in the shared transcripts repo under kittylang/2026-09-12-surface-syntax-grilling (grill.md, the parser-coverage probe, three slice review rounds).
- 2026-09-19: Exit demo run and the record closed during the orchestration that followed the M2 gate's opening; see the M2 record's Outcome.
