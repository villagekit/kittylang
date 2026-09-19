---
title: Dedents after a newline before an error token
status: todo
priority: medium
parent: 1cee599ce218
derived_from: 1cee599ce218
---
The indenter yields its `Dedent` tokens after a `Newline` whatever token
follows, an `Error` token included, closing the last Gap line in
`specs/lexing.md`. A follow-up of [[1cee599ce218]], found by
[[0248a546fe20]]; [[378fe0c39238]] closed the other half of that Gap.

## Work

- `lexer/src/indenter.rs`: the branch taken after a `Newline` looks at
  the next token only when it is `Ok`; when the next token is an `Error`
  (at column zero, or after the line's whitespace) no `Dedent` is
  queued, and the dedents arrive at the next newline that dedents or at
  the end of the source. Make the level of the new line decide the
  dedents, not the kind of its first token. An `Error` token stays an
  `Error` token, in place; only the block tokens around it change.
- Red first: a lexer snapshot of a two-level block whose next line
  starts at column zero with a byte that does not lex, and one where the
  line dedents one level and then holds the `Error`.
- Run `timeout 120 just fuzz` once after the fix, since the change is
  in the indenter the fuzz target found its last defect in.

Verify first: the Gap line under Indentation in `specs/lexing.md` is
still true, by lexing such a source with `cargo run -q -p kitty-cli --
lex`.
Specs: `specs/lexing.md`, Indentation: the rule cites the new tests and
the Gap line goes.
Not this slice: the indenter's non-guarantees (a comment-only line's
indentation counting, a dedent to a level not on the stack), which the
spec lists as such and no example needs.

## Seams under test

`kitty_lexer::lex` snapshots in `lexer/src/lib.rs`.

## Done when

- The two new lexer snapshots show the `Dedent` tokens at the newline
  before the `Error` token
- No Gap line remains in `specs/lexing.md`
- `timeout 120 just fuzz` finds nothing
- `timeout 600 just check` is green

## Outcome

## Log
