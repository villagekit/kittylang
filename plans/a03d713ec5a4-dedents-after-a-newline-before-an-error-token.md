---
title: Dedents after a newline before an error token
status: done
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

Shipped in `kitty-lexer` and `specs/lexing.md`; the Gap line is gone.

- `indenter.rs`: the branch taken after a `Newline` that the next line
  starts at column zero now fires on any following token, `Ok` or
  `Err`, so the level of the new line decides the dedents, not the kind
  of its first token. The end-of-source path (`None`) is unchanged. The
  `Error` token stays in place; only the `Dedent` tokens around it move,
  from the end of the source to the newline before it.
- Two lexer tests, in the `check_tokens` form the other indent tests
  use: `lex_indent_error_at_column_zero_dedents` was red before the fix
  (the dedents arrived at the end of the source);
  `lex_indent_error_after_a_dedent` was green before it, since the
  whitespace branch never looked at the token after the run. It is kept
  as the plan asked, pinning that half of the rule.
- The spec's two rules say "an `Error` token included" and cite the new
  tests.
- Verified first, with `kitty lex` over a two-level block followed by a
  `$` at column zero: `Error@28..30` arrived with the two `Dedent`s at
  the end of the source, as the Gap line said. After the fix they sit
  at `28..28`, before it.
- `timeout 120 just fuzz`: 216k runs in 60 seconds, no artifact.
- The plan's `## Done when` says the dedent before the `Error` sits "at
  the newline"; in the one-level case it sits at the end of the
  whitespace run (`14..14`), where the spec's rule for a lesser level
  has always put it. The tests follow the spec.
- From review: the spec's `Error` token row cites the column-zero test
  instead of "review only"; `lex_indent_error_after_a_dedent` is cited
  on the "less" sub-rule it exercises, not the parent rule; a
  Non-guarantees bullet says the indenter sees no newline inside an
  `Error` token that runs across a line end (an unterminated `#=`); the
  indenter's comment names the end-of-source case.
- Dropped from review: folding `else if ahead.is_some()` into a bare
  `else`, since the `None` arm is the one place the end-of-source
  dedents are popped and the reviewer wanted no change either.
- Last open slice of [[1cee599ce218]]'s three follow-ups; the record's
  finishing is the next iteration's, not this one's.

## Log
