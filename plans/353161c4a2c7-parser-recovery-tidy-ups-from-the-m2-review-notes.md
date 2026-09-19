---
title: Parser recovery tidy-ups from the M2 review notes
status: todo
priority: medium
parent: 1cee599ce218
derived_from: 1cee599ce218
---
Three small recovery and test defects the M2 slices marked and left,
fixed together because each is a few lines in `kitty-parser` and none
changes the grammar. A follow-up of [[1cee599ce218]].

## Work

- **A body after a malformed `where` clause.** The bound loop's recovery
  set does not hold `=>`, so `fn f() where => 1` reads the body as more
  bounds (the `TODO(cc)` in `parser/src/grammar/function.rs`, left by
  [[378fe0c39238]]). Add `=>` to the set where a function's `where`
  clause is parsed, so the clause ends and the body is a body. The
  `TODO(cc)` goes.
- **A missing `let` body's expected list.** When the body of a `let` is
  missing at the end of the input, the error lists the operator kinds
  the value's Pratt loop tested, and `in` (the `TODO(cc)` in
  `parser/src/grammar/expression.rs`, left by [[94bcf8245325]]). The
  list should be what may start the body. Fix it if the fix is local (a
  clear of the expected kinds once the value has ended, if `Parser` has
  or cheaply gains one); if it needs a change to how expected kinds are
  recorded across the parser, stop, leave the `TODO(cc)`, and say so in
  the Outcome.
- **The or-pattern tests.** `pattern_literal_number_or` and the
  type-pattern one beside it in `parser/src/grammar/pattern.rs` write
  `|`, which does not lex, are labelled happy path, and pin a tree that
  stops at the first pattern; the rule takes the `or` keyword and is
  untested ([[0248a546fe20]]'s finding). Rewrite them to the `or`
  spelling as built, so `PatternOr` has a snapshot. Which spelling the
  language keeps is an open question carried to [[5890c35571be]]; the
  test pins the grammar as built and the spec already says so.

Specs: `specs/grammar.md`: Functions or Generics and where clauses, for
the `where` recovery; Let, if the expected list changes; Patterns, the
or-pattern line cites its test in place of `review only`.
Not this slice: any change to what parses without error.

## Seams under test

`kitty_parser::parse` snapshots beside each rule.

## Done when

- A snapshot shows `fn f() where => 1` with one error and the `1` inside
  the function's body
- A snapshot shows `2 or 3 or 4` as a `PatternOr`
- The `let` expected-list fix has a snapshot, or the Outcome says why it
  was left
- `timeout 600 just check` is green

## Outcome

## Log
