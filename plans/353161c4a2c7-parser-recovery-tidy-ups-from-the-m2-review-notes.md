---
title: Parser recovery tidy-ups from the M2 review notes
status: done
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

Shipped, all three, in `kitty-parser` alone; nothing that parsed
without error changed its tree.

- A function's `where` clause is parsed with `=>` in its recovery set,
  so `fn f() where => 1` puts the `1` in the function's body. One error
  needed a second small change: `generic_where_clause` expects a
  `Dedent` only when the `Indent` was there, since a block that never
  opened has no dedent to close it. That reaches the struct, enum, trait
  and impl bodies too, where a one-line clause used to take the body's
  dedent as its own; it no longer does
  (`struct_where_clause_with_no_block_keeps_the_body_dedent`). The
  `TODO(cc)` is gone. Snapshots:
  `function_body_after_a_where_clause_with_no_bounds` and
  `where_clause_on_one_line_ends_at_the_body` in
  `parser/src/grammar/declaration.rs`. The latter's input, `fn f()
  where T: T => 1`, was `where_clause_with_no_bounds_ends_at_the_input`
  until this change, and no longer reached the end of the input inside
  the bound loop; that test keeps its name and the invariant it pins
  with the input `fn f() where T: T`, which does.
- The `let` fix was local: `Parser` gained `clear_expected`, and
  `expression_let` calls it when no `in` was consumed, so a missing
  body's error lists what may start a body
  (`let_expression_missing_body_expects_what_starts_a_body`,
  `parser/src/grammar/expression.rs`; the `3d-math.kitty` snapshot in
  `parser/src/lib.rs` shortened the same way). No change to how expected
  kinds are recorded elsewhere, so the same residue stands where the
  plan did not point: a `let with` whose block is missing
  (`let_with_empty_block`) and the end-of-file messages in
  `parser/src/examples.rs`. The cause is `expression_pratt` testing its
  operators with `at`/`at_set`, which record, where it is choosing a
  path; `Parser::peek_in` exists for that, records nothing, and would
  end all of them at once. That is a change across the parser, so a follow-up, not
  this slice.
- The or-pattern tests read `2 or 3 or 4` and `This or That` and pin a
  `PatternOr`. The chain nests to the right as the rule is built (the
  loop recurses into `pattern`); the spec's Patterns section says so
  beside the production, which stays flat. The spelling question stays
  with [[5890c35571be]].

Specs: `specs/grammar.md`, the Let, Patterns and Generics and where
clauses sections.

## Log
