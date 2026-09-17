---
title: The parser never panics
status: done
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
priority: medium
derived_from: 1cee599ce218
---

The parser never panics, on any input ([[1cee599ce218]]): every assertion and
unwrap reachable from `parse` becomes a parse error with recovery or is
made unreachable by construction, starting with the two that fire on
the examples today.

## Work

- `parser/src/grammar/function.rs`: the parameter-list rule asserts on
  `(`; a missing list becomes a parse error naming the expected token
  and a `Missing` node, and parsing continues. `fn parts: Parts` and a
  lambda inside an argument then parse with errors instead of aborting.
- `parser/src/grammar/pattern.rs`: `pattern_type` asserts on a type
  identifier but is dispatched on a set that includes `Self`; a `Self`
  pattern parses as a type path. The `TODO(cc)` there goes.
- The rest of the inventory, each made an error or unreachable:
  `Node::cast(..).unwrap()` in `parser/src/lib.rs`; the two
  `last_token_range().unwrap()` calls in `parser/src/parser.rs`,
  reachable on empty input; the two asserts in `parser/src/sink.rs`;
  `Parser::parse`'s `expect` on an empty event list and the `None`
  `Marker::abandon` leaves mid-stream (research synthesis of 2026-09-09,
  section 8); the `DropBomb` on a leaked `Marker`; and the `assert!(p.at(
  ..))` preconditions at the head of the grammar rules, which stay only
  where the dispatching caller guarantees them and become `debug_assert!`
  with a comment saying which caller, or an error otherwise.
- The three `should_panic` cases in the examples test become
  expectations.

Interfaces: none produced; consumes the examples test from [[942ff7a43d93]].
Verify first: `cargo test -p kitty-parser examples` shows three
`should_panic` cases; `grep -rn "assert!\|unwrap()\|expect(" parser/src`
is the inventory to walk.
Specs: `grammar.md`, recovery.
Not this slice: the fuzz target ([[378fe0c39238]]); any grammar the examples
lack.

## Seams under test

`kitty_parser::parse` recovery snapshots for a function without a
parameter list, a lambda in an argument, a `Self` pattern and empty
input; the examples test.

## Done when

- `cargo test -p kitty-parser examples` has six expectations and no `should_panic`
- A `kitty-parser` snapshot shows empty input parsing to an empty module with no errors
- `timeout 600 just check` is green

## Outcome

Shipped: every panic reachable from `kitty_parser::parse` is a parse
error with recovery or unreachable by construction. The six examples
parse with recorded expectations, no `should_panic`; empty and
trivia-only input parse to an empty `Module` with no errors.

- Grammar preconditions: where the dispatching caller guarantees the
  first token, the `assert!` became a `debug_assert!` on `p.peek()`,
  with a comment naming the caller; `peek` records nothing, so debug and
  release builds report the same `expected` lists. The unguaranteed ones
  became errors: the function parameter list (the `FunctionParamList`
  node is made holding only the recovery rule's node, its recovery set
  extended by `:`, `where`, `=>`), a `Self` pattern (dispatch on
  `TYPE_PATH_FIRST`), and the labelled argument after a comma in call
  args, generic args and type patterns.
- Found on the walk, not named in the plan: a unary operator whose
  operand fails leaked its `Marker` (`-` at end of input panicked in the
  `DropBomb`); the node is now made with a `Missing` operand, as the
  binary rule does. A declaration's missing function name now recovers
  at `[` or `(` rather than eating the `(`, so a nameless `fn (x) => x`
  where a declaration stands parses its parameter list.
- Core: `Parser::error` is one match on the peeked token, so the
  `last_token_range().unwrap()` calls are gone (`Source::end_offset` is
  zero for an empty input); `Parser::parse` drops `None` events
  (an abandoned marker mid-stream) instead of `expect`ing none; `bump`
  is bounded at the end of the input; the sink's unsafe pointer walk is
  a `Peekable` walk with the same trivia placement, its shape asserts
  now `debug_assert!`s since eventree 0.7.0's `SyntaxBuilder::finish`
  (`src/tree.rs:264-266`) asserts a finished root in release too; the
  `Marker` bomb is a `DebugDropBomb`, so a leaked marker (a grammar bug)
  fails a debug build and degrades to an abandon in release.
- Left by construction: `parse`'s `expect` on `Module::cast`, since the
  module rule completes a `Module` root on every input (rust-analyzer's
  `Parse<SourceFile>::tree` does the same). `ParseError::Unexpected.found`
  stays `Option<TokenKind>` though `error` now always fills it; changing
  the public type is not this slice.
- Specs: `grammar.md` Contract, Recovery, Functions, Lambda and Patterns
  cite the new tests; the abort half of the Gap line is gone. Glossary:
  the Event entry no longer lists an error among events (it never was
  one).

Deviations: the diff is about 550 lines outside the recorded example
snapshots (a further 500 lines of expectations), over the 500-line
guide; the slice is one plan and the snapshots are recorded output.

Review: three rounds on fresh sub-agents, the last clean. Rejected:
the sink's shape asserts as `assert!` "to avoid UB in `root()`"
(eventree asserts in release, cited above). Applied: the param-list
node always made, with a snapshot for its `Error` branch; the unary
rule returning its node; side-effect-free preconditions; the typed cast
kept in `check_grammar` so the `kitty-cst` casts stay covered; rustdoc
on `Parse`'s fields; glossary vocabulary in place of "gap" and
"placeholder"; the hang flag below widened. Deferred: `Marker::abandon`
is still dead code with no test; the 25 dispatch preambles repeat but
are explicit and cheap; the examples expectations will churn with every
grammar change until the examples are rewritten to the settled surface
([[75d0d7eea26c]]).

Flag for the operator: the parser hangs, it does not panic, on
`fn f() => match x` and `fn f() where T: T => 1`, because the `match`
arm and `where` bound loops end only at a dedent, and an arm or a bound
consumes nothing when the next token is in its recovery set or the
input has ended (the specs worker found the end-of-input case; the
review found the recovery-set case; `specs/grammar.md` keeps the Gap
line). In a release build the spin grows the event list until the
process aborts, which a user reads as a panic. It is distinct from this
plan's inventory and no plan owns it; the fuzz target
([[378fe0c39238]]) would find it as a timeout. The fix is a progress
guard on both loops (stop when an iteration consumed nothing), not an
`at_end` check, which the review tried and found insufficient. It
belongs in a plan of its own or in the fuzz slice.

Visual gate: none in this repo.

## Log
