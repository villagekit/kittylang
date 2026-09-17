---
title: Keyword arguments with equals, and spread
status: done
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
  - target: 466ffcebbfc1
    strength: soft
    note: both touch function.rs; sequencing avoids a conflict
priority: medium
derived_from: 1cee599ce218
---

Keyword arguments are spelled `name = value` in every call form
([[95cd2585f916]], [[881178303bc8]]): `( )`, `{ }` and an indented
block, and `...expr` spreads a value into a construction
([[1cee599ce218]]).

## Work

- `parser/src/grammar/function.rs`: the labelled argument rule takes
  `=` instead of `:`; `expression_apply` also fires on `{` (a keyword
  argument list in braces) and on an indent after a callee (an indented
  block of `name = value` lines, one per line). A positional line in an
  indented block is a parse error citing [[e6a33eab19d4]] until that
  plan decides otherwise; mixed blocks are an error.
- `parser/src/grammar/pattern.rs`: a brace-delimited constructor
  pattern, `Self { x, y = a }`, which does not exist today (patterns
  dispatch on `(` only, and `3d-math.kitty`'s `let Self { x, y, z } =
  self` cascades into a dozen errors): `pattern_single` accepts a type
  path followed by `{`, the field list allowing a bare name (shorthand)
  or `name = pattern`; and the existing labelled field in the `( )` form
  takes `=` instead of `:` too, so [[95cd2585f916]]'s patterns read as
  shown.
- `...expr` as an argument in any of the three forms, a spread node in
  `kitty-syntax` with a `kitty-cst` view.
- Snapshots for each form, for a spread, for a positional line in an
  indented block, for `{` on a struct name.

Interfaces: produces the argument list that [[328c0a9906fb]] reuses for
attributes.
Verify first: `function_labelled_arg` in `parser/src/grammar/function.rs`
and `pattern_type_arg_labelled` in `parser/src/grammar/pattern.rs` both
expect `Colon`; `grep -rn Ellipses parser/src` prints nothing.
Specs: `grammar.md`, calls and arguments.
Not this slice: positional lines in indented blocks, the design plan's.

## Seams under test

`kitty_parser::parse` snapshots per form; the examples test.

## Done when

- `Self { x = 1 }`, `f(x = 1)` and a callee followed by an indented `x = 1` line each parse to the same argument-list shape in the snapshot
- A snapshot shows `Self { x = 1 }` parsing to a labelled argument carrying `Equal`, and `Self { x: 1 }` recovering with one error
- Snapshots show the patterns `Self { x, y, z }` and `Self { x = a }`, the second binding `a` to field `x`
- `timeout 600 just check` is green

## Outcome

Shipped: keyword arguments spelled `name = value` in every call form.
`function_arg_list` in `parser/src/grammar/function.rs` parses `( )`,
`{ }` and an indented block after a callee, all to the same
`FunctionArgList` shape; `...expr` is a `FunctionArgSpread` node in
`kitty-syntax` with a `kitty-cst` view. The pratt loop in
`parser/src/grammar/expression.rs` fires a call on `(`, `{` or an
indent; `match` parses its scrutinee with `expression_before_block`, a
`BlockArgs` restriction carried through operators and the tails of
`if` and `let`, so the arms block is never taken as arguments. In
`parser/src/grammar/pattern.rs`, a constructor pattern takes `{ }` with
shorthand (`Self { x, y = a }`), and the `( )` named field takes `=`.
`Parser` gains `error_misplaced`, `at_recovery` and `peek_in`;
`EXPRESSION_FIRST` names what an expression can start with.

Snapshots: `keyword_args_in_parens`, `keyword_args_in_braces`,
`keyword_args_in_a_block`, `spread_among_keyword_args`,
`spread_in_a_block`, `keyword_arg_with_a_colon_recovers`,
`keyword_arg_without_a_value_is_missing`,
`positional_arg_in_braces_is_an_error`,
`positional_line_in_a_block_is_an_error`,
`stray_token_in_a_block_is_one_error`,
`let_pattern_with_a_renamed_field`,
`match_scrutinee_ends_before_the_arms`,
`match_scrutinee_tail_ends_before_the_arms`,
`match_with_no_arms_ends_at_the_input` in
`parser/src/grammar/expression.rs`; `pattern_type_brace_shorthand`,
`pattern_type_brace_rename`,
`pattern_type_brace_field_with_a_colon_recovers` in
`parser/src/grammar/pattern.rs`. The examples test shrank from about
160 error lines to 20: what remains is the `:` spelling in the
examples (one error per line, at the colon), positional lines starting
with `self` in `3d-object.kitty`, `let` without `in`, and
`Vector3[Length].default()` (the value-segments plan's).

Deviations. The plan reads a `{ }` pattern field as `name = pattern`;
the spec's production takes an identifier, as the `( )` field always
has, and no decision covers nested patterns in fields, so the parser
takes an identifier and the spec's non-guarantee stands. The plan
names the positional line's error as citing the design plan; the
error is the ordinary `Unexpected` and the citation is the rule's
comment. A positional argument where only keyword arguments may stand
is parsed whole with one error, not token by token, so a Village Kit
parts block reports one error per line and keeps its nested blocks
balanced; a line that starts with a value identifier is read as a
keyword argument instead, so `beam(1)` on its own line reports more
than one error. A keyword argument's value is now required; the old
rule silently accepted `f(a:)`.

Out of the plan's scope but changed here: the `match` arm loop is
bounded with `at_recovery`, since the block call form made its
pre-existing spin reachable from well-formed input before the
restriction was threaded through `if` and `let` tails, and both
reviewers asked for it. The `where` bound loop, the other half of the
operator's open flag, is untouched.

Review. Round one, both axes: `let Self { x = a } = self` misparsed,
because the `{ }` field decided shorthand by the caller's recovery set
and `let` recovers at `=` (fixed: a bare name ends at `,`, `}` or the
input); the restriction did not reach `if` and `let` tails, so
`match if a then b else c` followed by arms spun (fixed, and the loop
bounded); the Recovery bullet overclaimed one error per positional
line (reworded to what the rule does); a stray token in a keyword-only
position gave two errors (now one `Error` node); no `{ }` positional
test (added); the nested-pattern sentence duplicated a non-guarantee
(removed); "keyword argument" was missing from the glossary (added,
with "positional argument"). Rejected: renaming `FunctionArgLabelled`
to match the term, since `GenericArgLabelled` and
`PatternTypeArgLabelled` share the word and a rename is its own
change; `self` as a label, pre-existing and now noted in the spec as
the grammar as built; merging the two `_braces` list rules, which
differ in what an item is. Deferred: a lambda body as a `match`
scrutinee is not restricted (no use for it; spec says review only);
`let with` will need `expression_before_block` when it lands; the CST
views are still untested (pre-existing). Round two, Spec: the "one per
line" wording was not what the parser holds, since newlines are trivia
(now a Gap citing the newline design plan); the `self` label sentence
lacked its `review only`; a construction has no shorthand, said now
beside the pattern's; the glossary's positional entry no longer says
`( )` only. Round two, Standards: the `if` condition and the `let`
value keep block calls even under `match`, so `match if a` with no
`then` followed by arms takes the arms as arguments; malformed input
only, and a well-formed condition wants the block call, so left as is.
Rejected: dropping `error_misplaced`'s end-of-input arm as unreachable,
since the rule must never panic and a `Missing` error is the honest
record when no token is there. `expression_before_block` made private.

Follow-ups minted: none. The examples' rewrite to `=` is plan
75d0d7eea26c; positional lines in blocks are plan e6a33eab19d4.

## Log
