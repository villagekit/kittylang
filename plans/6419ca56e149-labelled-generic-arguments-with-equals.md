---
title: Labelled generic arguments with equals
status: todo
priority: medium
parent: 1cee599ce218
derived_from: 1cee599ce218
---
A labelled generic argument is spelled `Name = Type`, as
[[95cd2585f916]] decided, in a type's argument list and in a type
pattern's, closing the last Gap line under Types in `specs/grammar.md`.
A follow-up of [[1cee599ce218]], found by [[0248a546fe20]] and left
without a slice.

## Work

- `generic_arg_list` in `parser/src/grammar/type.rs`: the labelled
  argument takes `=` between the label and the type. The `:` spelling
  latches into labelled arguments too and records one `expected ‘=’`
  error at the colon, as the keyword argument rule does for `label :`
  ([[4423cd0aa2bb]], [[328c0a9906fb]]).
- The same for a type pattern's labelled arguments, if
  `parser/src/grammar/pattern.rs` spells them `:` today.
- The label's class stays as built. Whether it is a type identifier
  (`Vector3[N = Number]`) or a value identifier (`Map[key = String]`)
  is unsettled, [[95cd2585f916]] shows both; the spec keeps saying so
  and the question is carried to [[5890c35571be]]. Do not decide it
  here.
- `kitty-cst` views over `GenericArgLabelled` keep working; the
  examples test's expectations do not grow.

Verify first: `generic_arg_labelled` in `type.rs` pins `Foo[Bar:
Number]`; no `examples/*.kitty` file writes a labelled generic argument
(check with a search for `[` followed by a name and `:` or `=`).
Specs: `specs/grammar.md`, Types: the production and its citations; the
Gap line goes.
Not this slice: the label's class; generic parameter defaults.

## Seams under test

`kitty_parser::parse` snapshots in `parser/src/grammar/type.rs` (and
`pattern.rs` if it changes): the happy path with `=`, positional then
labelled, and the recovery on `:`.

## Done when

- `Foo[Bar = Number]` and `Foo[Number, Bar = String]` parse without
  error, in named snapshot tests
- `Foo[Bar: Number]` records one error, at the colon, and keeps the
  `GenericArgLabelled` node
- No Gap line about labelled generic arguments remains in
  `specs/grammar.md`
- `timeout 600 just check` is green

## Outcome

## Log
