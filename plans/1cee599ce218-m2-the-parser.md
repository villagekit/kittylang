---
title: "M2: the parser"
status: doing
parent: 436aea0e22af
---

## Goal

Tokens to a concrete syntax tree for the whole surface syntax the
grilling of 2026-09-12 settled, resilient to malformed input, never
panicking, every `examples/*.kitty` program parsing without error except
at the sites the open design plans own, and the lexing and grammar specs
written first and kept true by every slice.

## Scope

Specs first, then the lexer, then the grammar, then the examples.

- **`specs/lexing.md` and `specs/grammar.md`**, written from the code as
  it is and the decisions as they stand, the not-yet-implemented forms
  marked as such, before any code moves. The parts of the surface this
  record settles as spec text, without a decision item: `True` and
  `False` are ordinary `Boolean` variants and no keyword
  ([[f2708b12e004]] makes `false` a value identifier); string literals
  are double-quoted only; `=>` is mandatory on every function body, then
  an inline expression or an indented block (already held); `let with <expr>` followed
  by an indented list of names destructures over several lines, and `let
  ... in` stays for a single line, a newline being the implicit `in`.
- **The lexer**: the package regex, which today truncates `@std/math` to
  `@std/m`; an `@` token for attributes; the `Boolean` token kind
  removed so `True` and `False` lex as type identifiers; the `#= =#`
  multi-line comment, which the lexer lacks; the `from` token accepted as a
  function name so `fn from` parses; the `with` keyword for `let with`; the glossary's metadata
  comment term deleted ([[03212e993893]] found nothing to remove in the
  lexer); the `:1` version suffix on an import accepted and kept in the
  tree, its meaning deferred to [[a89ddd383a16]].
- **The parser never panics**: every assertion and unwrap reachable
  from `parse`, starting with the parameter-list assertion in
  `function.rs` and the `Self` pattern assertion in `pattern.rs`,
  becomes a parse error with recovery or is made unreachable; then a
  fuzz target over lex and parse, seeded with `examples/`, guards the
  rule.
- **Grammar**: return type annotations on functions and the mandatory
  `=>`; keyword arguments with `=` in `( )`, `{ }` and indented blocks,
  brace-delimited constructor patterns with shorthand and `=` fields
  ([[95cd2585f916]], [[881178303bc8]]) and `...spread`; a value segment
  after a type path in an expression (`N.default()`, `GridBeam.Z`), which
  [[f2708b12e004]] promises and the grammar lacks; attributes
  ([[03212e993893]]) reusing the call argument list; `let with`.
- **The examples**, rewritten to the settled surface in one slice,
  [[75d0d7eea26c]]; no other slice edits `examples/`: `seatHeight` to `seat_height`, `GridPanel`
  imported, `fn parts: Parts` to `fn parts(self): Parts`, `regular(self)`
  to `regular()`, `fn plugins()` a method of `impl Assembly for Chair`
  returning `List(SmartFasteners())`, `false` and `true` to `False` and
  `True`, `'top'` to `"top"`, `[0, seat_width]` to `(0, seat_width)`
  ([[ec7345d92813]]), `fn default()` in `3d-object.kitty` given `=>`,
  every `x: 0` argument and `Self { x: ... }` to `=`, the bare `in`
  line after `let with` in `chair.kitty` removed. Left as they are
  until their design plans close: `impl Object3d for Assembly`
  ([[3738718cde03]]), `struct Parts(...)`, `PartsItem.None` and the `case Name(Type)`
  payloads ([[dd325e81ad2c]]), the positional indented blocks under
  `Vector3` and `Parts` with the `then`-less `if` among them
  ([[e6a33eab19d4]]), and any bracket spanning lines
  ([[d0658cb19697]]).

Specs: every section of `lexing.md` and `grammar.md` this record
touches; `docs/context.md` gains attribute, spread and the `let with`
form.

## Seams under test

- `kitty_lexer::lex`, `expect-test` snapshots of the token stream per
  lexing rule, the indenter's cases.
- `kitty_parser::parse`, `expect-test` snapshots of the tree per grammar
  rule: the happy path and the recovery on malformed input, spans on
  every node.
- The examples test from [[5c5f4b9256b3]], the end-to-end fixture.
- The fuzz target, run bounded in the slice that adds it.

## Exit demo

`timeout 600 just check` is green, and the examples test's expectation
shows zero parse errors for `units.kitty`, `3d-math.kitty` and
`sample.kitty`, and for the other three only errors at sites the open
design plans own, each listed in the expectation with the plan's prefix
beside it.

## Out of scope

The five design plans above and what they decide; analysis (M3), which
its own grilling designs after [[0ccbdb209358]]; the playground and the
command, in [[5c5f4b9256b3]].

## Outcome

## Log
